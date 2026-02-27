# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This R project implements a PTAL-inspired (Public Transport Accessibility Level) multi-amenity accessibility index for Greater Melbourne, Australia. It calculates accessibility to employment, open space, supermarkets, and various community facilities (education, healthcare, childcare, sports) from residential mesh blocks using time-dependent public transport routing combined with walking network analysis.

The system uses a time-expanded graph with BFS and temporal dominance pruning (C++ via Rcpp) to model realistic public transport journeys, accounting for departure times, walking transfers, and walking connections at both ends of the journey.

## Temporal Logic (Critical)

The system uses **two opposing time frames** that interact throughout the pipeline. Understanding when each is used is essential.

### The Two Clocks

1. **Time Remaining** (counts down from 9:45 AM): Used in the place registry and vertex definitions. `minutes_until_time_limit = time_limit - departure_time`. A service departing at 9:10 has 35 minutes remaining. **Higher value = earlier departure = more time budget.**

2. **Time Elapsed** (counts up from 8:59 AM): Used inside the Dijkstra traversal. `current_elapsed_time = max_time - current_time_remaining`. If you've used 11 minutes of your 46-minute budget, elapsed = 11. **Higher value = later in journey = less budget.**

### How They Connect: `time_margin`

The `time_margin` field in the place registry bridges the two clocks. It is computed in the "remaining" frame but compared against the "elapsed" frame in the Dijkstra:

```
time_margin = time_left_after_walking - mins_left_at_dep_time
```

- `time_left_after_walking = 46 - walking_time`: time budget remaining after walking from source stop to the boarding stop
- `mins_left_at_dep_time`: time remaining when the service departs from the boarding stop
- `time_margin`: the waiting buffer — how many extra minutes you have at the boarding stop (assuming you arrived at the source stop at minute 0)

**In the Dijkstra**, when arriving at a source stop with `current_elapsed_time` minutes already used, an edge is valid if:

```
time_margin >= current_elapsed_time
```

This works because: if the margin assumed arrival at minute 0, and you actually arrived at minute N, your effective margin shrinks by N. You can still make the connection only if the original margin exceeds the delay.

### Worked Example

- You are at Stop A at minute 0 (8:59 AM). Walking to Stop B takes 5 min → `time_left_after_walking = 41`
- A bus departs Stop B at 9:10 AM → `mins_left_at_dep_time = 35` (35 min before 9:45)
- `time_margin = 41 - 35 = 6` (you arrive 6 minutes before the bus leaves)
- Later, the Dijkstra reaches Stop A at elapsed time 4 → `time_margin (6) >= elapsed (4)` → valid, you still arrive 2 min early
- If the Dijkstra reaches Stop A at elapsed time 8 → `time_margin (6) >= elapsed (8)` → invalid, you'd miss the bus by 2 min

## Core Architecture

### Execution Flow (`main.R`)

When `source('main.R')` is run, the following pipeline executes in order:

1. **Reset Storage** (`reset_storage.R`): Clears `stop_isochrones/` directory to prevent mixing GTFS schedules
2. **Initialise GTFS** (`initialise_gtfs.R`): Loads GTFS data via `synfaxgtfs`, filters to departures within the 8:59–9:45 window, excludes parent rail stops (`vic` prefix). Globals: `gtfs_prefilter`, `stops`, `unique_stops`, `stop_id_to_name`
3. **Transit-to-UFI Mapping** (`get_transit_ufi_dict.R`): Loads cached mapping of transit stop_id → nearest road infrastructure UFI (or computes via `st_nearest_feature` if no cache)
4. **Walking Distances** (`calculate_walking_distances.R`): Loads pre-computed walking isochrones from `rdata_output/walking_distances_new.Rdata` (the actual computation is in `real_walking_distances.R`, run separately)
5. **Place Registry** (`generate_place_registry.R`): Builds the transit connection graph — for each transit stop, finds walkable transfer stops, identifies boardable services, and records all reachable alighting stops with temporal metadata. Parallel via `future`/`furrr`
6. **Mesh Block to UFI** (`mb_centroids_ufi.R`): Links each Greater Melbourne mesh block centroid to its nearest road infrastructure UFI via `st_nearest_feature`
7. **Walking Access Dictionary** (`link_walk_stops.R`): Loads all walking isochrone CSVs, joins with `master_mb_ufi` to filter to mesh-block UFIs, then joins with `transit_ufi_dict` to create a large lookup: from each transit stop, which mesh blocks (by MB_CODE21) are walkable and at what walking_time
8. **Employment per Mesh Block** (`employment_mb.R`): Spatially intersects mesh blocks with destination zones, allocates employment proportionally by overlap area
9. **Starting Indices** (`find_starting_indices.R`): For each mesh block, finds the best transit stop to walk to as a journey starting point. Joins MB centroids → walking network → transit stops → place registry vertices, filtered for temporal feasibility, picking the option with the most time remaining per mesh block
10. **Amenity Dictionary** (`create_master_amenity_mb_dict.R`): Sources all amenity scripts, joins employment + open space + supermarkets + VicMap FOI data into a single data.table keyed by MB_CODE21
11. **Dijkstra Routing** (`dijkstra/dijkstra_routing.R`): Builds the time-expanded graph in R, flattens to CSR format, runs Dijkstra (C++ via Rcpp, `cpp/bfs_routing.cpp`) from each starting vertex, then does per-vertex walking joins and amenity summation in R
12. **Package Results** (`final_mesh_block_result.R`): Links routing results back to origin mesh blocks, computes percentile ranks per amenity, sums into `total_score`, writes GeoPackage

### Walking Network (`gtfs_files/real_walking_distances.R`)

Pre-computes walking isochrones using VicMap road network data. **Run separately before the main pipeline.**

- Loads `TR_ROAD_ALL.shp` (edges with FROM_UFI/TO_UFI and distance) and `TR_ROAD_INFRASTRUCTURE.shp` (vertices/nodes with UFI)
- Creates bidirectional edges (each road segment generates both directions)
- Processes each SA2 region independently: buffers the SA2 by max walk distance, builds a local road subgraph from overlapping SA2s, runs Dijkstra from every road UFI within the SA2
- Walking speed: 84 m/min (~5 km/h). `walking_time = distance ÷ 84` (integer division)
- Max walking distance: 20 min × 84 m/min = 1,680m buffer
- Uses Dijkstra on road network with distance tracking and shortest-path updates (simple queue, not min-heap)
- FORK-based parallelism via `parLapplyLB` (shared memory, Linux only)
- Outputs: one CSV per SA2 in `walking_isochrones_sa2/`, columns: `start_UFI`, `UFI`, `distance`, `walking_time`
- Final combined result also saved as `rdata_output/walking_distances_new.Rdata` and joined with transit stops

### Place Registry (`gtfs_files/generate_place_registry.R`)

Builds the transit connection graph. For each transit stop (`source_stop_id`):

1. **Find walkable stops**: Look up the source stop's nearest UFI in the walking distances table to find all stops reachable on foot. Keep only the shortest walk per destination stop.
2. **Identify boardable services**: Join walkable stops with GTFS departures. Apply temporal filter: `time_left_after_walking >= minutes_until_time_limit` — you must arrive at the boarding stop before the service departs.
3. **Select boarding point per trip**: For each trip, pick the boarding stop with `which.max(time_left_after_walking)` — the closest walkable stop (least walking time consumed, most budget remaining).
4. **Find alighting stops**: Join with all downstream stops on the same trip (where `stop_sequence >= boarding stop_sequence`).
5. **Compute time_margin**: `time_left_after_walking - mins_left_at_dep_time` — the temporal slack at the boarding stop.

**Output columns**: `source_stop_id`, `trip_id`, `stop_id` (alighting), `stop_sequence`, `minutes_until_time_limit` (at alighting), `walking_time`, `time_left_after_walking`, `mins_left_at_dep_time`, `min_stop_seq`, `time_margin`, `boarding_stop_name`, `alighting_stop_name`

### Time-Expanded Graph Routing (`gtfs_files/dijkstra/dijkstra_routing.R`)

The routing is split between R (data wrangling, graph construction) and C++ via Rcpp (graph traversal).

#### Step 1: Build Vertices (R)

Vertices are `(stop_id, time_remaining)` pairs, drawn from three sources:
- Source pairs: `(source_stop_id, mins_left_at_dep_time)` from place registry
- Destination pairs: `(stop_id, minutes_until_time_limit)` from place registry
- Fake start pairs: `(stop_id, 46 - walking_time)` from `find_starting_indices` — these represent the time budget a person has after walking from their mesh block to the nearest transit stop

Each vertex gets a numeric index. Vertex metadata is stored in pre-allocated arrays for O(1) access: `vertex_stop_ids`, `vertex_time_remaining`, `vertex_stop_numeric`.

#### Step 2: Build Edges + CSR Format (R)

Edges from `place_registry` are sorted by `source_stop_numeric` and flattened into **CSR (Compressed Sparse Row)** format — three parallel vectors that C++ can consume efficiently:
- `adj_offsets`: integer vector of length `(n_stops + 1)`. Edges for stop `i` are at positions `adj_offsets[i]` to `adj_offsets[i+1] - 1`
- `adj_dest`: integer vector — all `dest_vertex_index` values concatenated
- `adj_margin`: numeric vector — all `time_margin` values concatenated

Built via `setorder(edges_dt, source_stop_numeric)` → `tabulate()` → `cumsum()`. All indices are converted to 0-based before passing to C++.

#### Step 3: BFS Traversal (C++ — `cpp/bfs_routing.cpp`)

The core graph traversal is implemented in C++ via `Rcpp::sourceCpp()`.

**Algorithm** (`bfs_pruned` function):
1. Initialise a FIFO queue (pre-allocated `std::vector<int>` with head/tail pointers) — O(1) push/pop
2. Push the starting vertex; track queued vertices with `std::vector<bool>` to prevent duplicate entries
3. Process vertices in FIFO order:
   - Skip if already visited
   - Compute `current_elapsed_time = max_time - current_time_remaining`
   - **Temporal dominance**: if `best_time_per_stop[stop] >= current_time_remaining`, skip — a previous visit to this stop had more time budget
   - For each outgoing edge (CSR iteration): if `time_margin >= current_elapsed_time`, push unqueued destination vertex
4. Return deduplicated results: one `(stop_numeric, time_remaining)` pair per visited stop

**Why FIFO over priority queue**: Tested `std::priority_queue` (proper Dijkstra) — it was slower (8 min vs 5 min) due to O(log n) heap operations and duplicate entries. The FIFO queue with temporal dominance pruning is correct and faster for this graph structure.

**0-indexing boundary**: R subtracts 1 from all index inputs before calling C++. C++ is fully 0-indexed internally. Return values add 1 for R (`stop_indices.push_back(i + 1)`).

**Function signature**:
```cpp
List bfs_pruned(
    int start_vertex_index,       // 0-indexed
    int n_vertices, int n_stops,
    NumericVector vertex_time_remaining,
    IntegerVector vertex_stop_numeric,   // 0-indexed
    double max_time,
    IntegerVector adj_offsets,    // 0-indexed CSR offsets
    IntegerVector adj_dest,       // 0-indexed CSR destinations
    NumericVector adj_margin      // CSR time margins
)
// Returns: List with stop_numeric (1-indexed) and time_remaining vectors
```

#### Step 4: Per-Vertex Post-Processing (R)

For each starting vertex (~30k total, processed sequentially via `lapply`):
1. Call `run_bfs()` → get ~870 `(stop_id, time_remaining)` pairs
2. Join with `walking_access_dict` → ~36k rows (each stop connects to ~41 mesh blocks)
3. Filter `walking_time <= time_remaining` (only walkable destinations within budget)
4. Dedup per MB: `setorder(MB_CODE21, -time_remaining_incl_walking)` + `unique(by='MB_CODE21')` — keeps best arrival per mesh block
5. Join `master_amenity_dt` and `colSums()` amenity columns — one row per starting vertex

**Performance**: ~5 minutes for full pipeline (down from ~12 minutes with pure R BFS). The C++ BFS returns deduplicated stops, avoiding the vertex-level explosion that previously caused memory issues.

### Starting Indices (`gtfs_files/find_starting_indices.R`)

Determines how each mesh block enters the transit network:

1. Load all walking isochrones and join with `transit_ufi_dict` to find which transit stops are walkable from each road UFI
2. Join with `master_mb_ufi` to link mesh block centroids to the walking network
3. Join with place registry vertices `(stop_id, minutes_until_time_limit)` to ensure the stop has valid connections
4. Filter: `walking_time + minutes_until_time_limit <= 46` (temporal feasibility)
5. Per mesh block, pick the option with `which.max(time)` — maximising the time budget remaining

The result feeds into the Dijkstra as "fake start pairs" with `time = 46 - walking_time`.

**Known issue (double-back problem)**: A resident walks from their mesh block centroid to the nearest transit stop, but the Dijkstra then allows walking transfers between stops — potentially retracing the same path. Impact is likely minimal due to small mesh block sizes.

### Amenity Calculation (`gtfs_files/create_master_amenity_mb_dict.R`)

Aggregates amenity counts per mesh block by sourcing scripts in `gtfs_files/amenities/`:

- **Employment** (`employment_mb.R`): Intersects mesh blocks with destination zones, allocates jobs proportionally by overlap area. Filters overlaps < 0.001 km².
- **Open Space** (`link_open_space.R`): Queries OSM for leisure=park/nature_reserve/recreation_ground/garden polygons. Intersects with mesh blocks, returns count and total area per MB.
- **Supermarkets** (`link_supermarkets.R`): Queries OSM for shop=supermarket/greengrocer/grocery. Deduplicates polygon+point pairs (converts overlapping polygons to centroids). Returns count per MB.
- **VicMap FOI** (`manipulate_vicmap_foi.R`): Loads FOI_INDEX_CENTROID.shp, filters to: sport facility, education centre (excl. education complex), child care, tertiary institution, hospital, health facility. Pivots to wide format with count per facility type per MB.

Final output: single data.table keyed by MB_CODE21 with columns: `jobs`, `total_open_space_area`, `n_supermarkets`, `n_sport_facility`, `n_education_centre`, `n_child_care`, `n_tertiary_institution`, `n_hospital`, `n_health_facility`. NAs filled with 0.

### Final Output (`gtfs_files/final_mesh_block_result.R`)

1. Links Dijkstra results back to origin mesh blocks via the starting indices name key (`stop_id_time`)
2. Saves raw results to `qs/trimmed_results.qs` (includes mesh_block_list and travel_times per origin MB)
3. Computes `percent_rank()` for each amenity column (0–1 scale)
4. Sums percentile ranks into `total_score` (multi-amenity accessibility index)
5. Saves scores to `qs/trimmed_scores.qs`
6. Joins with mesh block geometry and writes `sf_output/final_result.gpkg`

## Data Dependencies

### Required Input Files

**Transit Data** (loaded via `synfaxgtfs` package):
- GTFS schedule data for Melbourne (rail, tram, bus)

**Spatial Boundaries** (in `~/Documents/r_projects/shapefiles/`):
- `MB_2021_AUST_SHP_GDA2020/`: ABS Mesh Blocks 2021 (residential origins)
- `DZN_2021_AUST_GDA2020_SHP/`: ABS Destination Zones 2021 (employment zones)
- `SA2_2021_AUST_SHP_GDA2020/`: Statistical Areas Level 2 (walking network chunking)

**Road Network** (VicMap Transport, in `sf_input/`):
- `tr_road_infrastructure/.../TR_ROAD_INFRASTRUCTURE.shp`: Road intersection points (UFI nodes)
- `tr_road/.../TR_ROAD_ALL.shp`: Road segments (edges with FROM_UFI/TO_UFI)

**Employment Data**:
- `sf_output/dzns_sf.shp`: Destination zones with employment counts (pre-processed)

**Facilities Data** (VicMap):
- `sf_input/Order_N13IFM/.../FOI_INDEX_CENTROID.shp`: Features of Interest (community facilities)

**Amenity Data**:
- OSM data fetched live via `osmdata` package (open space, supermarkets)

### Generated Outputs

- `walking_isochrones_sa2/`: Walking distances from road UFIs (one CSV per SA2)
- `rdata_output/`: Cached intermediate objects (`walking_distances_new.Rdata`, `transit_ufi_dict.Rdata`)
- `qs/`: Serialised intermediate results (`trimmed_results.qs`, `trimmed_scores.qs`)
- `sf_output/final_result.gpkg`: Final mesh block accessibility scores with geometry
- `stop_isochrones/`: Cleared on each run, used for intermediate storage

## Key Parameters

**GTFS Settings** (`main.R`):
```r
mode_numbers = 2:4        # Rail, tram, bus
day = 'wednesday'          # Weekday schedule
start_time = "8:59:00"    # Journey start time
time_limit = "9:45:00"    # Must arrive by this time (46 min window)
xfer_penalty = "00:05:00" # Defined but not currently used in routing logic
```

**Spatial Settings**:
```r
walking_speed = 84        # meters/minute (~5 km/h), used in real_walking_distances.R
max_walking_time = 20     # minutes (walking network pre-computation buffer)
max_time = 46             # Total journey time budget (minutes), hardcoded in several files
```

**Performance Settings**:
```r
doParallel = T            # Enable parallel processing for place registry
num_cores = RAM/6GB       # Cores capped at (available_RAM / 6GB) or (total_cores - 1)
```

## Development Commands

**Run Full Pipeline**:
```r
source('main.R')  # calls Rcpp::sourceCpp('cpp/bfs_routing.cpp') internally
```

**Compile C++ Only** (for iterating on `cpp/bfs_routing.cpp`):
```r
Rcpp::sourceCpp('cpp/bfs_routing.cpp')
```

**Clear Cached Isochrones**:
```r
source('reset_storage.R')
reset_storage()
```

**Generate Walking Network** (run separately before main pipeline):
```r
source('gtfs_files/real_walking_distances.R')
combined_results <- run_parallel_walking_isochrones()
```

## Global Variables

The pipeline uses `<<-` extensively to share state between functions:

| Variable | Set By | Used By | Description |
|---|---|---|---|
| `gtfs_prefilter` | `initialise_gtfs` | `generate_place_registry` | Filtered GTFS stop_times |
| `stops` | `initialise_gtfs` | `main.R` | All transit stops (sf) |
| `unique_stops` | `initialise_gtfs` | `main.R`, `generate_place_registry` | Character vector of active stop IDs |
| `stop_id_to_name` | `initialise_gtfs` | `generate_place_registry`, `dijkstra_routing` | Named vector: stop_id → stop_name |
| `transit_ufi_dict` | `main.R` | `generate_place_registry`, `link_walk_stops`, `find_starting_indices` | stop_id → nearest road UFI |
| `walking_distances` | `main.R` | `generate_place_registry` | Walking isochrones data.table |
| `place_registry` | `main.R` | `find_starting_indices`, `dijkstra_routing` | Full transit connection graph |
| `master_mb_ufi` | `main.R` | `link_walk_stops`, `find_starting_indices` | MB_CODE21 → nearest UFI |
| `walking_access_dict` | `main.R` | `dijkstra_routing` | Transit stop → walkable mesh blocks |
| `master_amenity_dt` | `main.R` | `dijkstra_routing` | MB_CODE21 → amenity counts |
| `test` | `main.R` | `dijkstra_routing`, `final_mesh_block_result` | Starting indices (MB → transit stop + walk time) |
| `vertex_to_index` | `dijkstra_routing` | `final_mesh_block_result` | Vertex name → numeric index |
| `isochrone_params` | `main.R` | `initialise_gtfs`, `generate_place_registry` | Time window parameters |
| `max_time` | `main.R` | `find_starting_indices`, `dijkstra_routing` | 46 (total minutes) |

## Known Issues & Limitations

1. **Double-back Problem** (`find_starting_indices.R`):
   Residents are modelled as starting at the road UFI closest to their mesh block centroid. They walk to the nearest transit stop, but the routing algorithm then allows walking transfers between stops — potentially retracing the original walk path. Impact likely minimal due to small mesh block sizes.

2. **Road Infrastructure Alignment**:
   Uses road intersection points (TR_ROAD_INFRASTRUCTURE) rather than building entrances. May introduce minor spatial inaccuracies in walking distance calculations.

3. **Hardcoded `max_time = 46`**:
   The 46-minute budget appears as a literal in `generate_place_registry.R` (lines 48, 59), `dijkstra_routing.R` (line 17), and `find_starting_indices.R` (via `max_time` global). Changing the time window requires updating multiple locations.

4. **`xfer_penalty` Unused**:
   The transfer penalty parameter is defined in `isochrone_params` but never referenced in the routing logic. Transfers are penalised implicitly through walking time + waiting time only.

5. **Duplicate Walking Isochrone Reads**:
   `link_walk_stops.R` and `find_starting_indices.R` both independently read all CSVs from `walking_isochrones_sa2/` via `map_dfr(fread(...))`. Noted with a TODO in the code.

6. **Memory Requirements**:
   The walking access dictionary (`link_walk_stops`) produces a large cartesian join (transit stops × walkable mesh blocks). The place registry is also large. Pipeline benefits from 32GB+ RAM.

7. **Walking Dijkstra Uses Simple Queue**:
   The walking network Dijkstra (`real_walking_distances.R`) uses an expanding vector as a queue rather than a min-heap priority queue. Functionally correct but not optimal time complexity. (The transit Dijkstra in `cpp/bfs_routing.cpp` does use a proper priority queue.)

## Dependencies

**Core Packages**: `synfaxgtfs`, `tidyverse`, `data.table`, `sf`, `s2`
**C++ Integration**: `Rcpp` (graph traversal in `cpp/bfs_routing.cpp`, compiled via `Rcpp::sourceCpp()`)
**Parallelism**: `future`, `furrr`, `parallel`, `doParallel`
**Spatial Data**: `osmdata`, `leaflet`
**Utilities**: `lubridate`, `tictoc`, `janitor`, `benchmarkme`, `qs`, `profvis`

The project relies on the custom `synfaxgtfs` package for GTFS data loading and filtering.

### C++ Files (`cpp/`)

| File | Purpose |
|---|---|
| `bfs_routing.cpp` | Core BFS traversal with FIFO queue and temporal dominance pruning. Takes CSR graph + vertex metadata, returns deduplicated (stop, time_remaining) pairs. Priority queue tested and reverted — slower due to O(log n) overhead. |
| `filter_edges.cpp` | Learning exercise — returns indices where value >= threshold. Not used in pipeline. |
| `hello.cpp` | Learning exercise — sums an integer vector. Not used in pipeline. |

## Project Type

This is an RStudio project. Open via `dwelling_job_access.Rproj` in RStudio for proper working directory and environment setup. CRS used throughout: GDA2020 / MGA zone 55 (EPSG:7855) for projected operations; WGS84 for visualisation.
