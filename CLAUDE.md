# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This R project implements a PTAL-inspired (Public Transport Accessibility Level) multi-amenity accessibility index for Greater Melbourne, Australia. It calculates accessibility to employment, open space, supermarkets, and various community facilities (education, healthcare, childcare, sports) from residential mesh blocks using time-dependent public transport routing combined with walking network analysis.

The system uses a time-expanded graph with Dijkstra's algorithm to model realistic public transport journeys, accounting for departure times, transfers, and walking connections at both ends of the journey.

## Core Architecture

### Execution Flow (`main.R`)

When `source('main.R')` is run, the following pipeline executes:

1. **Reset Storage** (`reset_storage.R`): Clears `stop_isochrones/` directory to prevent mixing GTFS schedules
2. **Initialize GTFS** (`initialise_gtfs.R`): Loads and filters GTFS transit data for specified modes, day, and time window
3. **Calculate Walking Distances** (`calculate_walking_distances.R`): Loads pre-computed walking isochrones from road network
4. **Link Transit to Road Network** (`get_transit_ufi_dict.R`): Maps transit stops to nearest road infrastructure UFIs
5. **Generate Place Registry** (`generate_place_registry.R`): Creates time-expanded routing graph of all possible transit connections
6. **Map Mesh Blocks to Road Network** (`mb_centroids_ufi.R`): Links mesh block centroids to nearest road UFI for journey origins
7. **Link Walking to Stops** (`link_walk_stops.R`): Creates dictionary of which mesh blocks are reachable from each transit stop
8. **Calculate Employment per Mesh Block** (`employment_mb.R`): Distributes employment data from destination zones to mesh blocks
9. **Find Starting Indices** (`find_starting_indices.R`): Determines valid journey start points (mesh block → nearest transit stop)
10. **Create Amenity Dictionary** (`create_master_amenity_mb_dict.R`): Aggregates all amenity data per mesh block
11. **Run Dijkstra Routing** (`dijkstra_routing.R`): Computes all reachable mesh blocks from each starting point using time-dependent routing
12. **Package Final Results** (`final_mesh_block_result.R`): Converts routing results to percentile-ranked accessibility scores

## Key Components

### Walking Network (`gtfs_files/real_walking_distances.R`)

Pre-computes walking isochrones using VicMap road network data:
- Processes all Melbourne SA2 regions in parallel using FORK-based shared memory
- Uses Dijkstra's algorithm on road infrastructure UFI (Unique Feature Identifier) graph
- Walking speed: 84 m/min (5 km/h)
- Max walking distance: 20 minutes
- Outputs saved to `walking_isochrones_sa2/` as CSV files per SA2

**Key limitation**: Uses road infrastructure centroids which may not align perfectly with building access points.

### Time-Expanded Graph Routing (`gtfs_files/dijkstra/dijkstra_routing.R`)

Core innovation of the project:
- **Vertices**: (stop_id, time_remaining) pairs representing arrival at a stop with specific time budget remaining
- **Edges**: Transit connections between stops, filtered by departure time and transfer feasibility
- **Time margin**: Ensures passengers have sufficient time to walk between stops before service departs
- **Temporal dominance pruning**: Skips vertices if stop already visited with more remaining time
- **Starting points**: Mesh block centroid → nearest transit stop (with walking time deducted from initial budget)

Returns all mesh blocks reachable within time limit (currently 46 minutes from 8:59 AM start).

### Place Registry (`gtfs_files/generate_place_registry.R`)

Creates the routing graph by:
1. Finding which stops are walkable from each source stop (using pre-computed walking distances)
2. Identifying transit services departing from walkable stops within time window
3. Determining all stops reachable via each service
4. Calculating time margins to ensure temporal feasibility

Uses `future` package for parallel processing across multiple cores.

### Amenity Calculation (`gtfs_files/create_master_amenity_mb_dict.R`)

Aggregates multiple amenity types per mesh block:
- **Employment**: Proportional allocation from Destination Zones based on spatial intersection
- **Open Space**: OSM leisure features (parks, nature reserves, recreation grounds, gardens)
- **Supermarkets**: OSM shop tags (supermarket, greengrocer, grocery)
- **Community Facilities**: VicMap FOI data (sport, education, childcare, tertiary, hospital, health)

All amenities sourced from `gtfs_files/amenities/` scripts using spatial intersection with mesh blocks.

### Final Output (`gtfs_files/final_mesh_block_result.R`)

Produces `sf_output/final_result.gpkg` containing:
- Raw accessibility counts for each amenity type per mesh block
- Percentile-ranked scores (0-1 scale) for each amenity
- `total_score`: Sum of all percentile ranks (multi-amenity accessibility index)

**Note**: Output format still being refined for Quarto + Shiny presentation layer (in development).

## Data Dependencies

### Required Input Files

**Transit Data** (loaded via `synfaxgtfs` package):
- GTFS schedule data for Melbourne (rail, tram, bus)

**Spatial Boundaries**:
- `MB_2021_AUST_SHP_GDA2020/`: ABS Mesh Blocks 2021 (residential origins)
- `DZN_2021_AUST_GDA2020_SHP/`: ABS Destination Zones 2021 (employment zones)
- `SA2_2021_AUST_SHP_GDA2020/`: Statistical Areas Level 2 (walking network chunking)

**Road Network** (VicMap Transport):
- `sf_input/tr_road_infrastructure/TR_ROAD_INFRASTRUCTURE.shp`: Road intersection points (UFI nodes)
- `sf_input/tr_road/TR_ROAD_ALL.shp`: Road segments (edges with FROM_UFI/TO_UFI)

**Employment Data**:
- `sf_output/dzns_sf.shp`: Destination zones with employment counts (pre-processed)

**Facilities Data** (VicMap):
- `sf_input/Order_N13IFM/.../FOI_INDEX_CENTROID.shp`: Features of Interest (community facilities)

**Amenity Data**:
- OSM data fetched live via `osmdata` package (open space, supermarkets)

### Generated Outputs

- `walking_isochrones_sa2/`: Walking distances from road UFIs (one CSV per SA2)
- `rdata_output/`: Cached intermediate objects for faster re-runs
- `sf_output/final_result.gpkg`: Final mesh block accessibility scores

## Key Parameters

**GTFS Settings** (`main.R`):
```r
mode_numbers = 2:4        # Rail, tram, bus
day = 'tuesday'           # Weekday schedule
start_time = "8:59:00"    # Journey start time
time_limit = "9:45:00"    # Must arrive by this time (46 min window)
xfer_penalty = "00:05:00" # Transfer penalty (5 minutes)
```

**Spatial Settings**:
```r
walking_speed = 84        # meters/minute (5 km/h)
max_walking_time = 20     # minutes (for pre-computed walking network)
max_time = 46             # Total journey time budget (minutes)
```

**Performance Settings**:
```r
doParallel = T            # Enable parallel processing
num_cores = RAM/6GB       # Cores based on available memory
```

## Development Commands

**Run Full Pipeline**:
```r
source('main.R')  # 3-4+ hours with 32GB+ RAM
```

**Clear Cached Isochrones**:
```r
source('reset_storage.R')
reset_storage()
```

**Generate Walking Network** (run separately before main pipeline):
```r
source('gtfs_files/real_walking_distances.R')
combined_results <- run_parallel_walking_isochrones()  # ~1-2 hours
```

## Known Issues & Limitations

1. **Double-back Problem** (`find_starting_indices.R`):
   - Starting point logic simulates residents at mesh block centroid UFI walking to nearest transit stop
   - Routing algorithm then allows walking between transit stops, potentially "doubling back" along the original path
   - Impact likely minimal due to small mesh block sizes and sparse transit networks in low-PT areas
   - Documented but not yet resolved

2. **Road Infrastructure Alignment**:
   - Uses road infrastructure centroids (points) rather than actual building entrances
   - May introduce minor spatial inaccuracies in walking distance calculations

3. **Output Format In Development**:
   - Current percentile ranking and total score system still being refined
   - Shiny/Quarto presentation layer not yet implemented

4. **Memory Requirements**:
   - Full pipeline requires 32GB+ RAM
   - Place registry and walking access dictionary create large in-memory data.tables
   - Uses FORK-based parallelism for memory efficiency where possible

## Dependencies

**Core Packages**: `synfaxgtfs`, `tidyverse`, `data.table`, `sf`, `s2`
**Parallelism**: `future`, `furrr`, `parallel`, `doParallel`
**Spatial Data**: `osmdata`, `leaflet`
**Utilities**: `lubridate`, `tictoc`, `janitor`, `benchmarkme`

The project relies heavily on the custom `synfaxgtfs` package for GTFS data loading and filtering.

## Project Type

This is an RStudio project. Open via `dwelling_job_access.Rproj` in RStudio for proper working directory and environment setup.