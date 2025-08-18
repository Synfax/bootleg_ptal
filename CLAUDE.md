# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an R project that calculates transport service accessibility indices for Melbourne, Australia. The project measures job accessibility from residential mesh blocks using public transport isochrones (GTFS data) combined with walking distances. It produces employment accessibility statistics (mean, median, maximum) for each mesh block based on reachable jobs within specified travel time limits.

## Key Architecture Components

### Main Processing Pipeline (`main.R`)
The main script orchestrates a complex 6-step process:
1. Initialize GTFS data and parameters using `synfaxgtfs` package
2. Generate place registry (routing lookup tables) via parallel processing
3. Calculate isochrones for all transit stops using fork-based parallel processing
4. Buffer stops and calculate employment accessibility per isochrone
5. Map mesh blocks to nearby stops within walking distance (450m buffer)
6. Aggregate employment statistics for each mesh block

### Core Components

**GTFS Processing (`gtfs_files/`)**
- `initialise_gtfs.R`: Loads and filters GTFS transit data for Melbourne
- `generate_place_registry.R`: Creates routing lookup tables for transit connections
- `parallel_fork_processing.R`: Processes isochrones using fork-based parallelism with shared memory
- `calculate_walking_distances.R`: Pre-computes walking distances between transit stops

**Employment Calculations**
- `calculate_isochrone_employment.R`: For each stop's isochrone, calculates accessible employment by intersecting buffered isochrone areas with employment destination zones
- `calculate_mesh_block_employment.R`: For each mesh block, aggregates employment statistics from accessible stops

### Data Dependencies

**Required Input Files (configured in `main.R`):**
- Mesh block shapefiles (`MB_2021_AUST_SHP_GDA2020`)
- Destination zone shapefiles (`DZN_2021_AUST_GDA2020_SHP`) 
- Melbourne dwelling data (`melbourne_dwelling_data.gpkg`)
- Employment CSV (`sf_input/employment_dzn.csv`)

**Generated Outputs:**
- `stop_isochrones/`: CSV files for each stop's reachable destinations
- `sf_output/`: Shapefiles including final mesh block employment results
- `rdata_output/`: Cached R objects for performance

### Key Parameters

**GTFS Parameters:**
- Mode numbers: 2-4 (rail, tram, bus)
- Day: Wednesday
- Time window: 8:59 AM - 9:45 AM
- Transfer penalty: 5 minutes

**Spatial Parameters:**
- Walking buffer: 450 meters
- Walking speed: 84 m/min (5 km/h)

## Development Commands

This is an RStudio project. Use RStudio to open `dwelling_job_access.Rproj`.

**Running the Analysis:**
```r
source('main.R')  # Runs full pipeline (3-4+ hours)
```

**Documentation:**
```r
# Generate documentation (Quarto)
quarto::quarto_render('documentation.qmd')
```

**Key Settings in `main.R`:**
- `doParallel = T`: Enable parallel processing (required for reasonable performance)
- `num_cores`: Calculated based on RAM (6GB per core recommended)

## Performance Considerations

- Full pipeline requires 32GB+ RAM and takes 3-4+ hours
- Uses fork-based parallelism for shared memory efficiency
- Intermediate results are cached in `rdata_output/` to enable resuming
- Run `reset_storage.R` to clear isochrone cache between different GTFS schedules

## Spatial Analysis Details

The project implements a sophisticated two-stage accessibility calculation:
1. **Isochrone Generation**: Uses GTFS routing to find all stops reachable within time limits
2. **Employment Weighting**: Intersects buffered isochrone areas with employment zones, assuming uniform job distribution within each zone

Output provides three employment accessibility measures per mesh block:
- Mean accessible employment (average across nearby stops)
- Median accessible employment (reduces outlier impact)  
- Maximum accessible employment (assumes optimal stop choice)

## Dependencies

Key R packages: `synfaxgtfs`, `tidyverse`, `data.table`, `sf`, `s2`, `future`, `furrr`, `parallel`, `doParallel`

The project depends on the custom `synfaxgtfs` package for GTFS data processing and routing calculations.