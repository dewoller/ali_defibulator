# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a geospatial analysis project studying defibrillator accessibility in Victoria, Australia. The project analyzes the relationship between defibrillator locations, demographic data (CALD populations, age 55+), cardiac arrest incidents (VACAR data), and socio-economic indicators (SEIFA) using spatial analysis techniques.

## Development Commands

### Running the Pipeline
```bash
# Run the full targets pipeline
Rscript run.R
# or
./run.sh

# Run specific targets interactively in R
targets::tar_make()
targets::tar_make(target_name)  # for specific targets

# Check pipeline status
targets::tar_visnetwork()  # visualize pipeline
targets::tar_outdated()    # see what needs updating
```

### Testing
```bash
# Run all tests
Rscript tests/testthat.R

# In R console for interactive testing
devtools::load_all()
devtools::test()
testthat::test_dir("tests/testthat/")
```

### R Environment Management
```bash
# Restore packages from lockfile
R -e "renv::restore()"

# Update lockfile after adding packages
R -e "renv::snapshot()"
```

## Architecture Overview

### Core Framework
- **Targets Pipeline**: Uses `_targets.R` for reproducible data analysis pipeline
- **Spatial Analysis**: Extensive use of `sf` (Simple Features) for geospatial operations
- **OSRM Integration**: Custom routing using local OSRM server at `http://localhost:1234/`
- **renv**: Package management for reproducible environments

### Key Data Sources
- **Meshblocks**: 2021 Census meshblock boundaries and demographics for Victoria
- **Defibrillator Data**: AED locations from Victoria ambulance registry (SUA data)
- **VACAR**: Victorian Ambulance Cardiac Arrest Registry incident data  
- **Census Data**: CALD populations, age demographics, SEIFA socio-economic indices
- **Geographic Boundaries**: SA1, SA2 areas, suburbs from Australian Bureau of Statistics

### Main Data Processing Flow
1. **Reference Data Setup**: Load meshblocks, census boundaries, establish CRS
2. **Defibrillator Processing**: Clean and geocode AED locations, create service catchment areas (isochrones)
3. **Spatial Intersection**: Calculate which meshblocks fall within defibrillator catchments
4. **Distance Calculations**: Find closest defibrillators to each meshblock/incident using OSRM
5. **Demographic Analysis**: Aggregate census data (age, CALD, SEIFA) by service areas
6. **Export**: Generate CSV and geospatial outputs for analysis

### Custom R Functions (in `/R/`)
- `find_closest_osrm_point.R`: OSRM-based routing distance calculations
- `point2isochrone.R`: Generate walking catchment areas with fallback buffers  
- `find_n_closest_mb.R`: Spatial joins for meshblock analysis
- `summarise_one_sua.R`: Aggregate demographics within service catchment areas
- `get_victoria_defib_ll.R`: Web scraping defibrillator locations
- `packages.R`: Centralized package loading with conflict resolution

### Key Spatial Concepts
- **SUA**: Service Unit Areas - isochrone catchment areas around defibrillators
- **Meshblocks**: Smallest census geographic units for demographic data
- **Reservoir Analysis**: Special focus area with 400m buffer zone
- **Distance Metrics**: Both Euclidean and network-based (OSRM) routing distances

## Dependencies and Infrastructure

### External Services Required
- **OSRM Server**: Must be running at `http://localhost:1234/` for routing calculations
- **Reference Datasets**: Census and boundary files in `~/code/reference_datasets/`

### Key R Packages
- Spatial: `sf`, `nngeo`, `units`, `strayr`  
- Data: `targets`, `dplyr`, `tidyr`, `purrr`, `readr`, `janitor`
- Routing: `osrm`, `tidygeocoder`
- Visualization: `leaflet`
- Testing: `testthat`, `devtools`

### Data Outputs
- CSV files: `vacar.csv`, `mesh.csv`, `sua_noh.csv` 
- GeoJSON: All spatial datasets for mapping
- Excel: Combined exports in `victoria_export.xlsx`
- Versioned outputs in `1m/`, `2m/` subdirectories

## Testing Strategy

Tests focus on spatial operations and data processing:
- Geospatial function validation with mock data
- OSRM integration error handling  
- Census data processing accuracy
- Integration tests for full pipeline segments

## Special Considerations

### Performance
- Large spatial datasets require memory management
- OSRM calls are rate-limited and can timeout
- Pipeline uses caching via targets for expensive computations

### Data Quality
- Coordinate validation for OSRM calls (removes NA coordinates)
- Defibrillator data deduplicated by lat/lon
- Fallback buffers when isochrone generation fails
- SJA (St John Ambulance) defibrillators analyzed separately

### Geographic Focus
- Analysis restricted to Victoria (state code "2")  
- Special attention to Reservoir suburb as case study area
- CRS transformations between WGS84 (4326) and projected coordinates