---
last_updated: 2026-05-28
last_updated_by: github-copilot-cli
owner_skill: architecture_overview/SKILL.md
quadrant: explanation
---

# Architecture

## Overview

`deprivateR` is an R package that provides a unified interface for calculating area-level deprivation indices from U.S. Census Bureau data. It wraps multiple upstream packages (`sociome`, `ndi`, `tidycensus`) behind a consistent API, adds CDC Social Vulnerability Index (SVI) calculations natively, and provides post-processing utilities for analysis and visualization.

## Package Structure

```
R/
  dep_get_index.R        # Primary user entry point (download + calculate)
  dep_calc_index.R       # Calculate indices on existing data
  dep_build_varlist.R    # Census variable list construction
  dep_get_data.R         # Census data download (internal)
  dep_process.R          # Orchestrator for index-specific processors
  dep_process_adi.R      # ADI calculation (delegates to sociome)
  dep_process_gini.R     # Gini coefficient (delegates to tidycensus)
  dep_process_ndi.R      # NDI Messer + Powell-Wiley (delegates to ndi)
  dep_process_svi.R      # SVI calculation (native implementation)
  dep_quantiles.R        # Quantile binning utility
  dep_rank.R             # Percentile ranking utility
  dep_map_breaks.R       # Choropleth classification breaks
  dep_sample_data.R      # Bundled sample data accessor
  dep_utils.R            # Shared internal helpers
  dep_globals.R          # NSE global variable declarations
  sysdata.rda            # Internal data (see below)
```

## Data Flow

The package supports two primary workflows:

### One-Step (dep_get_index)

```
User call
  -> dep_get_index()
    -> dep_build_varlist()     [determine which Census variables are needed]
    -> dep_get_data()          [download via tidycensus]
    -> dep_process()           [route to index-specific processor]
      -> dep_process_svi()     [or _adi, _gini, _ndi_m, _ndi_pw]
    -> output formatting       [wide, tidy, or sf]
  <- tibble or sf object
```

### Two-Step (dep_calc_index)

```
User provides pre-downloaded data
  -> dep_calc_index()
    -> dep_process()           [route to index-specific processor]
    -> output formatting       [wide or tidy only]
  <- tibble
```

## Key Design Decisions

- **Variable lists are year-sensitive.** Census variable codes change between ACS releases. The `request_vars` internal dataset maps `(index, year, survey)` tuples to the correct variable codes.
- **SVI is computed natively.** Unlike ADI and NDI (which delegate to `sociome` and `ndi`), SVI calculations are implemented directly in `deprivateR` to support four methodology variants (2010, 2014, 2020, 2020-simplified).
- **Geography abstraction.** ZCTA3 and ZCTA5 geographies require special handling (aggregation, crosswalks) that is encapsulated in `dep_get_zcta3()` and `dep_get_zcta5()`.
- **Sample data for offline use.** Missouri county-level 2022 ACS data is bundled in `sysdata.rda` so tests and vignettes run without API keys.

## Internal Data (sysdata.rda)

| Object | Description |
|--------|-------------|
| `mo` | Missouri county-level ACS data (2022) for sample data functions |
| `request_vars` | Lookup table mapping (index, year, survey, geography) to Census variable codes |
| `states_lookup` | State FIPS code and abbreviation crosswalk |

## Exported Functions

| Function | Role |
|----------|------|
| `dep_get_index()` | Download + calculate (primary entry point) |
| `dep_calc_index()` | Calculate on existing data |
| `dep_build_varlist()` | Get required Census variable names |
| `dep_sample_data()` | Access bundled sample datasets |
| `dep_quantiles()` | Bin scores into quantile categories |
| `dep_percentiles()` | Calculate percentile ranks |
| `dep_map_breaks()` | Classification breaks for choropleth maps |

## Dependencies

**Core:** `tidycensus` (Census API), `sociome` (ADI), `ndi` (NDI), `sf` (spatial), `dplyr`/`tidyr`/`tibble` (data manipulation), `classInt` (map breaks), `cli` (user messaging).

**Suggested:** `knitr`/`rmarkdown` (vignettes), `testthat` (testing), `lintr` (style), `spelling` (documentation QC), `tigris` (shapefiles).
