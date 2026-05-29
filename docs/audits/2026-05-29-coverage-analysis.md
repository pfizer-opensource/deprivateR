# Test Coverage Analysis

_Date: 2026-05-29_
_Tool: covr (R package coverage)_
_Parent issue: #51_
_Parent epic: #52 (Code Quality & Test Coverage)_

## Summary

| Metric | Value |
|--------|-------|
| **Overall line coverage** | **34.7%** |
| R source files | 16 (excl. `sysdata.rda`) |
| Files with 0% coverage | 4 |
| Files with 100% coverage | 3 |
| Exported functions | 7 |
| Internal functions | 25 |

## Per-File Coverage

| File | Lines Covered | Total Lines | Coverage |
|------|--------------|-------------|----------|
| `dep_quantiles.R` | 97 | 97 | **100.0%** ✅ |
| `dep_rank.R` | 18 | 18 | **100.0%** ✅ |
| `dep_sample_data.R` | 14 | 14 | **100.0%** ✅ |
| `dep_map_breaks.R` | 71 | 72 | **98.6%** ✅ |
| `dep_calc_index.R` | 87 | 117 | 74.4% |
| `dep_set_api_key.R` | 20 | 28 | 71.4% |
| `dep_get_index.R` | 80 | 119 | 67.2% |
| `dep_utils.R` | 21 | 36 | 58.3% |
| `dep_build_varlist.R` | 142 | 299 | 47.5% |
| `dep_process.R` | 25 | 90 | 27.8% |
| `dep_process_ndi.R` | 35 | 177 | 19.8% |
| `dep_get_data.R` | 0 | 232 | **0.0%** 🔴 |
| `dep_process_adi.R` | 0 | 29 | **0.0%** 🔴 |
| `dep_process_gini.R` | 0 | 7 | **0.0%** 🔴 |
| `dep_process_svi.R` | 0 | 422 | **0.0%** 🔴 |
| `dep_globals.R` | — | — | N/A (constants only) |

## Exported Functions

All 7 exported functions and their coverage status:

| Function | File | Coverage | Has Test File |
|----------|------|----------|---------------|
| `dep_build_varlist()` | `dep_build_varlist.R` | 47.5% | ✅ |
| `dep_calc_index()` | `dep_calc_index.R` | 74.4% | ✅ |
| `dep_get_index()` | `dep_get_index.R` | 67.2% | ✅ |
| `dep_map_breaks()` | `dep_map_breaks.R` | 98.6% | ✅ |
| `dep_percentiles()` | `dep_rank.R` | 100.0% | ✅ |
| `dep_quantiles()` | `dep_quantiles.R` | 100.0% | ✅ |
| `dep_sample_data()` | `dep_sample_data.R` | 100.0% | ✅ |

## Untested Internal Functions (0% Coverage Files)

These internal functions have **zero test coverage**:

### Critical — Core Processing Pipeline

| Function | File | Lines | Risk |
|----------|------|-------|------|
| `dep_get_data()` | `dep_get_data.R` | 232 | **High** — primary data retrieval; calls Census API |
| `dep_process()` | `dep_process.R` | 90 | **High** — top-level dispatcher for all index processing |
| `dep_process_svi()` | `dep_process_svi.R` | 422 | **High** — largest file; SVI index with 5 sub-theme functions |
| `dep_process_svi_ses()` | `dep_process_svi.R` | (included) | High — SVI socioeconomic sub-theme |
| `dep_process_svi_hhd()` | `dep_process_svi.R` | (included) | High — SVI household/disability sub-theme |
| `dep_process_svi_msl()` | `dep_process_svi.R` | (included) | High — SVI minority status/language sub-theme |
| `dep_process_svi_htt()` | `dep_process_svi.R` | (included) | High — SVI housing type/transport sub-theme |
| `dep_process_svi_pri()` | `dep_process_svi.R` | (included) | High — SVI primary composite |
| `dep_process_ndi_m()` | `dep_process_ndi.R` | 177 | **High** — NDI Messer variant |
| `dep_process_ndi_pw()` | `dep_process_ndi.R` | (included) | High — NDI Powell-Wiley variant |
| `dep_process_adi()` | `dep_process_adi.R` | 29 | Medium — ADI index processing |
| `dep_process_gini()` | `dep_process_gini.R` | 7 | Low — thin wrapper |

### Supporting Functions (Partial or No Coverage)

| Function | File | Coverage | Risk |
|----------|------|----------|------|
| `dep_get_census()` | `dep_get_data.R` | 0% | High — Census API interface |
| `dep_get_zcta3()` | `dep_get_data.R` | 0% | Medium — ZCTA3 crosswalk |
| `dep_get_zcta5()` | `dep_get_data.R` | 0% | Medium — ZCTA5 crosswalk |
| `validate_state()` | `dep_get_data.R` | 0% | Medium — input validation |
| `dep_build_multi_varlist()` | `dep_build_varlist.R` | partial | Low |
| `dep_expand_varlist()` | `dep_build_varlist.R` | partial | Low |
| `build_adi_varlist()` | `dep_build_varlist.R` | partial | Low |
| `dep_zcta3_varlist()` | `dep_build_varlist.R` | partial | Low |
| `pivot_demos()` | `dep_utils.R` | partial | Low |
| `dep_ordinal()` | `dep_utils.R` | partial | Low |
| `simpleCapSO()` | `dep_utils.R` | partial | Low |
| `dep_percent_rank()` | `dep_utils.R` | partial | Low |
| `dep_quantile_label()` | `dep_quantiles.R` | 100% | — |

## Risk-Prioritized Recommendations

### Priority 1 — High Risk, High Impact

1. **`dep_process.R` + `dep_process_*.R` family** — The entire processing pipeline is untested or minimally tested. These functions transform raw Census data into deprivation indices. A regression here silently produces incorrect index values.
   - Recommended: Mock `dep_get_data()` return values and test each `dep_process_*` function in isolation.

2. **`dep_get_data.R`** — The data retrieval layer (232 lines, 0% coverage). Contains Census API calls, ZCTA crosswalk logic, and state validation.
   - Recommended: Test `validate_state()` and crosswalk logic with mocked HTTP responses (use `httptest` or `webmockr`). Network-dependent tests should be separate from unit tests.

### Priority 2 — Medium Risk

3. **`dep_build_varlist.R`** (47.5%) — Variable list construction. Partially tested but many code paths in `dep_build_multi_varlist()` and `dep_expand_varlist()` remain uncovered.

4. **`dep_set_api_key.R`** (71.4%) — Some edge cases in key validation untested.

### Priority 3 — Low Risk (Already Well-Covered)

5. Files at 98–100% coverage (`dep_map_breaks.R`, `dep_quantiles.R`, `dep_rank.R`, `dep_sample_data.R`) — no action needed.

## Testing Strategy Recommendations

- **Mocking approach**: Use `httptest` or `webmockr` for Census API calls rather than live API tests
- **Test data**: Leverage `dep_sample_data()` which provides pre-built sample datasets for testing
- **Isolation**: Test `dep_process_*` functions independently by providing pre-formatted input data frames
- **Parameterized tests**: The `dep_process_*` family shares patterns — consider parameterized test helpers

## Coverage Baseline

This report establishes the following baseline for regression tracking:

```
Overall: 34.7% line coverage
Date: 2026-05-29
Tool: covr::package_coverage()
R version: (current)
```

Future PRs that reduce coverage below this baseline should be flagged.
