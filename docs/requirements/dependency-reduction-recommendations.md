# Dependency Reduction Recommendations

> **Issue:** #36  
> **Type:** Discovery spike / Research  
> **Date:** 2026-05-28  
> **Epic:** #15 ([Epic A] CRAN Compliance & Release Readiness)

## Executive Summary

Of 14 `Imports` dependencies, **3 can be removed with minimal effort** (english, tidyselect, tibble), **2 require moderate refactoring** (stringr, tidyr), and **9 should be retained** (dplyr, rlang, sf, tidycensus, ndi, sociome, zippeR, cli, classInt). The quick wins reduce the dependency tree by 3 packages with negligible risk.

## Current Dependency Inventory

| Package | Call sites | Files using | Core functionality? |
|---|---:|---:|---|
| cli | 84 | 9 | No (UX) |
| rlang | 20 | 2 | No (metaprogramming) |
| dplyr | 17 | 7 | No (data manipulation) |
| tidycensus | 8 | 2 | **Yes** (Census API) |
| zippeR | 8 | 2 | **Yes** (ZIP/ZCTA) |
| stringr | 6 | 2 | No (string ops) |
| tibble | 5 | 4 | No (data structure) |
| tidyr | 4 | 2 | No (reshaping) |
| english | 3 | 1 | No (formatting) |
| classInt | 2 | 1 | No (classification) |
| ndi | 2 | 1 | **Yes** (deprivation indices) |
| sociome | 2 | 1 | **Yes** (ADI calculation) |
| sf | 1 | 1 | **Yes** (spatial) |
| tidyselect | 1 | 1 | No (column selection) |

## Tier 1: Quick Wins (Drop with minimal effort)

### 1. `english` → custom utility

**Usage:** `english::ordinal()` in `dep_quantiles.R` (3 call sites)  
**Replacement:** A 5-line internal helper function:

```r
.ordinal_suffix <- function(n) {

  suffix <- ifelse(n %% 100 %in% 11:13, "th",
    switch(as.character(n %% 10), "1" = "st", "2" = "nd", "3" = "rd", "th"))
  paste0(n, suffix)
}
```

**Effort:** ~30 minutes  
**Risk:** None — trivial string formatting  
**Impact:** Removes 1 dependency entirely

### 2. `tidyselect` → base R

**Usage:** `tidyselect::all_of()` in `dep_utils.R` (1 call site)  
**Replacement:** Standard bracket indexing `df[, cols]` or `dplyr::all_of()` (re-exported)  
**Note:** `dplyr` re-exports `all_of()` so if dplyr is retained, this dependency is already redundant in the namespace.  
**Effort:** ~15 minutes  
**Risk:** None  
**Impact:** Removes 1 (effectively redundant) dependency

### 3. `tibble` → dplyr (already available) or base R

**Usage:** `tibble::as_tibble()` and `tibble::tibble()` across 4 files (5 call sites)  
**Replacement:** Since `dplyr` is retained and re-exports `tibble()` and `as_tibble()`, these calls can use `dplyr::as_tibble()` / `dplyr::tibble()` — or switch to `base::data.frame()` if we accept base data.frame output.  
**Effort:** ~30 minutes  
**Risk:** Low — `dplyr` already re-exports these; users already get tibbles from dplyr pipelines  
**Impact:** Removes 1 explicit dependency (already an implicit transitive dependency of dplyr)

## Tier 2: Moderate Effort

### 4. `stringr` → base R

**Usage:** `str_trim()` (base R `trimws()` is identical), `word()` (extracting nth word from strings)  
**Replacement:**
- `stringr::str_trim()` → `base::trimws()`
- `stringr::word(x, n)` → custom helper using `strsplit()` + indexing

**Effort:** ~1 hour  
**Risk:** Low — need to handle edge cases in `word()` replacement  
**Impact:** Removes 1 dependency + its stringi transitive dependency (large compiled package)

### 5. `tidyr` → base R reshape

**Usage:** `pivot_longer()`, `pivot_wider()`, `unchop()` in 2 files (4 call sites)  
**Replacement:** `stats::reshape()` or manual list manipulation for `unchop()`  
**Effort:** ~2–3 hours  
**Risk:** Medium — `reshape()` has a notoriously confusing API; `unchop()` has no clean base equivalent  
**Impact:** Removes 1 dependency

## Tier 3: Retain (High effort or core functionality)

### 6. `dplyr` — **Retain**

17 call sites across 7 files. Deeply embedded in data manipulation logic (`mutate`, `case_when`, `select`, `rename`, `left_join`, `group_by`, `summarise`). Replacing would require rewriting nearly all data processing logic into base R — a massive refactor with high regression risk for marginal gain.

### 7. `rlang` — **Retain**

20 call sites for tidy evaluation (`enquo`, `sym`, `quo_name`). Required for programmatic column selection in user-facing functions. Removing rlang would require fundamentally changing the API design. Also a transitive dependency of dplyr.

### 8. `cli` — **Retain**

84 call sites across 9 files. Recently migrated to cli from base `stop()`/`warning()` per #28. Removing would revert that work. Standard for CRAN packages; CRAN reviewers do not penalize cli usage.

### 9. `classInt` — **Retain**

Provides `classIntervals()` for map break classification (Jenks, Fisher, quantile methods). The statistical algorithms are non-trivial to reimplement and the package is lightweight.

### 10. `sf` — **Retain (core)**

Spatial features are core functionality. No alternative.

### 11. `tidycensus` — **Retain (core)**

Census Bureau API access is core functionality. No practical alternative.

### 12. `ndi` / `sociome` — **Retain (core)**

These implement the actual deprivation index calculations (Messer, Powell-Wiley, ADI). They ARE the package's value proposition.

### 13. `zippeR` — **Retain (core)**

ZIP/ZCTA validation and aggregation. Core spatial lookup functionality.

## Recommended Phased Approach

### Phase 1: Quick wins (est. 1–2 hours total)

1. Remove `english` — replace with internal `.ordinal_suffix()` helper
2. Remove `tidyselect` — redundant with dplyr re-export
3. Remove `tibble` — use dplyr re-exports instead

**Net result:** 14 → 11 explicit Imports

### Phase 2: String operations (est. 1–2 hours)

4. Remove `stringr` — replace with `trimws()` + internal `.str_word()` helper

**Net result:** 11 → 10 explicit Imports  
**Bonus:** Eliminates `stringi` transitive dependency (significant install time reduction)

### Phase 3: Evaluate tidyr (est. 3–4 hours, optional)

5. Remove `tidyr` — requires careful reshape refactoring and thorough testing

**Net result:** 10 → 9 explicit Imports  
**Recommendation:** Only pursue if CRAN reviewers specifically flag dependency count

## Performance Considerations

- **Phase 1:** No performance impact (formatting/type changes only)
- **Phase 2:** `trimws()` vs `str_trim()` — equivalent performance; custom `word()` may be marginally slower but irrelevant at typical data sizes
- **Phase 3:** `stats::reshape()` may be slower than `pivot_*` for large datasets — benchmark before committing

## Summary Table

| Package | Verdict | Phase | Effort | Risk |
|---|---|---|---|---|
| english | **Remove** | 1 | Low | None |
| tidyselect | **Remove** | 1 | Low | None |
| tibble | **Remove** | 1 | Low | Low |
| stringr | **Remove** | 2 | Medium | Low |
| tidyr | **Maybe remove** | 3 | High | Medium |
| dplyr | Retain | — | — | — |
| rlang | Retain | — | — | — |
| cli | Retain | — | — | — |
| classInt | Retain | — | — | — |
| sf | Retain | — | — | — |
| tidycensus | Retain | — | — | — |
| ndi | Retain | — | — | — |
| sociome | Retain | — | — | — |
| zippeR | Retain | — | — | — |

## Next Steps

File implementation issues for each phase:
- Phase 1: Single issue for all three quick-win removals
- Phase 2: Separate issue for stringr removal
- Phase 3: Separate issue for tidyr removal (if desired)
