# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

### Added

- Test coverage analysis report at `docs/audits/2026-05-29-coverage-analysis.md` — establishes 34.7% baseline and identifies critical gaps (#51)
- `dep_set_api_key()` — wrapper around `tidycensus::census_api_key()` with cascading key lookup (explicit key → env var → interactive prompt) (#11)
- Unit tests for `dep_process()` dispatcher, SVI sub-theme functions, NDI (Messer + Powell-Wiley), ADI, Gini, and `validate_state()` — 173 new tests (#55, #57, #56)

### Fixed

- pkgdown CI build failure caused by `docs/` directory conflict — pkgdown now outputs to `_site/` (#35)

### Changed

- Removed manual `@usage` roxygen2 tags from function documentation; usage is now auto-generated from signatures (#40)
- Migrated all `stop()`/`warning()`/`message()` calls to `cli::cli_abort()`/`cli::cli_warn()`/`cli::cli_inform()` for tidyverse-style formatted error messages (#28)
- Removed `english`, `tibble`, and `tidyselect` from Imports; replaced with `dplyr` re-exports (14 → 11 dependencies) (#44)
- Removed `stringr` from Imports; replaced with `trimws()` and internal `.dep_str_word()` helper (11 → 10 dependencies) (#43)
- Bumped minimum R version from 4.1 to 4.4 (required by Matrix package dependency chain)
- Bumped minimum R version from 3.5 to 4.1 (#5)
- Removed stale `CRAN-SUBMISSION` file from v0.1.0 (#7)
- Removed duplicate `.github` entry in `.Rbuildignore` (#8)

### Added

- Dependency reduction recommendations document with phased removal strategy (#36)
- Introductory "Getting Started" vignette covering all supported indices, sample data workflow, `dep_get_index()` one-step workflow, quantiles, map breaks, and spatial output (#6)
- Substantive content for `docs/ARCHITECTURE.md`, `docs/GLOSSARY.md`, `docs/OBJECTIVES.md`, and `docs/decisions/` (#19)
- Unit tests for `dep_get_index`, `dep_map_breaks`, `dep_quantiles`, `dep_percentiles`, and internal helpers — 179 assertions across 8 test files (#3)
- testthat 3rd edition adopted (`Config/testthat/edition: 3` in DESCRIPTION) (#4)
- lintr CI workflow with `.lintr` config; currently enforces `linters_with_defaults()` minus style-only linters that conflict with existing code (#12)
- `LICENSE.md` with full Apache 2.0 text for pkgdown site rendering (#9)
- `CHANGELOG.md` excluded from R package tarball via `.Rbuildignore` (#21)
- Spelling check in CI workflow with `inst/WORDLIST` for domain terms (#27)
- `Language: en-US` field to DESCRIPTION
- R package caching (`cache: TRUE`) in pkgdown workflow (#30)
- System dependency caching (GDAL, PROJ, GEOS, udunits) in R-CMD-check workflow (#30)

### Fixed

- Typos in documentation: "appeneded" → "appended", "Sruvey" → "Survey", "indicies" → "indices" (#27)
