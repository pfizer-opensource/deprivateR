# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

### Changed

- Bumped minimum R version from 3.5 to 4.1 (#5)
- Removed stale `CRAN-SUBMISSION` file from v0.1.0 (#7)
- Removed duplicate `.github` entry in `.Rbuildignore` (#8)

### Added

- `LICENSE.md` with full Apache 2.0 text for pkgdown site rendering (#9)
- `CHANGELOG.md` excluded from R package tarball via `.Rbuildignore` (#21)
- Spelling check in CI workflow with `inst/WORDLIST` for domain terms (#27)
- `Language: en-US` field to DESCRIPTION

### Fixed

- Typos in documentation: "appeneded" → "appended", "Sruvey" → "Survey", "indicies" → "indices" (#27)
