---
status: draft
owner: prenec
created: 2026-05-29
last-updated: 2026-05-29
related-epic(s): TBD (Epic F, not yet filed)
---
# Requirements: Code Quality & Test Coverage

## 1. Background

The deprivateR package has grown through five epics (A–E) focused on CRAN compliance, test quality, documentation, new features, and dependency reduction. With that foundation in place, a systematic audit of both source code quality and test coverage is needed to identify remaining gaps before the next feature push. Currently, 7 of 16 R source files have no corresponding test file (notably the `dep_process_*` family and `dep_get_data.R`), and no formal code quality analysis has been performed to surface maintainability or performance improvements.

## 2. Sources

- **S1.** Repo file listing — 16 R source files, 9 test files; coverage gap is visible from file-count mismatch alone.
- **S2.** Quick-capture issues #50 and #51 — user-identified need for both source code and test analysis.
- **S3.** Prior Epic B (Test Quality & Developer Experience, #16) — established the testing framework but focused on test infrastructure rather than comprehensive coverage analysis.

## 3. Requirements

### R1. Produce a test coverage report

- **Rationale:** Without a quantitative baseline, prioritizing test gaps is guesswork.
- **Source(s):** S1, S2
- **Acceptance criteria:** A per-file and per-function coverage report exists (e.g., via `covr`); coverage percentage is known for every exported function.
- **Out of scope:** Achieving a specific coverage target (that's a follow-up decision).
- **Notes:** The report should identify both line coverage and branch coverage where tooling supports it.

### R2. Identify and prioritize untested code paths

- **Rationale:** Not all coverage gaps are equal — core computation functions (e.g., `dep_process_adi`, `dep_calc_index`) carry higher regression risk than utility helpers.
- **Source(s):** S1, S3
- **Acceptance criteria:** A prioritized list of untested functions/code paths exists, ranked by risk (exported vs. internal, complexity, user-facing impact).
- **Out of scope:** Writing the tests themselves (that's follow-up work filed as sub-issues).

### R3. Conduct source code quality analysis

- **Rationale:** Code quality issues (duplicated logic, overly complex functions, inconsistent patterns) accumulate over time and increase maintenance cost.
- **Source(s):** S2
- **Acceptance criteria:** A documented analysis identifies concrete improvement opportunities (e.g., function decomposition, pattern standardization, performance hotspots) with actionable recommendations.
- **Out of scope:** Implementing the improvements (follow-up issues).

### R4. File follow-up issues for identified gaps

- **Rationale:** The analysis is only valuable if it produces actionable backlog items.
- **Source(s):** S2, S3
- **Acceptance criteria:** Each significant finding (coverage gap or code quality issue) has a corresponding GitHub issue filed under Epic F with acceptance criteria.
- **Out of scope:** Prioritizing the follow-up issues (that's triage/grooming work).
