---
last_updated: 2026-05-28
last_updated_by: github-copilot-cli
owner_skill: glossary/SKILL.md
quadrant: reference
---

# Glossary

Domain terms used across `deprivateR` documentation, code, and issues.

## Deprivation Indices

| Term | Definition | Source |
|------|-----------|--------|
| **ADI** | Area Deprivation Index. A factor-based composite of 17 socioeconomic indicators measuring neighborhood disadvantage. | [Singh (2003)](https://doi.org/10.2105/AJPH.93.7.1137) |
| **Gini Coefficient** | A measure of income inequality ranging from 0 (perfect equality) to 1 (perfect inequality). | U.S. Census Bureau ACS Table B19083 |
| **NDI (Messer)** | Neighborhood Deprivation Index per Messer et al. A principal-component-based composite of 8 census variables. Higher values indicate greater deprivation. | [Messer et al. (2006)](https://doi.org/10.1007/s10900-006-9002-y) |
| **NDI (Powell-Wiley)** | Neighborhood Deprivation Index per Powell-Wiley et al. An alternative NDI formulation using a different variable set and weighting. | [Andrews et al. (2020)](https://doi.org/10.1080/17445647.2020.1750066) |
| **SVI** | Social Vulnerability Index. The CDC/ATSDR composite measure of community vulnerability to health hazards, calculated as percentile ranks across four themes. | [CDC/ATSDR SVI](https://www.atsdr.cdc.gov/placeandhealth/svi/) |
| **SVI Themes** | The four SVI sub-domains: Socioeconomic Status (SES), Household Characteristics and Disability (HCD), Minority Status and Language (MSL), and Housing Type and Transportation (HTT). | CDC/ATSDR |

## Census and Geography

| Term | Definition |
|------|-----------|
| **ACS** | American Community Survey. The U.S. Census Bureau's ongoing annual survey providing demographic, social, economic, and housing estimates. Available as 1-year, 3-year (discontinued after 2013), or 5-year products. |
| **FIPS** | Federal Information Processing Standards code. Numeric codes identifying states (2-digit) and counties (5-digit) in the U.S. |
| **GEOID** | Geographic Identifier. A standardized code uniquely identifying a census geographic unit (e.g., state + county + tract FIPS concatenation). |
| **ZCTA** | ZIP Code Tabulation Area. Census-defined approximations of USPS ZIP Code service areas. ZCTA5 uses 5-digit codes; ZCTA3 aggregates to 3-digit prefixes. |
| **Census Tract** | A small, relatively stable geographic area defined by the Census Bureau, typically containing 1,200 to 8,000 people. The most common unit for neighborhood-level deprivation analysis. |

## Package Concepts

| Term | Definition |
|------|-----------|
| **Choropleth** | A thematic map where areas are shaded proportionally to a measured variable. `dep_map_breaks()` creates classification bins for this purpose. |
| **Subscales** | Sub-components of a composite index (e.g., SVI themes, ADI factor groupings). Retained when `keep_subscales = TRUE`. |
| **Components** | The individual Census demographic variables that feed into an index calculation. Retained when `keep_components = TRUE`. |
| **Percentile rank** | A value's position in a distribution expressed as a percentage (0-100). SVI is always returned as percentile ranks; other indices use raw scores by default. |
| **Quantile** | A division of a distribution into equal-probability groups (quartiles = 4, quintiles = 5, deciles = 10). Used to convert continuous deprivation scores into categorical variables for modeling. |

## R Ecosystem

| Term | Definition |
|------|-----------|
| **tidycensus** | R package providing an interface to the Census Bureau API. Used by `deprivateR` for data download and Gini coefficient retrieval. |
| **sociome** | R package implementing ADI calculations. Wrapped by `dep_process_adi()`. |
| **ndi** | R package implementing NDI (Messer and Powell-Wiley). Wrapped by `dep_process_ndi_m()` and `dep_process_ndi_pw()`. |
| **classInt** | R package providing classification interval algorithms (Fisher-Jenks, quantile, equal, etc.). Used by `dep_map_breaks()`. |
| **sf** | Simple Features for R. The standard package for spatial vector data. Used when `output = "sf"` is requested. |
