
<!-- README.md is generated from README.Rmd. Please edit that file -->

# deprivateR

[![R build
status](https://github.com/pfizer-evgen/deprivateR/workflows/R-CMD-check/badge.svg)](https://github.com/pfizer-evgen/deprivateR/actions)

`deprivateR` is meant to provide a unified API for accessing and
calculating a number of different measures of socioeconomic deprivation
in the United States, including the Area Deprivation Index (ADI),
Neighborhood Deprivation Index (NDI), and the Social Vulnerability
Index. The Gini Coefficient can also be returned, though it is not
re-calculated on the fly.

## Motivation

The [`sociome`](https://CRAN.R-project.org/package=sociome) and
[`ndi`](https://CRAN.R-project.org/package=ndi) packages are excellent
contributions, but offer different APIs for returning their respective
indices. `deprivateR` provides a unified interface for accessing these
measures of deprivation, as well as the ability to calculate the various
forms of the Social Vulnerability Index (SVI) that the Centers for
Disease Control and Prevention (CDC) has published. Importantly, SVI can
be calculated for a variety of years and geographic levels. This
functionality expands the possibilities for implementing these measures
in research and public health practice. However, users should also be
aware that ADI, NDI, and SVI have not been extensively validated for
some Census geographies.

## Installation

The development version of `deprivateR` can be accessed from GitHub with
`remotes`:

``` r
# install.packages("remotes")
remotes::install_github("pfizer-evgen/deprivateR")
```

## Usage

### Calculate Deprivation Indices

The core function in `deprivateR` is `dep_get_index()`. This function
returns the specified index for the given geography and year:

``` r
> dep_get_index(geography = "county", state = "MO", index = "adi", year = 2022)
Using FIPS code '29' for state 'MO'
# A tibble: 115 × 3
   GEOID NAME                         ADI
   <chr> <chr>                      <dbl>
 1 29001 Adair County, Missouri     101. 
 2 29003 Andrew County, Missouri     69.4
 3 29005 Atchison County, Missouri  104. 
 4 29007 Audrain County, Missouri   115. 
 5 29009 Barry County, Missouri     106. 
 6 29011 Barton County, Missouri    118. 
 7 29013 Bates County, Missouri     107. 
 8 29015 Benton County, Missouri    103. 
 9 29017 Bollinger County, Missouri 104. 
10 29019 Boone County, Missouri      68.9
# ℹ 105 more rows
# ℹ Use `print(n = ...)` to see more rows
```

The `index` argument can take multiple indicies at once, as can the
`year` argument. This gives users the ability to compare multiple
indicies across multiple years:

``` r
> dep_get_index(geography = "county", state = "MO", index = c("svi20", "svi20s"), year = c(2021, 2022))
Using FIPS code '29' for state 'MO'
# A tibble: 230 × 5
   GEOID NAME                       YEAR SVI_20 SVI_20S
   <chr> <chr>                     <dbl>  <dbl>   <dbl>
 1 29001 Adair County, Missouri     2021  0.377   0.386
 2 29001 Adair County, Missouri     2022  0.456   0.439
 3 29003 Andrew County, Missouri    2021  0       0    
 4 29003 Andrew County, Missouri    2022  0       0    
 5 29005 Atchison County, Missouri  2021  0.149   0.167
 6 29005 Atchison County, Missouri  2022  0.149   0.167
 7 29007 Audrain County, Missouri   2021  0.746   0.781
 8 29007 Audrain County, Missouri   2022  0.886   0.904
 9 29009 Barry County, Missouri     2021  0.702   0.693
10 29009 Barry County, Missouri     2022  0.702   0.667
# ℹ 220 more rows
# ℹ Use `print(n = ...)` to see more rows
```

An alternative to `dep_get_index()` is `dep_calc_index()`, which
provides users with the ability to calculate indicies using
pre-downloaded data. The `dep_sample_data()` function can be used to
explore how this function works using sample data from the 2018-2022
5-year American Community Survey for Missouri Counties:

``` r
> ndi_m <- dep_sample_data(index = "ndi_m")
> dep_calc_index(ndi_m, geography = "county", index = "ndi_m", year = 2022)
Warning: The proportion of variance explained by PC1 is less than 0.50.
# A tibble: 115 × 4
   GEOID NAME                        YEAR   NDI_M
   <chr> <chr>                      <dbl>   <dbl>
 1 29001 Adair County, Missouri      2022  0.0193
 2 29003 Andrew County, Missouri     2022 -0.108 
 3 29005 Atchison County, Missouri   2022 -0.0505
 4 29007 Audrain County, Missouri    2022  0.0107
 5 29009 Barry County, Missouri      2022  0.0129
 6 29011 Barton County, Missouri     2022  0.105 
 7 29013 Bates County, Missouri      2022  0.0679
 8 29015 Benton County, Missouri     2022  0.0283
 9 29017 Bollinger County, Missouri  2022  0.0565
10 29019 Boone County, Missouri      2022 -0.0646
# ℹ 105 more rows
# ℹ Use `print(n = ...)` to see more rows
```

### Additional Functionality

The `deprivateR` package also contains a number of helper functions that
we use in our disparities work. These include:

- `dep_percentiles()`: Calculate percentiles for a given variable in a
  data frame. This is the method used to reproduce SVI estimates, which
  include percentiles for each variable. It is also the method used for
  `dep_get_index()` and `dep_calc_index()` when
  `return_percentiles = TRUE`.
- `dep_quantiles()`: Calculate quantiles for a given variable in a data
  frame. We use this to create tertiles and quartiles for descriptive
  statistics and regression analyses.
- `dep_map_breaks()`: Calculate map breaks for a given variable in a
  data frame. This is useful for creating choropleth maps with package
  like `ggplot2` or `leaflet`. It can be used to create “bins”
  automatically, using any of the algorithms supported by , or accept
  pre-specified breaks.

## Gratitude

`deprivateR` would not be possible without the work of the
[`sociome`](https://CRAN.R-project.org/package=sociome) and
[`ndi`](https://CRAN.R-project.org/package=ndi) packages. The `sociome`
package’s development was led by Nik Krieger, and the `ndi` package’s
author is Ian D. Buller - we’re immensely grateful for their
contributions to the field. Likewise, `deprivateR` would not be possible
without [Kyle Walker’s](https://walker-data.com) packages
[`tigris`](https://CRAN.R-project.org/package=tigris) and
[`tidycensus`](https://walker-data.com/tidycensus/), which provide
access to the underlying U.S. Census Bureau data for calculating these
indices.

## Feedback and Code of Conduct

If you have feedback on `deprivateR`, please [open an issue on
GitHub](https://github.com/pfizer-evgen/deprivateR/issues) after
checking the [contribution
guidelines](https://github.com/pfizer-evgen/deprivateR/blob/main/.github/CONTRIBUTING.md).
Please note that this project is released with a Contributor [Code of
Conduct](https://github.com/pfizer-evgen/deprivateR/blob/main/.github/CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms.
