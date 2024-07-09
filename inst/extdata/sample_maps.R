## variable definitions
### https://www.atsdr.cdc.gov/placeandhealth/svi/documentation/SVI_documentation_2018.html
### vars_all <- load_variables(year = 2019, dataset = "acs5")
### https://api.census.gov/data/2019/acs/acs5/profile/variables.html
### https://api.census.gov/data/2019/acs/acs5/subject/variables.html

devtools::load_all()
library(ggplot2)
library(tigris)
library(dplyr)

dep_get_index(geography = "county", index = "gini", year = 2020, state = "MO")
z <- dep_get_index(geography = "zcta5", index = "svi", year = 2020, territory = NULL, keep_subscales = TRUE)
counties <- dep_get_index(geography = "county", index = c("gini", "adi", "svi"), year = 2020, state = "MO",
                          geometry = TRUE)

county <- dep_get_index(geography = "county", index = "gini", year = 2020, territory = "PR", zcta3_method = "mean", geometry = TRUE, cb = TRUE, shift_geo = TRUE)
county <- dep_get_index(geography = "county", index = "svi", year = 2020, territory = NULL,
                        output = "sf", cb = TRUE, shift_geo = TRUE) %>%
  mutate(SVI = SVI*100)

county <- dep_get_index(geography = "county", index = "svi", year = 2020, territory = NULL, geometry = FALSE, keep_subscales = TRUE, keep_components = TRUE)

readr::write_csv(county, "inst/extdata/svi2020_all.csv")
# county <- shift_geometry(county, position = "below")

zcta5 <- dep_get_index(geography = "zcta5", index = "adi", year = 2020, territory = "PR", zcta3_method = "mean", geometry = TRUE, cb = TRUE, shift_geo = TRUE)
zcta3 <- dep_get_index(geography = "zcta3", index = "svi", year = 2020, territory = "PR", zcta3_method = "mean", geometry = TRUE, shift_geo = TRUE)



# zcta_sub <- filter(zcta3, substr(ZCTA3, 1,1) %in% "6")

## states
states <- states(cb=TRUE) %>%
  filter(STUSPS %in% c("AS", "GU", "MP", "VI") == FALSE)
states <- shift_geometry(states, position = "below")

# states_sub <- filter(states, STUSPS == "MO")

## create breaks
zcta3 <- map_breaks(zcta3, var = "SVI", newvar = "map_svi",
                       style = "fisher", classes = 5, dig_lab = 3)
zcta5$ADI_rank <- dep_percent_rank(zcta5$ADI)*100
zcta5 <- map_breaks(zcta5, var = "ADI_rank", newvar = "map_adi",
                       style = "fisher", classes = 5, dig_lab = 3)
county <- map_breaks(county, var = "E_GINI", newvar = "map_gini",
                       style = "fisher", classes = 5, dig_lab = 3)
county <- map_breaks(county, var = "SVI", newvar = "map_svi",
                     style = "fisher", classes = 5, dig_lab = 4)
county <- map_breaks(county, var = "SVI", newvar = "map_svi",
                     style = "quantile", classes = 4, dig_lab = 4)

# map SVI ####
## create map
p <- ggplot() +
  geom_sf(data = states, fill = "#e0e0e0", color = NA) +
  geom_sf(data = county, mapping = aes(fill = map_svi), size = .2) +
  geom_sf(data = states, fill = NA, color = "black", size = 1.2) +
  scale_fill_brewer(palette = "Blues", name = "SVI", na.value = "#e0e0e0") +
  labs(
    # title = "Social Vulnerability Index (2020)",
    # subtitle = "Counties"
    title = "SVI by County (2020)"
  ) +
  sequoia_theme(base_size = 18, background = "white", map = TRUE)

## save map
save_plots(filename = "inst/extdata/counties_svi_2020.png", plot = p, preset = "lg")

## create map
p <- ggplot() +
  geom_sf(data = states, fill = "#e0e0e0", color = NA) +
  geom_sf(data = county, mapping = aes(fill = map_svi), size = .2) +
  geom_sf(data = states, fill = NA, color = "black", size = 1.2) +
  scale_fill_brewer(palette = "Blues", name = "SVI", na.value = "#e0e0e0") +
  labs(
    title = "Social Vulnerability Index (2020)",
    subtitle = "Counties"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save map
save_plots(filename = "inst/extdata/county_svi_2020.png", plot = p, preset = "lg")

# map ADI ####
## create map
p <- ggplot() +
  geom_sf(data = states, fill = "#e0e0e0", color = NA) +
  geom_sf(data = zcta5, mapping = aes(fill = map_adi), size = .05) +
  geom_sf(data = states, fill = NA, color = "black", size = 1.2) +
  scale_fill_brewer(palette = "Greens", name = "ADI Percentiles", na.value = "#e0e0e0") +
  labs(
    title = "Area Deprivation Index (2020)",
    subtitle = "Five-Digit ZIP Code Tabulation Areas"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save map
save_plots(filename = "inst/extdata/zcta5_adi_2020.png", plot = p, preset = "lg")

# map Gini ####
## create map
p <- ggplot() +
  geom_sf(data = states, fill = "#e0e0e0", color = NA) +
  geom_sf(data = county, mapping = aes(fill = map_gini), size = .2) +
  geom_sf(data = states, fill = NA, color = "black", size = 1.2) +
  scale_fill_brewer(palette = "Purples", name = "Gini Coefficient", na.value = "#e0e0e0") +
  labs(
    title = "Gini Coefficient (2020)",
    subtitle = "Counties"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save map
save_plots(filename = "inst/extdata/county_gini_2020.png", plot = p, preset = "lg")

# create vector of variables to pull
varlist <- dep_build_varlist(geography = "county", index = "adi",
                             year = 2020, survey = "acs5", output = "vector")

# create demo and geo tables
demo <- dep_get_data(geography = "county", varlist = varlist, year = 2019,
                     survey = "acs5", state = "MO", county = NULL, geometry = FALSE,
                     keep_geo_vars = FALSE, shift_geo = FALSE, debug = "live")
demo <- demo$demo

dep_process_adi(demo, geography = "county", year = 2019, survey = "acs5", adi = TRUE, adi3 = TRUE, keep_components = FALSE)

# _001 - HOUSEHOLDS BY TYPE


y <- tidycensus::get_acs(geography = "county", year = 2019, state = "MO", variables = c("B11012_001", "B11012_010", "B11012_015"), output = "wide")
z <- tidycensus::get_acs(geography = "county", year = 2019, state = "MO", variables = c("DP02_0001", "DP02_0007", "DP02_0011"), output = "wide")
#

