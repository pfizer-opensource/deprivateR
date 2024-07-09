devtools::load_all()
library(dplyr)

pr <- dep_get_index(geography = "county", index = "svi14", year = 2020, territory = "PR", keep_subscales = TRUE, keep_components = TRUE)

pr <- dep_get_index(geography = "zcta5", index = "svi", year = 2020, keep_subscales = TRUE, keep_components = TRUE)

pr %>%
  filter(stringr::str_sub(GEOID, 1, 3) %in% c("006", "007", "008", "009", "969")) %>%
  select(GEOID, starts_with("E_")) -> terr


library(tidycensus)

pr_dis1 <- get_acs(geography = "zcta", variables = c("S2701_C01_001", "DP02_0071", "DP02_0072"), year = 2021, output = "wide") %>%
  filter(stringr::str_sub(GEOID, 1, 3) %in% c("006", "007", "008", "009", "010", "969"))

pr_dis2 <- get_acs(geography = "zcta", table = "B18101", year = 2021, output = "wide") %>%
  filter(stringr::str_sub(GEOID, 1, 3) %in% c("006", "007", "008", "009", "010", "969")) %>%
  mutate(derived = B18101_004E + B18101_007E + B18101_010E + B18101_013E + B18101_016E + B18101_019E +
           B18101_023E + B18101_026E + B18101_029E + B18101_032E + B18101_035E + B18101_038E) %>%
  select(GEOID, B18101_001E, derived)

pr_dis <- left_join(pr_dis1, pr_dis2, by = "GEOID") %>%
  select(GEOID, NAME, ends_with("E"), derived)

pr_county <- get_acs(geography = "county", state = "GU", table = "B01003", output = "wide")


