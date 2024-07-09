current <- c("S1101_C01_001E",
  "B25115_001",
  "B25115_009", # owner occupied, male head, no spouse, own kids < 18 (2009-2021)
"B25115_012", # owner occupied, female head, no spouse, own kids < 18 (2009-2021)
"B25115_022", # renter occupied, male head, no spouse, own kids < 18 (2009-2021)
"B25115_025")

cdc18 <- c("DP02_0001", "DP02_0007", "DP02_0009")
cdc19 <- c("B11012_001", "B11012_010", "B11012_015", "DP02_0001", "DP02_0007", "DP02_0011")

extra <- c("S1903_C01_001", "S1903_C01_021", "S1903_C01_023")

library(dplyr)
library(tidycensus)

sngpnt14 <- get_acs(geography = "county", state = "MO", variables = c(current, cdc18, extra),
                    year = 2014, output = "wide") %>%
  rename(
    S1101_denom = S1101_C01_001E,
    B25115_denom = B25115_001E,
    DP02_denom = DP02_0001E
  ) %>%
  mutate(
    B25115_count = B25115_009E+B25115_012E+B25115_022E+B25115_025E,
    DP02_count = DP02_0007E+DP02_0009E
  ) %>%
  select(GEOID, S1101_denom, B25115_denom, DP02_denom, B25115_count, DP02_count)

sngpnt18 <- get_acs(geography = "county", state = "MO", variables = c(current, cdc18, extra),
                    year = 2018, output = "wide") %>%
  rename(
    S1101_denom = S1101_C01_001E,
    B25115_denom = B25115_001E,
    S1903_denom = S1903_C01_001E,
    DP02_denom = DP02_0001E
  ) %>%
  mutate(
    B25115_count = B25115_009E+B25115_012E+B25115_022E+B25115_025E,
    S1903_count = S1903_C01_021E+S1903_C01_023E,
    DP02_count = DP02_0007E+DP02_0009E
  ) %>%
  select(GEOID, B25115_denom, S1101_denom, S1903_denom, DP02_denom, B25115_count, S1903_count, DP02_count)

sngpnt19 <- get_acs(geography = "county", state = "MO", variables = c(current, cdc19, extra),
                    year = 2019, output = "wide") %>%
  rename(
    S1101_denom = S1101_C01_001E,
    B25115_denom = B25115_001E,
    S1903_denom = S1903_C01_001E,
    DP02_denom = DP02_0001E,
    B11012_denom = B11012_001E
  ) %>%
  mutate(
    B25115_count = B25115_009E+B25115_012E+B25115_022E+B25115_025E,
    S1903_count = S1903_C01_021E+S1903_C01_023E,
    DP02_count = DP02_0007E+DP02_0011E,
    B11012_count = B11012_010E+B11012_015E
  ) %>%
  select(GEOID, B25115_denom, S1101_denom, S1903_denom, DP02_denom, B11012_denom, B25115_count, S1903_count, DP02_count, B11012_count)

sngpnt19 <- get_acs(geography = "county", state = "MO", variables = c(current, cdc19, extra),
                  year = 2019, output = "wide") %>%
  rename(
    S1101_denom = S1101_C01_001E,
    B25115_denom = B25115_001E,
    S1903_denom = S1903_C01_001E,
    DP02_denom = DP02_0001E,
    B11012_denom = B11012_001E
  ) %>%
  mutate(
    B25115_count = B25115_009E+B25115_012E+B25115_022E+B25115_025E,
    S1903_count = S1903_C01_021E+S1903_C01_023E,
    DP02_count = DP02_0007E+DP02_0011E,
    B11012_count = B11012_010E+B11012_015E
  ) %>%
  select(GEOID, B25115_denom, S1101_denom, S1903_denom, DP02_denom, B11012_denom, B25115_count, S1903_count, DP02_count, B11012_count)


sngpnt20 <- get_acs(geography = "county", state = "MO", variables = c(current, cdc19, extra),
                    year = 2020, output = "wide") %>%
  rename(
    S1101_denom = S1101_C01_001E,
    B25115_denom = B25115_001E,
    S1903_denom = S1903_C01_001E,
    DP02_denom = DP02_0001E,
    B11012_denom = B11012_001E
  ) %>%
  mutate(
    B25115_count = B25115_009E+B25115_012E+B25115_022E+B25115_025E,
    S1903_count = S1903_C01_021E+S1903_C01_023E,
    DP02_count = DP02_0007E+DP02_0011E,
    B11012_count = B11012_010E+B11012_015E
  ) %>%
  select(GEOID, B25115_denom, S1101_denom, S1903_denom, DP02_denom, B11012_denom, B25115_count, S1903_count, DP02_count, B11012_count)

sngpnt21 <- get_acs(geography = "county", state = "MO", variables = c(current, cdc19, extra),
                  year = 2021, output = "wide") %>%
  rename(
    S1101_denom = S1101_C01_001E,
    B25115_denom = B25115_001E,
    S1903_denom = S1903_C01_001E,
    DP02_denom = DP02_0001E,
    B11012_denom = B11012_001E
  ) %>%
  mutate(
    B25115_count = B25115_009E+B25115_012E+B25115_022E+B25115_025E,
    S1903_count = S1903_C01_021E+S1903_C01_023E,
    DP02_count = DP02_0007E+DP02_0011E,
    B11012_count = B11012_010E+B11012_015E
  ) %>%
  select(GEOID, B25115_denom, S1101_denom, S1903_denom, DP02_denom, B11012_denom, B25115_count, S1903_count, DP02_count, B11012_count)

library(tidyr)

sngpnt18 %>%
  slice(1) %>%
  pivot_longer(cols = names(sngpnt18)[-1], names_to = "variable", values_to = "value") %>%
  mutate(year = 2018, .before = "GEOID")



sngpnt19 %>%
  slice(1) %>%
  pivot_longer(cols = names(sngpnt19)[-1], names_to = "variable", values_to = "value") %>%
  mutate(year = 2019, .before = "GEOID")

