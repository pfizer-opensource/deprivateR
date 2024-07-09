# Validation of deprivateR Against 2020 County-level Data

# dependencies ####
devtools::load_all()

library(dplyr)
library(readr)

# load data ####
## pull data with deprivateR
mo20_dep <- dep_get_index(geography = "county", index = "svi20", year = 2016,
                          state = "MO", keep_subscales = TRUE, keep_components = TRUE)

us20_dep <- dep_get_index(geography = "county", index = "svi20", year = 2020,
                          keep_subscales = TRUE, keep_components = TRUE)

mo20_cdc <- read_csv("data-raw/cdc_missouri_svi_county_2020.csv") %>%
  rename(GEOID = FIPS, SVI_CDC = RPL_THEMES, SVI_SES_CDC = RPL_THEME1,
         SVI_HCD_CDC = RPL_THEME2, SVI_MSL_CDC = RPL_THEME3,
         SVI_HTT_CDC = RPL_THEME4) %>%
  mutate(GEOID = as.character(GEOID)) %>%
  arrange(GEOID)

us20_cdc <- read_csv("data-raw/cdc_us_svi_county_2020.csv") %>%
  rename(GEOID = FIPS, SVI_CDC = RPL_THEMES, SVI_SES_CDC = RPL_THEME1,
         SVI_HCD_CDC = RPL_THEME2, SVI_MSL_CDC = RPL_THEME3,
         SVI_HTT_CDC = RPL_THEME4) %>%
  arrange(GEOID)

# missouri only ####

## primary
all(mo20_cdc$E_TOTPOP == mo20_dep$E_TOTPOP)
all(mo20_cdc$E_HU == mo20_dep$E_HU)
all(mo20_cdc$E_HH == mo20_dep$E_HH)

## ses
all(mo20_cdc$E_POV150 == mo20_dep$E_POV150, na.rm = TRUE)
all(mo20_cdc$E_UNEMP == mo20_dep$E_UNEMP, na.rm = TRUE)
all(mo20_cdc$E_HBURD == mo20_dep$E_HBURD, na.rm = TRUE)
all(mo20_cdc$E_UNINSUR == mo20_dep$E_UNINSUR, na.rm = TRUE)
all(mo20_cdc$E_NOHSDP == mo20_dep$E_NOHSDP, na.rm = TRUE)

## hcd
all(mo20_cdc$E_AGE17 == mo20_dep$E_AGE17, na.rm = TRUE)
all(mo20_cdc$E_AGE65 == mo20_dep$E_AGE65, na.rm = TRUE)
all(mo20_cdc$E_DISABL == mo20_dep$E_DISABL, na.rm = TRUE)
all(mo20_cdc$E_SNGPNT == mo20_dep$E_SNGPNT, na.rm = TRUE) # FALSE
all(mo20_cdc$E_LIMENG == mo20_dep$E_LIMENG, na.rm = TRUE)

# msl

all(mo20_cdc$E_MINRTY == mo20_dep$E_MINRTY, na.rm = TRUE)

# htt
all(mo20_cdc$E_MUNIT == mo20_dep$E_MUNIT, na.rm = TRUE)
all(mo20_cdc$E_MOBILE == mo20_dep$E_MOBILE, na.rm = TRUE)
all(mo20_cdc$E_CROWD == mo20_dep$E_CROWD, na.rm = TRUE)
all(mo20_cdc$E_NOVEH == mo20_dep$E_NOVEH, na.rm = TRUE)
all(mo20_cdc$E_GROUPQ == mo20_dep$E_GROUPQ, na.rm = TRUE)

# measures
all(mo20_cdc$SVI_CDC == mo20_dep$SVI, na.rm = TRUE)
all(mo20_cdc$SVI_SES_CDC == mo20_dep$SVI_SES, na.rm = TRUE)
all(mo20_cdc$SVI_HCD_CDC == mo20_dep$SVI_HCD, na.rm = TRUE)
all(mo20_cdc$SVI_MSL_CDC == mo20_dep$SVI_MSL, na.rm = TRUE)
all(mo20_cdc$SVI_HTT_CDC == mo20_dep$SVI_HTT, na.rm = TRUE)

x <- select(mo20_dep, GEOID, E_SNGPNT_dep = E_SNGPNT)

mo20_cdc %>%
  select(GEOID, E_SNGPNT_cdc = E_SNGPNT) %>%
  left_join(., x, by = "GEOID") -> x

x <- select(mo20_dep, GEOID, SVI_dep = SVI)

mo20_cdc %>%
  select(GEOID, SVI_cdc = SVI_CDC) %>%
  left_join(., x, by = "GEOID") %>%
  mutate(
    SVI_dep_round = round(SVI_dep, digits = 4),
    SVI_match = ifelse(SVI_cdc == SVI_dep_round, TRUE, FALSE),
    SVI_delta = ifelse(SVI_match == FALSE, SVI_cdc - SVI_dep_round, NA),
    .before = "SVI_dep"
  ) -> x

table(x$SVI_match)

hist(x$SVI_delta)


# nationwide data ####

## primary
all(us20_cdc$E_TOTPOP == us20_dep$E_TOTPOP)
all(us20_cdc$E_HU == us20_dep$E_HU)
all(us20_cdc$E_HH == us20_dep$E_HH)

## ses
all(us20_cdc$E_POV150 == us20_dep$E_POV150, na.rm = TRUE)
all(us20_cdc$E_UNEMP == us20_dep$E_UNEMP, na.rm = TRUE)
all(us20_cdc$E_HBURD == us20_dep$E_HBURD, na.rm = TRUE)
all(us20_cdc$E_UNINSUR == us20_dep$E_UNINSUR, na.rm = TRUE)
all(us20_cdc$E_NOHSDP == us20_dep$E_NOHSDP, na.rm = TRUE)

## hcd
all(us20_cdc$E_AGE17 == us20_dep$E_AGE17, na.rm = TRUE)
all(us20_cdc$E_AGE65 == us20_dep$E_AGE65, na.rm = TRUE)
all(us20_cdc$E_DISABL == us20_dep$E_DISABL, na.rm = TRUE)
all(us20_cdc$E_SNGPNT == us20_dep$E_SNGPNT, na.rm = TRUE)

# msl
all(us20_cdc$E_LIMENG == us20_dep$E_LIMENG, na.rm = TRUE)
all(us20_cdc$E_MINRTY == us20_dep$E_MINRTY, na.rm = TRUE)

# htt
all(us20_cdc$E_MUNIT == us20_dep$E_MUNIT, na.rm = TRUE)
all(us20_cdc$E_MOBILE == us20_dep$E_MOBILE, na.rm = TRUE)
all(us20_cdc$E_CROWD == us20_dep$E_CROWD, na.rm = TRUE)
all(us20_cdc$E_NOVEH == us20_dep$E_NOVEH, na.rm = TRUE)
all(us20_cdc$E_GROUPQ == us20_dep$E_GROUPQ, na.rm = TRUE)

# measures
all(us20_cdc$SVI_CDC == us20_dep$SVI, na.rm = TRUE)
all(us20_cdc$SVI_SES_CDC == us20_dep$SVI_SES, na.rm = TRUE)
all(us20_cdc$SVI_HCD_CDC == us20_dep$SVI_HCD, na.rm = TRUE)
all(us20_cdc$SVI_MSL_CDC == us20_dep$SVI_MSL, na.rm = TRUE)
all(us20_cdc$SVI_HTT_CDC == us20_dep$SVI_HTT, na.rm = TRUE)

x <- select(us20_dep, GEOID, SVI_dep = SVI)

us20_cdc %>%
  select(GEOID, SVI_cdc = SVI_CDC) %>%
  left_join(., x, by = "GEOID") %>%
  mutate(
    SVI_dep_round = round(SVI_dep, digits = 4),
    SVI_match = ifelse(SVI_cdc == SVI_dep_round, TRUE, FALSE),
    SVI_delta = ifelse(SVI_match == FALSE, SVI_cdc - SVI_dep_round, NA),
    .before = "SVI_dep"
  ) -> x

table(x$SVI_match)

hist(x$SVI_delta)
