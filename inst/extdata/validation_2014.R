# Validation of deprivateR Against 2014 County-level Data

# dependencies ####
devtools::load_all()

library(dplyr)
library(readr)

# load data ####
## pull data with deprivateR
mo14_dep <- dep_get_index(geography = "county", index = "svi14", year = 2014,
                          state = "MO", keep_subscales = TRUE, keep_components = TRUE)

us14_dep <- dep_get_index(geography = "county", index = "svi14", year = 2014,
                          keep_subscales = TRUE, keep_components = TRUE)


mo14_cdc <- read_csv("data-raw/cdc_missouri_svi_county_2014.csv") %>%
  mutate(FIPS = as.character(FIPS)) %>%
  rename(GEOID = FIPS, SVI_CDC = RPL_THEMES, SVI_SES_CDC = RPL_THEME1,
         SVI_HCD_CDC = RPL_THEME2, SVI_MSL_CDC = RPL_THEME3,
         SVI_HTT_CDC = RPL_THEME4) %>%
  arrange(GEOID)

us14_cdc <- read_csv("data-raw/cdc_us_svi_county_2014.csv") %>%
  rename(GEOID = FIPS, SVI_CDC = RPL_THEMES, SVI_SES_CDC = RPL_THEME1,
         SVI_HCD_CDC = RPL_THEME2, SVI_MSL_CDC = RPL_THEME3,
         SVI_HTT_CDC = RPL_THEME4) %>%
  arrange(GEOID)

# missouri only ####

## primary
all(mo14_cdc$E_TOTPOP == mo14_dep$E_TOTPOP)
all(mo14_cdc$E_HU == mo14_dep$E_HU)
all(mo14_cdc$E_HH == mo14_dep$E_HH)

## ses
all(mo14_cdc$E_POV == mo14_dep$E_POV, na.rm = TRUE)
all(mo14_cdc$E_UNEMP == mo14_dep$E_UNEMP, na.rm = TRUE)
all(mo14_cdc$E_PCI == mo14_dep$E_PCI, na.rm = TRUE)
all(mo14_cdc$E_NOHSDP == mo14_dep$E_NOHSDP, na.rm = TRUE)

## hcd
all(mo14_cdc$E_AGE17 == mo14_dep$E_AGE17, na.rm = TRUE)
all(mo14_cdc$E_AGE65 == mo14_dep$E_AGE65, na.rm = TRUE)
all(mo14_cdc$E_DISABL == mo14_dep$E_DISABL, na.rm = TRUE)
all(mo14_cdc$E_SNGPNT == mo14_dep$E_SNGPNT, na.rm = TRUE)

# msl
all(mo14_cdc$E_LIMENG == mo14_dep$E_LIMENG, na.rm = TRUE)
all(mo14_cdc$E_MINRTY == mo14_dep$E_MINRTY, na.rm = TRUE)

# htt
all(mo14_cdc$E_MUNIT == mo14_dep$E_MUNIT, na.rm = TRUE)
all(mo14_cdc$E_MOBILE == mo14_dep$E_MOBILE, na.rm = TRUE)
all(mo14_cdc$E_CROWD == mo14_dep$E_CROWD, na.rm = TRUE)
all(mo14_cdc$E_NOVEH == mo14_dep$E_NOVEH, na.rm = TRUE)
all(mo14_cdc$E_GROUPQ == mo14_dep$E_GROUPQ, na.rm = TRUE)

# measures
all(mo14_cdc$SVI_CDC == mo14_dep$SVI, na.rm = TRUE)
all(mo14_cdc$SVI_SES_CDC == mo14_dep$SVI_SES, na.rm = TRUE)
all(mo14_cdc$SVI_HCD_CDC == mo14_dep$SVI_HCD, na.rm = TRUE)
all(mo14_cdc$SVI_MSL_CDC == mo14_dep$SVI_MSL, na.rm = TRUE)
all(mo14_cdc$SVI_HTT_CDC == mo14_dep$SVI_HTT, na.rm = TRUE)

x <- select(mo14_dep, GEOID, SVI_dep = SVI)

mo14_cdc %>%
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
all(us14_cdc$E_TOTPOP == us14_dep$E_TOTPOP)
all(us14_cdc$E_HU == us14_dep$E_HU)
all(us14_cdc$E_HH == us14_dep$E_HH)

## ses
all(us14_cdc$E_POV == us14_dep$E_POV, na.rm = TRUE)
all(us14_cdc$E_UNEMP == us14_dep$E_UNEMP, na.rm = TRUE)
all(us14_cdc$E_PCI == us14_dep$E_PCI, na.rm = TRUE)
all(us14_cdc$E_NOHSDP == us14_dep$E_NOHSDP, na.rm = TRUE)

## hcd
all(us14_cdc$E_AGE17 == us14_dep$E_AGE17, na.rm = TRUE)
all(us14_cdc$E_AGE65 == us14_dep$E_AGE65, na.rm = TRUE)
all(us14_cdc$E_DISABL == us14_dep$E_DISABL, na.rm = TRUE)
all(us14_cdc$E_SNGPNT == us14_dep$E_SNGPNT, na.rm = TRUE)

# msl
all(us14_cdc$E_LIMENG == us14_dep$E_LIMENG, na.rm = TRUE)
all(us14_cdc$E_MINRTY == us14_dep$E_MINRTY, na.rm = TRUE)

# htt
all(us14_cdc$E_MUNIT == us14_dep$E_MUNIT, na.rm = TRUE)
all(us14_cdc$E_MOBILE == us14_dep$E_MOBILE, na.rm = TRUE)
all(us14_cdc$E_CROWD == us14_dep$E_CROWD, na.rm = TRUE)
all(us14_cdc$E_NOVEH == us14_dep$E_NOVEH, na.rm = TRUE)
all(us14_cdc$E_GROUPQ == us14_dep$E_GROUPQ, na.rm = TRUE)

# measures
all(us14_cdc$SVI_CDC == us14_dep$SVI, na.rm = TRUE)
all(us14_cdc$SVI_SES_CDC == us14_dep$SVI_SES, na.rm = TRUE)
all(us14_cdc$SVI_HCD_CDC == us14_dep$SVI_HCD, na.rm = TRUE)
all(us14_cdc$SVI_MSL_CDC == us14_dep$SVI_MSL, na.rm = TRUE)
all(us14_cdc$SVI_HTT_CDC == us14_dep$SVI_HTT, na.rm = TRUE)

x <- select(us14_dep, GEOID, SVI_dep = SVI)

us14_cdc %>%
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
