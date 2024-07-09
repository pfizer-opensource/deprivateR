# Validation of deprivateR Against 2016 County-level Data

# dependencies ####
devtools::load_all()

library(dplyr)
library(readr)

# load data ####
## pull data with deprivateR
mo18_dep <- dep_get_index(geography = "county", index = "svi14", year = 2018,
                          state = "MO", keep_subscales = TRUE, keep_components = TRUE)

us18_dep <- dep_get_index(geography = "county", index = "svi14", year = 2018,
                          keep_subscales = TRUE, keep_components = TRUE,
                          puerto_rico = FALSE)

us18_dep <- select(us18_dep, GEOID, E_TOTPOP, E_HU, E_HH, starts_with("D_"))

pr18_dep <- dep_get_index(geography = "county", index = "svi14", year = 2018,
                          puerto_rico = TRUE, keep_subscales = TRUE,
                          keep_components = TRUE)

mo18_cdc <- read_csv("data-raw/cdc_missouri_svi_county_2018.csv") %>%
  rename(GEOID = FIPS, SVI_CDC = RPL_THEMES, SVI_SES_CDC = RPL_THEME1,
         SVI_HCD_CDC = RPL_THEME2, SVI_MSL_CDC = RPL_THEME3,
         SVI_HTT_CDC = RPL_THEME4) %>%
  mutate(GEOID = as.character(GEOID)) %>%
  arrange(GEOID)

us18_cdc <- read_csv("data-raw/cdc_us_svi_county_2018.csv") %>%
  rename(GEOID = FIPS, SVI_CDC = RPL_THEMES, SVI_SES_CDC = RPL_THEME1,
         SVI_HCD_CDC = RPL_THEME2, SVI_MSL_CDC = RPL_THEME3,
         SVI_HTT_CDC = RPL_THEME4) %>%
  arrange(GEOID)

# missouri only ####

## primary
all(mo18_cdc$E_TOTPOP == mo18_dep$E_TOTPOP)
all(mo18_cdc$E_HU == mo18_dep$E_HU)
all(mo18_cdc$E_HH == mo18_dep$E_HH)

## ses
all(mo18_cdc$E_POV == mo18_dep$E_POV, na.rm = TRUE)
all(mo18_cdc$E_UNEMP == mo18_dep$E_UNEMP, na.rm = TRUE)
all(mo18_cdc$E_PCI == mo18_dep$E_PCI, na.rm = TRUE)
all(mo18_cdc$E_NOHSDP == mo18_dep$E_NOHSDP, na.rm = TRUE)

## hcd
all(mo18_cdc$E_AGE17 == mo18_dep$E_AGE17, na.rm = TRUE)
all(mo18_cdc$E_AGE65 == mo18_dep$E_AGE65, na.rm = TRUE)
all(mo18_cdc$E_DISABL == mo18_dep$E_DISABL, na.rm = TRUE)
all(mo18_cdc$E_SNGPNT == mo18_dep$E_SNGPNT, na.rm = TRUE)

# msl
all(mo18_cdc$E_LIMENG == mo18_dep$E_LIMENG, na.rm = TRUE)
all(mo18_cdc$E_MINRTY == mo18_dep$E_MINRTY, na.rm = TRUE)

# htt
all(mo18_cdc$E_MUNIT == mo18_dep$E_MUNIT, na.rm = TRUE)
all(mo18_cdc$E_MOBILE == mo18_dep$E_MOBILE, na.rm = TRUE)
all(mo18_cdc$E_CROWD == mo18_dep$E_CROWD, na.rm = TRUE)
all(mo18_cdc$E_NOVEH == mo18_dep$E_NOVEH, na.rm = TRUE)
all(mo18_cdc$E_GROUPQ == mo18_dep$E_GROUPQ, na.rm = TRUE)

# measures
all(mo18_cdc$SVI_CDC == mo18_dep$SVI, na.rm = TRUE)
all(mo18_cdc$SVI_SES_CDC == mo18_dep$SVI_SES, na.rm = TRUE)
all(mo18_cdc$SVI_HCD_CDC == mo18_dep$SVI_HCD, na.rm = TRUE)
all(mo18_cdc$SVI_MSL_CDC == mo18_dep$SVI_MSL, na.rm = TRUE)
all(mo18_cdc$SVI_HTT_CDC == mo18_dep$SVI_HTT, na.rm = TRUE)

x <- select(mo18_dep, GEOID, SVI_dep = SVI)

mo18_cdc %>%
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
all(us18_cdc$E_TOTPOP == us18_dep$E_TOTPOP)
all(us18_cdc$E_HU == us18_dep$E_HU)
all(us18_cdc$E_HH == us18_dep$E_HH)

## ses
all(us18_cdc$E_POV == us18_dep$E_POV, na.rm = TRUE)
all(us18_cdc$E_UNEMP == us18_dep$E_UNEMP, na.rm = TRUE)
all(us18_cdc$E_PCI == us18_dep$E_PCI, na.rm = TRUE)
all(us18_cdc$E_NOHSDP == us18_dep$E_NOHSDP, na.rm = TRUE)

## hcd
all(us18_cdc$E_AGE17 == us18_dep$E_AGE17, na.rm = TRUE)
all(us18_cdc$E_AGE65 == us18_dep$E_AGE65, na.rm = TRUE)
all(us18_cdc$E_DISABL == us18_dep$E_DISABL, na.rm = TRUE)
all(us18_cdc$E_SNGPNT == us18_dep$E_SNGPNT, na.rm = TRUE)

# msl
all(us18_cdc$E_LIMENG == us18_dep$E_LIMENG, na.rm = TRUE)
all(us18_cdc$E_MINRTY == us18_dep$E_MINRTY, na.rm = TRUE)

# htt
all(us18_cdc$E_MUNIT == us18_dep$E_MUNIT, na.rm = TRUE)
all(us18_cdc$E_MOBILE == us18_dep$E_MOBILE, na.rm = TRUE)
all(us18_cdc$E_CROWD == us18_dep$E_CROWD, na.rm = TRUE)
all(us18_cdc$E_NOVEH == us18_dep$E_NOVEH, na.rm = TRUE)
all(us18_cdc$E_GROUPQ == us18_dep$E_GROUPQ, na.rm = TRUE)

# measures
all(us18_cdc$SVI_CDC == us18_dep$SVI, na.rm = TRUE)
all(us18_cdc$SVI_SES_CDC == us18_dep$SVI_SES, na.rm = TRUE)
all(us18_cdc$SVI_HCD_CDC == us18_dep$SVI_HCD, na.rm = TRUE)
all(us18_cdc$SVI_MSL_CDC == us18_dep$SVI_MSL, na.rm = TRUE)
all(us18_cdc$SVI_HTT_CDC == us18_dep$SVI_HTT, na.rm = TRUE)

x <- select(us18_dep, GEOID, SVI_dep = SVI)

us18_cdc %>%
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

## alt
us18_dep <- dep_get_index(geography = "county", index = "svi14", year = 2018,
                          keep_subscales = FALSE, keep_components = FALSE,
                          puerto_rico = FALSE)

us18_cdc <- read_csv("data-raw/cdc_us_svi_county_2018.csv") %>%
  select(GEOID = FIPS, SVI_CDC = RPL_THEMES) %>%
  arrange(GEOID)

us18 <- us18_dep %>%
  rename(SVI_dep = SVI) %>%
  left_join(., us18_cdc, by = "GEOID") %>%
  mutate(
    SVI_dep_round = round(SVI_dep, digits = 4),
    SVI_match = ifelse(SVI_dep_round == SVI_CDC, TRUE, FALSE),
    SVI_delta = ifelse(SVI_match == FALSE, SVI_CDC - SVI_dep_round, NA)
  )
