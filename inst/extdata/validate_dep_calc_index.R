devtools::load_all()

library(dplyr)

vars1 <- dep_build_varlist(geography = "county", index = c("svi14", "svi20"), year = 2012, survey = "acs5")
vars2 <- dep_build_varlist(geography = "county", index = c("svi14", "svi20"), year = 2013, survey = "acs5")
vars3 <- dep_expand_varlist(geography = "county", index = "svi20", year = 2013, survey = "acs5")

df1 <- tidycensus::get_acs(geography = "county", variables = vars1, year = 2012, output = "wide") %>%
  mutate(YEAR = 2012, .after = "GEOID")
df2 <- tidycensus::get_acs(geography = "county", variables = vars2, year = 2013, output = "wide") %>%
  mutate(YEAR = 2013, .after = "GEOID")

df <- bind_rows(df1, df2)
rm(df1, df2)

df$score <- runif(nrow(df), min = 0, max = 1000)

dep_calc_index(df, geography = "county", index = "svi14", year = c(2012, 2013))
dep_calc_index(df, geography = "county", index = c("svi14", "svi20"), year = c(2012, 2013))

df <- select(df, -NAME)

dep_calc_index(df, geography = "county", index = "svi14", year = c(2012, 2013))
