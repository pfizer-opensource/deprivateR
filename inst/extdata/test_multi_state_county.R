devtools::load_all()

out <- tidycensus::get_acs(geography = "county subdivision", variables = "B01001_001",
                           state = "MO", output = "wide", geometry = TRUE)

stl_metro <- c("17005", "17013", "17027", "17083", "17117",
               "17119", "17133", "17163", "29071", "29099",
               "29113", "29183", "29189", "29219", "29510")

out <- dep_get_index(geography = "county", index = "svi10", year = 2010)
out <- dep_get_index(geography = "county", index = "svi10", year = 2011)
out <- dep_get_index(geography = "county", index = "svi10", year = 2018)


out <- dep_get_index(geography = "county", index = "svi14", year = 2018)
out_pr <- dep_get_index(geography = "county", index = "svi14", year = 2018,
                        puerto_rico = TRUE)

out <- dep_get_index(geography = "county", index = c("adi", "svi14"), year = c(2016:2018),
                     state = c("IL", "MO"))

out <- dep_get_index(geography = "county", index = "svi14", year = 2018,
                     county = stl_metro)

out <- dep_get_index(geography = "tract", index = "svi14", year = 2018)
out_pr <- dep_get_index(geography = "tract", index = "svi14", year = 2018,
                        puerto_rico = TRUE)

out <- dep_get_index(geography = "tract", index = "svi14", year = 2018)

out_metro <- dep_get_index(geography = "tract", index = "svi14", year = 2018,
                     county = stl_metro)

test <- purrr::map_df(.x = out, .f = ~tidycensus::get_acs(geography = "tract",
                                                  variables = "B01001_001",
                                                  output = "wide",
                                                  state = .x))
