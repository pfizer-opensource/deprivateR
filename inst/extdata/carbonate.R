library(deprivateR)

## gini coefficients at the ZCTA level
gini <- dep_get_index(geography = "zcta5", index = "gini", year = 2020,
                      territory = c("AS", "GU", "MP", "PR", "VI"))

## svi and adi at the county level
county <- dep_get_index(geography = "county", index = c("svi", "adi"),
                        year = 2020, territory = NULL,
                        output = "sf", cb = TRUE, shift_geo = TRUE)


library(carbonate)
x <- carbon$new()
x$template <-'cobalt'
x$browse()
