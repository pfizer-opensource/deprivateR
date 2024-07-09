devtools::load_all()

out <- dep_get_index(geography = "county", index = "svi14", year = 2018,
                     output = "sf")

out <- dep_get_index(geography = "county", index = "svi14", year = 2018,
                     output = "sf", shift_geo = TRUE)

out <- dep_get_index(geography = "county", index = "svi14", year = 2018,
                     output = "sf", shift_geo = TRUE, puerto_rico = TRUE)

out <- dep_get_index(geography = "county", index = "svi14", year = 2018,
                     output = "sf", puerto_rico = TRUE)

out <- dep_get_index(geography = "county", index = c("adi", "svi14"), year = 2018,
                     state = c("IL", "MO"), output = "sf")

stl_metro <- c("17005", "17013", "17027", "17083", "17117",
               "17119", "17133", "17163", "29071", "29099",
               "29113", "29183", "29189", "29219", "29510")

out <- dep_get_index(geography = "county", index = "svi14", year = 2018,
                     county = stl_metro, output = "sf")

out <- dep_get_index(geography = "county", index = "svi14", year = 2018,
                     county = stl_metro, output = "sf")

out <- dep_get_index(geography = "zcta5", index = "svi14", year = 2018,
                     state = "VT", output = "sf", zcta_cb = TRUE)

out <- dep_get_index(geography = "zcta5", index = "svi14", year = 2018,
                     state = "VT", zcta_geo_method = "centroid", output = "sf")
