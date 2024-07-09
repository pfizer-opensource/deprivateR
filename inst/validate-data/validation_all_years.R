devtools::load_all()

create_list <- function(index, year){

  out <- dep_get_index(geography = "county", index = index, year = year, state = "MO",
                       keep_subscales = TRUE, keep_components = TRUE)
  out <- dplyr::mutate(out, YEAR = year, .before = "GEOID")
  out <- dplyr::select(out, YEAR, GEOID, dplyr::starts_with("E_"))

  return(out)

}

svi10 <- purrr::map(.x = c(2010:2021), .f = ~create_list(index = "svi10", year = .x))
svi14 <- purrr::map(.x = c(2012:2021), .f = ~create_list(index = "svi14", year = .x))
svi20 <- purrr::map(.x = c(2012:2021), .f = ~create_list(index = "svi20", year = .x))
svi20s <- purrr::map(.x = c(2019:2021), .f = ~create_list(index = "svi20s", year = .x))
