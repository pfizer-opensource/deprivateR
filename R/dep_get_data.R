# Create Demographic Data
#
# This is an internal function that returns a list with two objects, one
# that corresponds to geometric data (or simply lists GEOID and NAME) and
# one that contains all of the demographic data.
#
dep_get_data <- function(geography, varlist, year, survey, state, county,
                         puerto_rico, zcta, zcta3_method, zcta_geo_method,
                         geometry, cb, keep_geo_vars,
                         shift_geo, key, debug){

  ## pull data based on geography type
  if (geography == "zcta5") {
    return(dep_get_zcta5(varlist = varlist, year = year, survey = survey,
                         state = state, county = county, puerto_rico = puerto_rico,
                         zcta = zcta, zcta_geo_method = zcta_geo_method, geometry = geometry,
                         cb = cb, keep_geo_vars = keep_geo_vars,
                         shift_geo = shift_geo, key = key, debug = debug))
  }

  if (geography == "zcta3") {
    return(dep_get_zcta3(varlist = varlist, year = year, survey = survey,
                         state = state, county = county, puerto_rico = puerto_rico,
                         zcta = zcta, zcta3_method = zcta3_method, geometry = geometry,
                         shift_geo = shift_geo, key = key, debug = debug))
  }

  ## county/tract geography
  out <- dep_get_county_tract(geography = geography, varlist = varlist,
                              year = year, survey = survey,
                              state = state, county = county,
                              puerto_rico = puerto_rico,
                              geometry = geometry, keep_geo_vars = keep_geo_vars,
                              key = key, debug = debug)

  ## finalize: split into geo and demo
  out <- dep_finalize_output(out, varlist = varlist, geometry = geometry,
                             keep_geo_vars = keep_geo_vars, shift_geo = shift_geo)

  return(out)
}

# Helper: retrieve county or tract data with flattened logic
dep_get_county_tract <- function(geography, varlist, year, survey,
                                 state, county, puerto_rico,
                                 geometry, keep_geo_vars, key, debug) {

  # when county is specified, iterate over unique states in county FIPS

  if (!is.null(county)) {
    states <- unique(substr(county, 1, 2))
    out <- lapply(states, function(states_vec) {
      dep_get_census(geography = geography, varlist = varlist,
                     year = year, survey = survey,
                     state = states_vec, county = county,
                     geometry = geometry, keep_geo_vars = keep_geo_vars,
                     key = key, debug = debug)
    })
    return(do.call(rbind, out))
  }

  # county geography without county filter
  if (geography == "county") {
    out <- dep_get_census(geography = geography, varlist = varlist,
                          year = year, survey = survey,
                          state = state, county = county,
                          geometry = geometry,
                          keep_geo_vars = keep_geo_vars,
                          key = key, debug = debug)

    territory_vec <- dep_territory_fips(puerto_rico)
    out <- subset(out, !substr(GEOID, 1, 2) %in% territory_vec)
    return(out)
  }

  # tract geography without county filter
  states <- dep_build_states(state = state, puerto_rico = puerto_rico)

  out <- lapply(states, function(states_vec) {
    dep_get_census(geography = geography, varlist = varlist,
                   year = year, survey = survey,
                   state = states_vec, county = county,
                   geometry = geometry, keep_geo_vars = keep_geo_vars,
                   key = key, debug = debug)
  })

  do.call(rbind, out)
}

# Helper: territory FIPS codes to exclude
dep_territory_fips <- function(puerto_rico) {
  if (puerto_rico) {
    c("60", "66", "69", "78")
  } else {
    c("60", "66", "69", "72", "78")
  }
}

# Helper: build state vector for tract iteration
dep_build_states <- function(state, puerto_rico) {
  base <- if (is.null(state)) state.abb else state
  if (puerto_rico) c(base, "PR") else base
}

# Helper: split raw output into geo/demo list
dep_finalize_output <- function(out, varlist, geometry, keep_geo_vars, shift_geo) {
  if (geometry) {
    if (keep_geo_vars) {
      geo <- subset(out, select = -c(varlist))
      demo <- data.frame(rep(NA, nrow(out)))
    } else {
      geo <- subset(out, select = c(GEOID, NAME))
      demo <- subset(out, select = -NAME)
      sf::st_geometry(demo) <- NULL
    }

    if (shift_geo) {
      geo <- tigris::shift_geometry(geo, position = "below")
    }
  } else {
    geo <- subset(out, select = c(GEOID, NAME))
    demo <- subset(out, select = -NAME)
  }

  list(geo = geo, demo = demo)
}


## sub functions
dep_get_zcta5 <- function(varlist, year, survey, state, county,
                          puerto_rico, zcta, geometry, cb, zcta_geo_method,
                          keep_geo_vars, shift_geo, key, debug){

  ## call zippeR for demographics
  if (debug == "live"){
    demo <- zippeR::zi_get_demographics(year = year,
                                        variables = varlist,
                                        survey = survey,
                                        output = "wide",
                                        key = key)
  } else if (debug %in% c("messages", "call")){
    demo <- zippeR::zi_get_demographics(year = year,
                                        variables = varlist,
                                        survey = survey,
                                        output = "wide",
                                        key = key,
                                        debug = debug)
  } else if (debug == "test"){
    cli::cli_abort("Testing debug mode not enabled yet.")
  }

  ## optionally filter based on zcta
  if (!is.null(zcta)){
    demo <- subset(demo, GEOID %in% zcta)
  }

  ## manage territories
  ### set territory vector
  if (!puerto_rico){
    territory_vec <- c("006", "007", "008", "009", "969")
  } else if (puerto_rico){
    territory_vec <- c("008", "969")
  }

  ### all territories not including American Samoa
  demo <- subset(demo, !(substr(GEOID, 1,3) %in% territory_vec))

  ### American Samoa
  demo <- subset(demo, GEOID != "96799")

  ## set year since 2020 uses the older geometries
  if (year == 2020){
    year_revise <- 2019
  } else if (year < 2020){
    year_revise <- year
  }

  ## optionally add geometry
  if (geometry){

    ## set return
    # if (keep_geo_vars == TRUE){
    #  return_type <- "full"
    # } else if (keep_geo_vars == FALSE){
    #  return_type <- "id"
    # }

    return_type <- "id"

    ## set PR flag
    if (!puerto_rico){
      pr <- NULL
    } else if (puerto_rico){
      pr <- "PR"
    }

    ## call zippeR
    geo <- zippeR::zi_get_geometry(year = year_revise,
                                   style = "zcta5",
                                   return = return_type,
                                   class = "sf",
                                   state = state,
                                   county = county,
                                   includes = zcta,
                                   method = zcta_geo_method,
                                   cb = cb,
                                   shift_geo = shift_geo)

    ## optionally filter based on zcta
    if (!is.null(zcta)){
      geo <- subset(geo, GEOID %in% zcta)
    }

  } else if (!geometry){
    geo <- dplyr::as_tibble(data.frame(GEOID = demo$GEOID))
  }

  ## construct output
  out <- list(
    geo = geo,
    demo = demo
  )

  ## return output
  return(out)

}

dep_get_zcta3 <- function(varlist, year, survey, state, county,
                          puerto_rico, zcta, zcta3_method, geometry,
                          shift_geo, key, debug){

  ## call zippeR for demographics
  if (debug == "live"){
    demo <- zippeR::zi_get_demographics(year = year,
                                        variables = varlist,
                                        survey = survey,
                                        output = "tidy",
                                        key = key)
  } else if (debug %in% c("messages", "call")){
    demo <- zippeR::zi_get_demographics(year = year,
                                        variables = varlist,
                                        survey = survey,
                                        output = "tidy",
                                        key = key,
                                        debug = debug)
  } else if (debug == "test"){
    cli::cli_abort("Testing debug mode not enabled yet.")
  }

  ## optionally filter based on zcta
  if (!is.null(zcta)){
    demo <- subset(demo, GEOID %in% zcta)
  }

  ## manage territories
  ### set territory vector
  if (!puerto_rico){
    territory_vec <- c("006", "007", "008", "009", "969")
  } else if (puerto_rico){
    territory_vec <- c("008", "969")
  }

  ### all territories not including American Samoa
  demo <- subset(demo, !(substr(GEOID, 1,3) %in% territory_vec))

  ### American Samoa
  demo <- subset(demo, GEOID != "96799")

  ## aggregate
  ### make lists of variables
  varlist_mod <- dep_zcta3_varlist(varlist = varlist)

  ### aggregate
  if (debug == "live"){
    demo <- zippeR::zi_aggregate(demo, year = year,
                                 extensive = varlist_mod$extensive,
                                 intensive = varlist_mod$intensive,
                                 intensive_method = zcta3_method,
                                 survey = survey, output = "wide",
                                 zcta = zcta, key = key)
  } else if (debug %in% c("messages", "call")){
    demo <- zippeR::zi_aggregate(demo, year = year,
                                 extensive = varlist_mod$extensive,
                                 intensive = varlist_mod$intensive,
                                 intensive_method = zcta3_method,
                                 survey = survey, output = "wide",
                                 zcta = zcta, key = key,
                                 debug = debug)
  } else if (debug == "test"){
    cli::cli_abort("Testing debug mode not enabled yet.")
  }

  ### rename id
  #demo <- dplyr::rename(demo, GEOID = ZCTA3)
  names(demo)[names(demo) == "ZCTA3"] <- "GEOID"

  ## set year since 2020 uses the older geometries
  if (year == 2020){
    year_revise <- 2019
  } else if (year < 2020){
    year_revise <- year
  }

  ## optionally add geometry
  if (geometry){

    ## set PR flag
    if (!puerto_rico){
      pr <- NULL
    } else if (puerto_rico){
      pr <- "PR"
    }

    ## call zippeR
    geo <- zippeR::zi_get_geometry(year = year_revise,
                                   style = "zcta3",
                                   return = "id",
                                   class = "sf",
                                   territory = pr,
                                   shift_geo = shift_geo)

    ### rename id
    names(demo)[names(demo) == "ZCTA3"] <- "GEOID"

    ## optionally filter based on zcta
    if (!is.null(zcta)){
      geo <- subset(geo, GEOID %in% zcta)
    }

  } else if (!geometry){
    geo <- dplyr::as_tibble(data.frame(GEOID = demo$GEOID))
  }

  ## construct output
  out <- list(
    geo = geo,
    demo = demo
  )

  ## return output
  return(out)

}

dep_get_census <- function(geography, varlist, year, survey, state, county,
                           geometry, cb, keep_geo_vars,
                           shift_geo, key, debug){

  ## subset county to only counties that match state fips
  if (!is.null(county)){

    ## isolate state
    county <- county[substr(county, 1, 2) == state]

    ## remove state
    county <- substr(county, 3, 5)

  }

  ## call tidycensus
  if (debug == "live"){
    out <- suppressMessages(tidycensus::get_acs(geography = geography,
                                                state = state,
                                                county = county,
                                                variables = varlist,
                                                output = "wide",
                                                year = year,
                                                survey = survey,
                                                geometry = geometry,
                                                keep_geo_vars = keep_geo_vars,
                                                key = key))
  } else if (debug == "messages"){
    out <- tidycensus::get_acs(geography = geography,
                               state = state,
                               county = county,
                               variables = varlist,
                               output = "wide",
                               year = year,
                               survey = survey,
                               geometry = geometry,
                               keep_geo_vars = keep_geo_vars,
                               key = key)
  } else if (debug == "call"){
    out <- tidycensus::get_acs(geography = geography,
                               state = state,
                               county = county,
                               variables = varlist,
                               output = "wide",
                               year = year,
                               survey = survey,
                               geometry = geometry,
                               keep_geo_vars = keep_geo_vars,
                               show_call = TRUE,
                               key = key)
  } else if (debug == "test"){
    cli::cli_abort("Testing debug mode not enabled yet.")
  }

  ## return output
  return(out)

}

