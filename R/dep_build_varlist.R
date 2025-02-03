#' Create Variable Lists
#'
#' @description This function creates a vector or \code{tibble} containing
#'     variables included in particular calls.
#'
#' @usage dep_build_varlist(geography, index, year, survey = "acs5", output = "vector")
#'
#' @param geography A character scalar; one of \code{"state"}, \code{"county"}, or
#'     \code{"tract"}
#' @param index A character scalar or vector listing deprivation measures
#'     to return. These include the area deprivation index (\code{"adi"}),
#'     the gini coefficient (\code{"gini"}), two versions of the neighborhood
#'     deprivation index by Messer (\code{"ndi_m"}) and Powell and Wiley
#'     (\code{"ndi_pw"}), and four versions of the social vulnerability
#'     index (\code{"svi10"}, \code{"svi14"}, \code{"svi20"}, and \code{"svi20s"}).
#' @param year A numeric scalar between 2010 and 2020
#' @param survey A character scalar representing the Census product. It can
#'     be any American Community Survey product (either \code{"acs1"},
#'     \code{"acs3"}, or \code{"acs5"}). Note that \code{"acs3"} was
#'     discontinued after 2013.
#' @param output A character scalar; either \code{"vector"} (default) or
#'     \code{tibble}. See Return below.
#'
#' @return A vector of variable names or a \code{tibble} containing both
#'     variable names, labels, and the measure(s) they are associated with.
#'
#' @examples
#' # Gini coefficient at the Census tract level
#' dep_build_varlist(geography = "tract", index = "gini", year = 2019)
#'
#' @export
dep_build_varlist <- function(geography, index, year, survey = "acs5", output = "vector"){

  # check inputs
  if (missing(geography) == TRUE) {
    stop("A level of geography must be provided. Please choose one of: 'county', 'zcta3', 'zcta5', or 'tract'.")
  }

  if (geography %in% c("county", "zcta3", "zcta5", "tract") == FALSE){
    stop("Invalid level of geography provided. Please choose one of: 'county', 'zcta3', 'zcta5', or 'tract'.")
  }

  if (missing(index) == TRUE){
    stop("A 'index' value must be provided. Please choose one of: 'adi', 'gini', 'ndi_m', 'ndi_pw', 'svi10', 'svi14', 'svi20', or 'svi20s'.")
  }

  if (all(index %in% c("adi", "gini", "ndi_m", "ndi_pw", "svi10", "svi14", "svi20", "svi20s")) == FALSE){
    stop("Invalid index provided. Please choose one of: 'adi', 'gini', 'ndi_m', 'ndi_pw', 'svi10', 'svi14', 'svi20', or 'svi20s'.")
  }

  if (missing(year) == TRUE){
    stop("A 'year' value must be provided. Please choose a numeric value between 2010 and 2022.")
  }

  if (is.numeric(year) == FALSE | (min(year) < 2010 | max(year) > 2022)){
    stop("The 'year' value provided is invalid. Please provide a numeric value between 2010 and 2022.")
  }

  if (any(index %in% c("svi14", "svi20")) == TRUE & min(year) < 2012){
    stop("The 'year' value provided is not valid for 2014 or 2020 SVI specifications. Each of those can be calculated from 2012 onward. Please use 'svi10' for 2010 or 2011.")
  }

  if (any(index == "svi20s") == TRUE & min(year) < 2019){
    stop("The 'year' value provided is not valid for the 2020 SVI specification with the alternate single parent measure. This can only be calculated for 2019 onward.")
  }

  if (survey %in% c("acs1", "acs3", "acs5") == FALSE){
    stop("The 'survey' value provided is not valid. Please choose one of 'acs1', 'acs3', or 'acs5'.")
  }

  if (survey == "acs3" & year > 2013){
    stop("The 'acs3' survey was discontinued after 2013. Please select one of 'acs1' or 'acs5'.")
  }

  if (output %in% c("vector", "tibble") == FALSE){
    stop("The 'output' value provided is not valid. Please choose one of 'vector' or 'tibble'.")
  }

  ## gini
  if ("gini" %in% index == TRUE){
    a <- request_vars$gini10
  } else {
    a <- NULL
  }

  ## svi
  ### 2010 svi style
  if ("svi10" %in% index == TRUE){
    if (year %in% c(2010:2014) == TRUE){
      b <- unlist(request_vars$svi10_10)
    } else if (year %in% c(2015, 2016) == TRUE){
      b <- unlist(request_vars$svi10_15)
    } else if (year > 2016){
      b <- unlist(request_vars$svi10_17)
    }
  }else {
    b <- NULL
  }

  ### 2014 svi style
  if ("svi14" %in% index == TRUE){
    if (year %in% c(2012:2014) == TRUE){
      c <- unlist(request_vars$svi14_12)
    } else if (year %in% c(2015, 2016) == TRUE){
      c <- unlist(request_vars$svi14_15)
    } else if (year > 2016){
      c <- unlist(request_vars$svi14_17)
    }
  } else {
    c <- NULL
  }

  ### 2020 svi style
  if ("svi20" %in% index == TRUE){
    if (year == 2012){
      d <- unlist(request_vars$svi20_12)
    } else if (year %in% c(2013, 2014)){
      d <- unlist(request_vars$svi20_13)
    } else if (year %in% c(2015, 2016) == TRUE){
      d <- unlist(request_vars$svi20_15)
    } else if (year > 2016){
      d <- unlist(request_vars$svi20_17)
    }
  } else {
    d <- NULL
  }

  ### 2020 svi style with alt definition of single parent households
  if ("svi20s" %in% index == TRUE){
    e <- unlist(request_vars$svi20s_19)
  } else {
    e <- NULL
  }

  ## adi
  if ("adi" %in% index == TRUE){
    f <- build_adi_varlist(geography, year = year, survey = survey)
  } else {
    f <- NULL
  }

  ## ndi, messer
  if ("ndi_m" %in% index == TRUE){
    g <- request_vars$ndi_m
  } else {
    g <- NULL
  }

  ## ndi, powell-wiley
  if ("ndi_pw" %in% index == TRUE){
    h <- request_vars$ndi_pw
  } else {
    h <- NULL
  }

  ## create output
  ### create initial vector
  out <- sort(unique(c(a,b,c,d,e,f,g,h)))

  ### optionally create output tibble
  if (output == "tibble"){

    #### convert to tibble
    out <- dplyr::tibble(
      name = out
    )

    #### download variable defs
    if (index %in% c("adi", "gini") == TRUE){
      vars <- tidycensus::load_variables(
        year = year,
        dataset = survey
      )
    } else if (index == "ndi_m") {

      vars_a <- tidycensus::load_variables(
        year = year,
        dataset = survey
      )
      vars_a <- vars_a[, -which(names(vars_a) == "geography")]

      vars_b <- tidycensus::load_variables(
        year = year,
        dataset = paste0(survey, "/profile")
      )

      vars <- rbind(vars_a, vars_b)

    } else {

      vars_a <- tidycensus::load_variables(
        year = year,
        dataset = survey
      )
      vars_a <- vars_a[, -which(names(vars_a) == "geography")]

      vars_b <- tidycensus::load_variables(
        year = year,
        dataset = paste0(survey, "/profile")
      )

      vars_c <- tidycensus::load_variables(
        year = year,
        dataset = paste0(survey, "/subject")
      )

      vars <- rbind(vars_a, vars_b, vars_c)

    }

    vars <- dplyr::select(vars, name, label, concept)

    #### create output
    out <- dplyr::left_join(out, vars, by = "name")

  }

  ## return output
  return(out)

}

# sub function for ADI specific variable lists
build_adi_varlist <- function(geography, year, survey){

  ## create data object
  out <- sociome::acs_vars

  ## subset based on year+survey combination
  if (year == 2010){

    if (survey == "acs5"){
      out <- subset(out, out$set5 == TRUE)
      out <- out[, c("variable", "description", "set5")]
      names(out)[names(out) == "set5"] <- "adi"
    } else if (survey %in% c("acs1", "acs3")){
      out <- subset(out, out$set4 == TRUE)
      out <- out[, c("variable", "description", "set4")]
      names(out)[names(out) == "set4"] <- "adi"
    }

  } else if (year == 2011){

    if (survey == "acs5"){
      out <- subset(out, out$set3 == TRUE)
      out <- out[, c("variable", "description", "set3")]
      names(out)[names(out) == "set3"] <- "adi"
    } else if (survey %in% c("acs1", "acs3")){
      out <- subset(out, out$set1 == TRUE)
      out <- out[, c("variable", "description", "set1")]
      names(out)[names(out) == "set1"] <- "adi"
    }

  } else if (year %in% c(2012:2014)){
    out <- subset(out, out$set1 == TRUE)
    out <- out[, c("variable", "description", "set1")]
    names(out)[names(out) == "set1"] <- "adi"
  } else if (year %in% c(2015:2016)){

    if (geography == "block group"){
      out <- subset(out, out$set2 == TRUE)
      out <- out[, c("variable", "description", "set2")]
      names(out)[names(out) == "set2"] <- "adi"
    } else {
      out <- subset(out, out$set1 == TRUE)
      out <- out[, c("variable", "description", "set1")]
      names(out)[names(out) == "set1"] <- "adi"
    }

  } else if (year %in% c(2016:2021)){
    out <- subset(out, out$set1 == TRUE)
    out <- out[, c("variable", "description", "set1")]
    names(out)[names(out) == "set1"] <- "adi"
  }

  ## create output
  out <- out$variable

  ## return output
  return(out)

}

# sub function for adding appropriate suffix values
dep_expand_varlist <- function(geography, index, year, survey,
                               estimates_only = FALSE, moe_only = FALSE){

  # create initial variable list
  if (index %in% c("gini", "adi", "svi10", "svi14", "svi20", "svi20s", "ndi_m", "ndi_pw")){

    out <- dep_build_varlist(geography = geography, index = index,
                             year = year, survey = survey)

  } else if (index == "svi10, msl all") {

    if (year %in% c(2010:2014) == TRUE){
      out <- c(request_vars$svi10_10$msl_vars, request_vars$svi10_10$eng_vars)
    } else if (year %in% c(2015, 2016) == TRUE){
      out <- c(request_vars$svi10_15$msl_vars, request_vars$svi10_15$eng_vars)
    } else if (year %in% c(2017:2022) == TRUE){
      out <- c(request_vars$svi10_17$msl_vars, request_vars$svi10_17$eng_vars)
    }

  } else if (index == "svi14, msl all") {

    if (year %in% c(2012:2014) == TRUE){
      out <- c(request_vars$svi14_12$msl_vars, request_vars$svi14_12$eng_vars)
    } else if (year %in% c(2015, 2016) == TRUE){
      out <- c(request_vars$svi14_15$msl_vars, request_vars$svi14_15$eng_vars)
    } else if (year %in% c(2017:2022) == TRUE){
      out <- c(request_vars$svi14_17$msl_vars, request_vars$svi14_17$eng_vars)
    }

  } else if (index == "svi10, hhd all") {

    if (year %in% c(2010:2014) == TRUE){
      out <- c(request_vars$svi10_10$hhd_vars, request_vars$svi10_10$age_lt18_vars,
               request_vars$svi10_10$age_gt64_vars)
    } else if (year %in% c(2015, 2016) == TRUE){
      out <- c(request_vars$svi10_15$hhd_vars, request_vars$svi10_15$age_lt18_vars,
               request_vars$svi10_15$age_gt64_vars)
    }

  } else if (index == "svi14, hhd all") {

    if (year %in% c(2012:2014) == TRUE){
      out <- c(request_vars$svi14_12$hhd_vars, request_vars$svi14_12$age_lt18_vars,
               request_vars$svi14_12$age_gt64_vars, request_vars$svi14_12$dis_vars)
    } else if (year %in% c(2015, 2016) == TRUE){
      out <- c(request_vars$svi14_15$hhd_vars, request_vars$svi14_15$age_lt18_vars,
               request_vars$svi14_15$age_gt64_vars, request_vars$svi14_15$dis_vars)
    } else if (year %in% c(2017:2022) == TRUE){
      out <- c(request_vars$svi14_17$hhd_vars, request_vars$svi14_17$dis_vars)
    }

  } else if (index == "svi20, hhd all") {

    if (year == 2012){
      out <- c(request_vars$svi20_12$hhd_vars, request_vars$svi20_12$age_lt18_vars,
               request_vars$svi20_12$age_gt64_vars, request_vars$svi20_12$dis_vars,
               request_vars$svi20_12$eng_vars)
    } else if (year %in% c(2013, 2014) == TRUE){
      out <- c(request_vars$svi20_13$hhd_vars, request_vars$svi20_13$age_lt18_vars,
               request_vars$svi20_13$age_gt64_vars, request_vars$svi20_13$dis_vars,
               request_vars$svi20_13$eng_vars)
    } else if (year %in% c(2015, 2016) == TRUE){
      out <- c(request_vars$svi20_15$hhd_vars, request_vars$svi20_15$age_lt18_vars,
               request_vars$svi20_15$age_gt64_vars, request_vars$svi20_15$dis_vars,
               request_vars$svi20_15$eng_vars)
    } else if (year %in% c(2017:2022) == TRUE){
      out <- c(request_vars$svi20_17$hhd_vars, request_vars$svi20_17$dis_vars,
               request_vars$svi20_17$eng_vars)
    }

  } else if (index == "svi10, ses all"){

    if (year < 2015){
      out <- c(request_vars$svi10_10$ses_vars, request_vars$svi10_10$edu_vars)
    }

  } else if (index == "svi14, ses all"){

    if (year < 2015){
      out <- c(request_vars$svi14_12$ses_vars, request_vars$svi14_12$edu_vars)
    }

  } else if (index == "svi20, ses all"){

    if (year == 2012){
      out <- c(request_vars$svi20_12$ses_vars, request_vars$svi20_12$edu_vars)
    } else if (year %in% c(2013, 2014) == TRUE){
      out <- c(request_vars$svi20_13$ses_vars, request_vars$svi20_13$edu_vars)
    }

  } else if (index == "svi20s, hhd all"){

    out <- c(request_vars$svi20s_19$hhd_vars, request_vars$svi20s_19$dis_vars,
             request_vars$svi20s_19$eng_vars)

  } else if (index %in% c("svi10, pri", "svi14, pri", "svi20, pri", "svi20s, pri",
                          "svi10, ses", "svi14, ses", "svi20, ses", "svi20s, ses",
                          "svi10, edu", "svi14, edu", "svi20, edu",
                          "svi10, hhd", "svi14, hhd", "svi20, hhd", "svi20s, hhd",
                          "svi10, age_lt18", "svi10, age_gt64", "svi14, age_lt18", "svi14, age_gt64", "svi20, age_lt18", "svi20, age_gt64",
                          "svi14, dis", "svi20, dis", "svi20s, dis",
                          "svi10, msl", "svi14, msl", "svi20, msl", "svi20s, msl",
                          "svi10, eng", "svi14, eng", "svi20, eng", "svi20s, eng",
                          "svi10, htt", "svi14, htt", "svi20, htt", "svi20s, htt") == TRUE){

    req <- paste0(stringr::word(index, 2),"_vars")

    if (stringr::word(index, 1) == "svi10,"){

      if (year %in% c(2010:2014) == TRUE){
        out <- request_vars$svi10_10[[req]]
      } else if (year %in% c(2015, 2016) == TRUE){
        out <- request_vars$svi10_15[[req]]
      } else if (year %in% c(2017:2022) == TRUE){
        out <- request_vars$svi10_17[[req]]
      }

    } else if (stringr::word(index, 1) == "svi14,"){

      if (year %in% c(2012:2014) == TRUE){
        out <- request_vars$svi14_12[[req]]
      } else if (year %in% c(2015, 2016) == TRUE){
        out <- request_vars$svi14_15[[req]]
      } else if (year %in% c(2017:2022) == TRUE){
        out <- request_vars$svi14_17[[req]]
      }

    } else if (stringr::word(index, 1) == "svi20,"){

      if (year == 2012){
        out <- request_vars$svi20_12[[req]]
      } else if (year %in% c(2013, 2014) == TRUE){
        out <- request_vars$svi20_13[[req]]
      } else if (year %in% c(2015, 2016) == TRUE){
        out <- request_vars$svi20_15[[req]]
      } else if (year %in% c(2017:2022) == TRUE){
        out <- request_vars$svi20_17[[req]]
      }
    } else if (stringr::word(index, 1) == "svi20s,"){
      out <- request_vars$svi20s_19[[req]]
    }
  }

  # expand and combine
  if (estimates_only == FALSE & moe_only == FALSE){
    out <- sort(c(paste0(out, "E"), paste0(out, "M")))
  } else if (estimates_only == TRUE & moe_only == FALSE){
    out <- sort(paste0(out, "E"))
  } else if (estimates_only == FALSE & moe_only == TRUE){
    out <- sort(paste0(out, "M"))
  }

  # return
  return(out)

}

# sub function for zcta3 aggregation
dep_zcta3_varlist <- function(varlist){

  ## intensive variables in scales
  intensive_vars <- c("B19083_001", # gini coefficient
                      "B06011_001", # median income
                      "B19301_001", # per capita income
                      "B19013_001", # median household income
                      "B19113_001", # median family income
                      "B25064_001", # median gross rent
                      "B25077_001", # median value of owner-occupied housing
                      "B25088_002") # median monthly costs for housing units w/ mortgage

  ## create output
  out <- list(
    extensive = varlist[varlist %in% intensive_vars == FALSE],
    intensive = varlist[varlist %in% intensive_vars == TRUE]
  )

  ## return output
  return(out)

}


dep_build_multi_varlist <- function(geography, index, year, survey){

  ## iterate over index values
  out <- lapply(index, function(index_vec) {
    dep_expand_varlist(geography = geography, index = index_vec, year = year, survey = survey)
  })

  ## limit output to unique column names
  out <- sort(unique(unlist(out)))

  ## return output
  return(out)

}

