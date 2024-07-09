#' Perform Deprivation Calculations
#'
#' @description Calculates various measures of deprivation on data you have.
#'     Data cannot be automatically downloaded with this option, and the
#'     output options are more limited. See Details under \code{dep_get_index} for
#'     more information. For information about structuring your data prior to
#'     using this function, see Details below.
#'
#' @usage dep_calc_index(.data, geography, index, year, survey = "acs5",
#'     return_percentiles = FALSE, keep_subscales = FALSE, keep_components = FALSE,
#'     output = "wide")
#'
#' @param .data A data frame, tibble, or \code{sf} object that contains all
#'     of the columns needed to calculate your selected deprivation measure(s).
#'     See Details below.
#' @param geography A character scalar; one of \code{"county"}, \code{"zcta3"},
#'     \code{"zcta5"}, or \code{"tract"}
#' @param index A character scalar or vector listing deprivation measures
#'     to return. These include the area deprivation index (\code{"adi"}),
#'     the gini coefficient (\code{"gini"}), two versions of the neighborhood
#'     deprivation index by Messer (\code{"ndi_m"}) and Powell and Wiley
#'     (\code{"ndi_pw"}), and four versions of the social vulnerability
#'     index (\code{"svi10"}, \code{"svi14"}, \code{"svi20"}, and \code{"svi20s"}).
#'     See Details.
#' @param year A numeric scalar between 2010 and 2022.
#' @param survey A character scalar representing the Census product. It can
#'     be any American Community Survey product (either \code{"acs1"},
#'     \code{"acs3"}, or \code{"acs5"}). Note that \code{"acs3"} was
#'     discontinued after 2013.
#' @param return_percentiles A logical scalar; if \code{TRUE}, scales
#'     (and their subscales) will be returned as percentiles instead of in
#'     raw scores. If \code{FALSE} (default), raw scores will be returned. Note
#'     that SVI is returned as a percentile regardless of what
#'     \code{return_percentiles} is set to.
#' @param keep_subscales A logical scalar; if \code{FALSE} (default), only the
#'     full ADI and/or SVI scores (depending on what is passed to the \code{index}
#'     argument) will be returned. If \code{TRUE} and \code{"svi"} is listed for
#'     the \code{index} argument, the four SVI "themes" (see Details) will be
#'     returned along with the full SVI score. Similarly, if \code{"adi"} is
#'     listed for the \code{index} argument, the three ADI subscales (see Details)
#'     will be returned.
#' @param keep_components A logical scalar; if \code{FALSE} (default), none of
#'     the components used to calculate the deprivation measures will be returned.
#'     If \code{TRUE}, all of the demographic variables used to calculate ADI
#'     and/or SVI will be returned.
#' @param output A character scalar; if \code{"wide"} (default), a tibble
#'     will be returned with row per jurisdiction where individual measures of
#'     deprivation stored in columns. If \code{"tidy"}, a tibble will be returned
#'     with one row for each combination of jurisdiction and deprivation measure.
#'
#' @return A tibble object containing the requested deprivation measures.
#'
#' @details Input data must be "wide" formatted and should have the following columns:
#'     \describe{
#'       \item{\code{"GEOID"}}{The appropriately formatted GEOID values for the
#'         geography given in the function. This is required.}
#'       \item{\code{"YEAR"}}{The year that corresponds to the demographic data.
#'         For five-year ACS data, this should correspond to the final year in
#'         the period (e.x. 2021 for the 2017-2021 ACS). This is required only
#'         if deprivation scores are being generated for more than one year.}
#'       \item{Demographic measures}{All of the necessary columns required for
#'         the deprivation scores and years given (since the input measures vary
#'         between scores and over time for individual scores.)}
#'      }
#'
#' @examples
#' ## load sample data
#' ndi_m <- dep_sample_data(index = "ndi_m")
#'
#' ## calculate NDI with sample data
#' ndi_m <- dep_calc_index(ndi_m, geography = "county", index = "ndi_m", year = 2022,
#'     return_percentiles = TRUE)
#'
#' @export
dep_calc_index <- function(.data, geography, index, year, survey = "acs5",
                           return_percentiles = FALSE, keep_subscales = FALSE,
                           keep_components = FALSE, output = "wide"){

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
    stop("A 'year' value must be provided. Please choose a numeric value between 2010 and 2021.")
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

  if (is.logical(return_percentiles) == FALSE){
    stop("Please provide a logical scalar for 'return_percentiles'.")
  }

  if (is.logical(keep_subscales) == FALSE){
    stop("Please provide a logical scalar for 'keep_subscales'.")
  }

  if (is.logical(keep_components) == FALSE){
    stop("Please provide a logical scalar for 'keep_components'.")
  }

  if (output %in% c("wide", "tidy") == FALSE){
    stop("The 'output' value provided is not valid. Please choose one of 'wide' or 'tidy'.")
  }

  if (output == "tidy" & keep_components == TRUE){
    stop("The 'output' requested is invalid. Tidy output is only available if 'keep_components' is 'FALSE'.")
  }

  # prep extra inputs
  ## fix input type
  input <- "user"

  ## create year label flag
  if (length(year) > 1){
    label_year <- TRUE
  } else {
    label_year <- FALSE
  }

  ## create multi-svi flag
  if (sum(grepl(pattern = "svi", index)) > 1){
    multi_svi = TRUE
  } else if (sum(grepl(pattern = "svi", index)) <= 1){
    multi_svi = FALSE
  }

  # validate variables
  ## create variable list
  var_expected <- lapply(year, function(year_vec) {
    dep_build_multi_varlist(geography = geography, index = index, year = year_vec, survey = survey)
  })

  ## limit output to unique column names
  if (length(year) > 1){
    var_expected <- c("GEOID", "YEAR", sort(unique(unlist(var_expected))))
  } else if (length(year) == 1){
    var_expected <- c("GEOID", sort(unique(unlist(var_expected))))
  }

  ## test variable names
  if (all(var_expected %in% names(.data)) == FALSE){
    stop("Variables necessary for the given year(s) and index/indicies are missing. Please double check your input data.")
  }

  # split data frame
  ## create calculation data
  calc <- subset(.data, select = var_expected)

  ## pull out variables not needed for scoring
  if (length(year) > 1){
    excess_vars <- c("GEOID", "YEAR", setdiff(names(.data), names(calc)))
  } else if (length(year) == 1){
    excess_vars <- c("GEOID", setdiff(names(.data), names(calc)))
  }

  .data <- subset(.data, select = excess_vars)

  # calculate deprivation measures
  ## iterate over years
  out <- lapply(year, function(years_vec) {
    dep_process(calc, geography = geography, index = index, year = years_vec,
                survey = survey, return_percentiles = return_percentiles,
                keep_subscales = keep_subscales, keep_components = keep_components,
                debug = NULL, input = input, output = output,
                label_year = label_year, multi_svi = multi_svi)
  })

  ## combine results
  out <- do.call(rbind, out)

  # re-construct output
  ## remove name if present
  if ("NAME" %in% names(.data) == TRUE){
    out_names <- names(out)[names(out) %in% c("NAME") == FALSE]
    out <- subset(out, select = out_names)
  }

  ## check for variable conflicts
  if (length(year) > 1){
    out_names <- names(out)[names(out) %in% c("GEOID", "YEAR") == FALSE]
  } else if (length(year) == 1){
    out_names <- names(out)[names(out) %in% c("GEOID") == FALSE]
  }

  ## throw warning
  if (any(out_names %in% names(.data)) == TRUE){
    warning("Variable conflicts present between input data and deprivation output. Only output returned.")
  } else if (any(out_names %in% names(.data)) == FALSE){
    if (length(year) > 1){
      out <- merge(x = .data, y = out, by = c("GEOID", "YEAR"), all.x = TRUE)
    } else if (length(year) == 1){
      out <- merge(x = .data, y = out, by = "GEOID", all.x = TRUE)
    }
  }

  # prep output
  if (output == "wide"){
    out <- tibble::as_tibble(out)
  }

  # return output
  return(out)

}
