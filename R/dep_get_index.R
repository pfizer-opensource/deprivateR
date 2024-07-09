#' Calculate Deprivation Measures
#'
#' @description Downloads raw data and then calculates various measures of
#'     deprivation and/or vulnerability, including a range of options for structuring output. The
#'     included measures include four versions of the CDC's social vulnerability
#'     index, which is a unique offering, along with wrappers that bring in
#'     additional measures from related packages: the area deprivation index
#'     (ADI; via \code{sociome}), gini coefficient (via \code{tidycensus}), and
#'     the neighborhood deprivation index (NDI; via \code{ndi}). Both ADI and NDI
#'     contain variations as well. See Details for more information.
#'
#' @usage dep_get_index(geography, index, year, survey = "acs5",
#'     return_percentiles = FALSE, keep_subscales = FALSE,
#'     keep_components = FALSE, output = "wide",
#'     state = NULL, county = NULL, puerto_rico = FALSE, zcta = NULL,
#'     zcta_geo_method = NULL, zcta_cb = FALSE, zcta3_method = NULL,
#'     shift_geo = FALSE, key = NULL)
#'
#' @param geography A character scalar; one of \code{"county"}, \code{"zcta3"},
#'     \code{"zcta5"}, or \code{"tract"}
#' @param index A character scalar or vector listing deprivation measures
#'     to return. These include the area deprivation index (\code{"adi"}),
#'     the gini coefficient (\code{"gini"}), two versions of the neighborhood
#'     deprivation index by Messer (\code{"ndi_m"}) and Powell and Wiley
#'     (\code{"ndi_pw"}), and four versions of the social vulnerability
#'     index (\code{"svi10"}, \code{"svi14"}, \code{"svi20"}, and \code{"svi20s"}).
#'     See Details.
#' @param year A numeric scalar or vector. 2010 is earliest year \code{deprivateR}
#'     supports, while 2022 is the most recent year.
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
#'     If \code{"sf"}, a "wide" data set will be returned with geometric data
#'     appeneded to facilitate mapping and/or spatial statistics.
#' @param state A character scalar or vector with character state abbreviations
#'     (e.x. \code{"MO"}) or numeric FIPS codes (e.x. \code{29}).
#' @param county A character scalar or vector with character GEOIDs (e.x.
#'     \code{"29510"})
#' @param puerto_rico A logical scalar; if \code{TRUE} (default), data for Puerto
#'     Rico will be included in calculations. If \code{FALSE}, Puerto Rico will
#'     not be included.
#' @param zcta An optional vector of ZCTAs that demographic data are requested
#'     for. If this is \code{NULL} and \code{geography = "zcta5"}, data will be
#'     returned for all ZCTAs. If a vector is supplied and \code{geography = "zcta5"},
#'     only data for those requested ZCTAs will be returned. The vector can be created
#'     with \code{zippeR::zi_get_geometry()} and should only contain five-digit ZCTAs.
#' @param zcta_geo_method A character scalar; if \code{geography = "zcta5"} or
#'     \code{geography = "zcta3"}, either \code{"intersect"} or \code{"centroid"},
#'     should be supplied. These two options alter how ZCTA overlap with states
#'     or counties is defined. See \code{zippeR::zi_get_geometry()} for more
#'     information.
#' @param zcta3_method A character scalar; if \code{geography = "zcta3"}, a
#'     method for aggregating spatially intensive values should be given;
#'     either \code{"mean"} or \code{"median"}. In either case, a weighted approach
#'     is used where total population for each five-digit ZCTA is used to calculate
#'     individual ZCTAs' weights. For American Community Survey Data, this is
#'     applied to the margin of error as well.
#' @param zcta_cb A logical scalar; if \code{FALSE}, the most detailed TIGER/Line
#'     data will be used for \code{style = "zcta5"}. If \code{TRUE}, a
#'     generalized (1:500k) version of the data will be used. The generalized
#'     data will download significantly faster, though they show less detail.
#'     According to the \code{tigris::zctas()} documentation, the download size
#'     if \code{TRUE} is ~65MB while it is ~500MB if \code{cb = FALSE}.
#'
#'     This argument does not apply to \code{geography = "zcta3"}, which only returns
#'     generalized data. It only applies if \code{output = "sf"}.
#' @param shift_geo A logical scalar; if \code{TRUE}, Alaska, Hawaii, and Puerto Rico
#'     will be re-positioned so that the lie to the southwest of the continental
#'     United States. This defaults to \code{FALSE}, and can only be used when
#'     states are not listed for the \code{state} argument. It only applies if
#'     \code{output = "sf"}.
#' @param key A Census API key, which can be obtained at
#'     \url{https://api.census.gov/data/key_signup.html}. This can be omitted if
#'     \code{tidycensus::census_api_key()} has been used to write your key to
#'     your \code{.Renviron} file. You can check whether an API key has been
#'     written to \code{.Renviron} by using \code{Sys.getenv("CENSUS_API_KEY")}.
#'
#' @details \code{deprivateR} provides a unique implementation of the Centers
#'     for Disease Control's Social Vulnerability Index at a greater range
#'     of years and geographies than the CDC originally supported. Four versions
#'     of the SVI are offered:
#'     \describe{
#'       \item{\code{"svi10"}}{The CDC's 2010 SVI vintage did not include a measure
#'         of civilians with a disability, unlike their later vintages. This version
#'         can be calculated using \code{deprivateR} for each year from 2010 through
#'         2021.}
#'       \item{\code{"svi14"}}{The CDC's 2014, 2016, and 2018 vintages added the
#'         measure of civilians with a disability to their SVI calculations. The
#'         disability measure was added to the American Community Survey beginning
#'         in 2012, so this version can be calculated using \code{deprivateR} for
#'         each year from 2012 through 2021.}
#'       \item{\code{"svi20"}}{The CDC's 2020 vintage made multiple substantive
#'         changes to how SVI is calculated that changed the underlying data
#'         used for the first three of the four themes. In the SES theme: (1) per
#'         capita income was replaced with a measure of housing burden; (2) poverty
#'         was converted to 150% of the poverty line; and (3) the rate of no health
#'         insurance. The Household Composition & Disability (HCD) theme was renamed
#'         Household Characteristics (HOU), and the English language proficiency measure
#'         was moved here from the former Minority Status and Language (MSL) theme.
#'         Since the English language measure was removed from MSL theme, it was
#'         renamed Racial & Ethnic Minority Status (REM). Though the CDC released
#'         this definition with their 2020 data, the underlying data can be
#'         accessed from the American Community Survey from 2012 onward. This means
#'         that this version can be calculated using \code{deprivateR} for
#'         each year from 2012 through 2021.}
#'       \item{\code{"svi20s"}}{The CDC's 2020 vintage changed the variables
#'         used to calculate the number of single-parent households. Their new
#'         approach does not have the backward compatibility that the other
#'         changes made in 2020 do. This version of SVI uses the same underlying
#'         data for single-parent households that the CDC's 2020 vintage does,
#'         along with the other changes made in 2020. This version can be
#'         calculated using \code{deprivateR} for each year from 2012 through
#'         2019. }
#'     }
#'
#'     In addition, wrappers to the \code{sociome}, \code{ndi}, and \code{tidycensus}
#'     package create a single point of departure for comparative work using multiple
#'     measures of deprivation or inequality.
#'
#' @examples
#' \donttest{
#'   # calculate ADI for all US counties
#'   dep_get_index(geography = "county", index = "adi", year = 2022)
#'
#'   # calculate two forms of SVI for all Missouri ZCTAs
#'   dep_get_index(geography = "zcta5", index = c("svi20", "svi20s"), year = 2022,
#'     state = "MO")
#'
#'   # calculate ADI and two forms of NDI for all US counties over three years
#'   # percentiles are returned to ease comparison
#'   dep_get_index(geography = "county", index = c("adi", "svi14"),
#'     year = c(2018:2020), return_percentiles = TRUE)
#' }
#'
#' @export
dep_get_index <- function(geography, index, year, survey = "acs5",
                          return_percentiles = FALSE, keep_subscales = FALSE,
                          keep_components = FALSE, output = "wide",
                          state = NULL, county = NULL, puerto_rico = FALSE,
                          zcta = NULL, zcta_geo_method = NULL, zcta_cb = FALSE,
                          zcta3_method = NULL, shift_geo = FALSE, key = NULL){

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
    stop("Invalid index provided. Please choose one of: 'adi', 'gini', 'ndi_m', 'ndi_pw', 'svi10', 'svi14', or 'svi20'.")
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

  if (survey == "acs3" & max(year) > 2013){
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

  if (is.null(state) == FALSE){
    state <- unlist(sapply(state, validate_state, USE.NAMES=FALSE))
  }

  if (any(state %in% c("AS", "GU", "MP", "PR", "VI")) == TRUE){
    stop("Territories cannot be specified using the 'state' argument. Use the 'puerto_rico' argument for Puerto Rico. Deprivation measures are not available for other territories.")
  }

  if (is.null(county) == FALSE & is.null(state) == FALSE){
    stop("Please choose values for either 'state' or 'county' but not both.")
  }

  if (is.logical(puerto_rico) == FALSE){
    stop("Please provide a logical scalar for 'puerto_rico'.")
  }

  ## need to check for zcta3_method

  if (geography != "zcta5" & is.null(zcta) == FALSE){
    warning("The 'zcta' argument was ignored because the geography requested is not 'zcta5'.")
  } else if (geography == "zcta5" & is.null(zcta) == FALSE){
    valid <- zippeR::zi_validate(zcta, style = geography)

    if (valid == FALSE){
      stop("ZCTA data passed to the 'zcta' argument are invalid. Please use 'zippeR::zi_validate()' with the 'verbose = TRUE' option to investgiate further. The 'zippeR::zi_repair()' function may be used to address isses.")
    }
  }

  if (output %in% c("tidy", "wide", "sf") == FALSE){
    stop("The 'output' requested is invalid. Please choose one of 'tidy', 'wide', or 'sf'.")
  }

  if (output == "tidy" & keep_components == TRUE){
    stop("The 'output' requested is invalid. Tidy output is only available if 'keep_components' is 'FALSE'.")
  }

  if (is.logical(zcta_cb) == FALSE){
    stop("Please provide a logical scalar for 'zcta_cb'.")
  }

  # if (is.logical(keep_geo_vars) == FALSE){
  #  stop("Please provide a logical scalar for 'keep_geo_vars'.")
  # }
  keep_geo_vars <- FALSE

  if (is.logical(shift_geo) == FALSE){
    stop("Please provide a logical scalar for 'shift_geo'.")
  }

  # if (units %in% c("mi2", "km2") == FALSE){
  #  stop("The 'units' requested are invalid. Please choose one of 'km2' or 'mi2'.")
  # }

  debug <- "live"

  # prep extra inputs
  ## fix input type
  input <- "download"

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

  ## create deprivation scores by iterating over year
  out <- lapply(year, function(years_vec) {
    dep_process(geography = geography, index = index, year = years_vec,
                survey = survey, return_percentiles = return_percentiles,
                keep_subscales = keep_subscales, keep_components = keep_components,
                state = state, county = county, puerto_rico = puerto_rico,
                zcta = zcta, zcta3_method = zcta3_method, zcta_geo_method = zcta_geo_method,
                geometry = geometry, cb = zcta_cb, keep_geo_vars = keep_geo_vars,
                shift_geo = shift_geo, key = key,
                debug = debug, input = input, output = output,
                label_year = label_year, multi_svi = multi_svi)
  })

  ## combine results
  out <- do.call(rbind, out)

  ## re-order results
  if (label_year == TRUE){
    out <- out[order(out$GEOID, out$YEAR), ]
  } else if (label_year == FALSE){
    out <- out[order(out$GEOID), ]
  }

  # return output
  return(out)

}

