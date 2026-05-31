#' Create Sample Data
#'
#' @description Create sample data for testing the package functionality.
#'
#' @usage dep_sample_data(index)
#'
#' @param index A character scalar or vector listing deprivation measures
#'     to return. These include the area deprivation index (\code{"adi"}),
#'     the gini coefficient (\code{"gini"}), two versions of the neighborhood
#'     deprivation index by Messer (\code{"ndi_m"}) and Powell and Wiley
#'     (\code{"ndi_pw"}), and four versions of the social vulnerability
#'     index (\code{"svi10"}, \code{"svi14"}, \code{"svi20"}, and \code{"svi20s"}).
#'
#' @return A tibble containing the raw 2022 American Community Survey data for
#'     the given index. Each tibble will contain observations for the 115
#'     counties in Missouri.
#'
#' @examples
#' ## load sample data
#' dep_sample_data(index = "ndi_m")
#'
#' @export
dep_sample_data <- function(index){

  # check inputs
  if (missing(index)){
    cli::cli_abort(c(
      "A {.arg index} value must be provided.",
      "i" = "Choose one of: {.val adi}, {.val ndi_m}, {.val ndi_pw}, {.val svi10}, {.val svi14}, {.val svi20}, or {.val svi20s}."
    ))
  }

  if (!all(index %in% c("adi", "ndi_m", "ndi_pw", "svi10", "svi14", "svi20", "svi20s"))){
    cli::cli_abort(c(
      "Invalid {.arg index} provided: {.val {index}}.",
      "i" = "Choose one of: {.val adi}, {.val ndi_m}, {.val ndi_pw}, {.val svi10}, {.val svi14}, {.val svi20}, or {.val svi20s}."
    ))
  }

  # get vector of variables
  vars <- dep_expand_varlist(geography = "county", index = index, year = 2022, survey = "acs5")

  # create output data
  out <- dplyr::mutate(mo, YEAR = 2022)
  out <- dplyr::select(out, dplyr::all_of(c("GEOID", "NAME", "YEAR", vars)))

  # return output
  return(out)

}
