#' Calculate Percentiles
#'
#' @description Calculate percentiles for a given variable in a data frame. This
#'     is the method used to calculate ranked percentiles for SVI.
#'
#' @usage dep_percentiles(.data, source_var, new_var)
#'
#' @param .data A tibble containing the data to be used for calculating percentiles.
#' @param source_var Required; the quoted or unquoted source variable to be
#'     divided into percentiles.
#' @param new_var Required; the quoted or unquoted name of the new variable to
#'     be created containing the quantile values.
#'
#' @return An updated tibble with the percentiles added as a new column or with
#'     replaced values in the source column.
#'
#' @examples
#' ## load sample data
#' ndi_m <- dep_sample_data(index = "ndi_m")
#'
#' # calculate percentiles for population 25 years and older
#' ndi_m <- dep_percentiles(ndi_m, source_var = B06009_001E,
#'     new_var = pop25_percentile)
#'
#' # preview the new data
#' ndi_m[names(ndi_m) %in% c("GEOID", "B06009_001E", "pop25_percentile")]
#'
#' @export
dep_percentiles <- function(.data, source_var, new_var){

  # check inputs
  if (!inherits(.data, what = "data.frame")){
    stop("The '.data' object provided is not a data frame or sf object.")
  }

  source_varQN <- as.character(substitute(source_var))

  if (source_varQN %in% names(.data) == FALSE){
    stop("The given 'source_var' column is not found in your data object.")
  }

  # if (!inherits(.data[[source_varQN]], what = "numeric")){
  #  stop("The given 'source_var' column is not numeric.")
  # }

  if (missing(new_var)){
    new_varQN <- source_varQN
  } else if (!missing(new_var)){
    new_varQN <- as.character(substitute(new_var))

    if (new_varQN %in% names(.data) == TRUE){
      warning("The given 'new_var' column exists in your data and will be overwritten.")
    }
  }

  # calculate percentiles
  .data[[new_varQN]] <- dep_percent_rank(.data[[source_varQN]])

  # return output
  return(.data)

}

# calculating percent rank
dep_percent_rank <- function(x){
  out <- (rank(x, ties.method = "min", na.last = "keep") - 1)/(sum(!is.na(x)) -1)
  return(out)
}
