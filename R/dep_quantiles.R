#' Return Quantiles of a Variable
#'
#' @description This helper function can be used to return quantiles of a
#'     deprivation index (or any other continuous distribution). This is useful
#'     for constructing independent variables for statistical analysis. The
#'     function supports splitting a distribution at the median (2 quantiles)
#'     through deciles (10 quantiles) if character labels are desired.
#'
#' @usage dep_quantiles(.data, source_var, new_var, n = 4L, return = "label")
#'
#' @param .data A tibble containing the data to be used for calculating quantiles.
#' @param source_var Required; the quoted or unquoted source variable to be
#'     divided into quantiles.
#' @param new_var Required; the quoted or unquoted name of the new variable to
#'     be created containing the quantile values.
#' @param n Required integer scalar; the number of quantiles to divide the source
#'     variable into. Defaults to \code{4L} (quartiles), but can be set to any
#'     value appropriate for your data as long as it is greater than or equal
#'     to \code{2L}.
#' @param return Required character scalar; one of either \code{"label"} (default)
#'     or \code{"factor"}. If \code{"label"}, the function will return a character
#'     vector of quantile labels. If \code{"factor"}, the function will return
#'     the underlying factor used in the creation of the quantiles measure.
#'
#' @returns A copy of \code{.data} with a new variable containing the requested
#'     quantile.
#'
#' @examples
#' ## load sample data
#' ndi_m <- dep_sample_data(index = "ndi_m")
#'
#' ## calculate NDI with sample data
#' ndi_m <- dep_calc_index(ndi_m, geography = "county", index = "ndi_m", year = 2022,
#'     return_percentiles = TRUE)
#'
#' ## calculate quantiles, return label
#' ndi_m <- dep_quantiles(ndi_m, source_var = NDI_M, new_var = ndi_m_quartiles_l)
#'
#' unique(sort(ndi_m$ndi_m_quartiles_l))
#'
#' ## calculate quantiles, return label
#' ndi_m <- dep_quantiles(ndi_m, source_var = NDI_M, new_var = ndi_m_quartiles_l6,
#'                        n = 6L)
#'
#' unique(sort(ndi_m$ndi_m_quartiles_l6))
#'
#' ## calculate quantiles, return factor
#' ndi_m <- dep_quantiles(ndi_m, source_var = NDI_M, new_var = ndi_m_quartiles_f,
#'     return = "factor")
#'
#' levels(ndi_m$ndi_m_quartiles_f)
#'
#' @export
dep_quantiles <- function(.data, source_var, new_var, n = 4L, return = "label"){

  # save parameters to list
  paramList <- as.list(match.call())

  # quote input variables
  if (!is.character(paramList$source_var)) {
    ref <- rlang::enquo(source_var)
  } else if (is.character(paramList$source_var)) {
    ref <- rlang::quo(!! rlang::sym(source_var))
  }

  refQ <- rlang::quo_name(rlang::enquo(ref))

  if (!is.character(paramList$new_var)) {
    new <- rlang::enquo(new_var)
  } else if (is.character(paramList$new_var)) {
    new <- rlang::quo(!! rlang::sym(new_var))
  }

  newQ <- rlang::quo_name(rlang::enquo(new))

  # check inputs
  if (!is.data.frame(.data)) {
    stop("The object passed to the '.data' argument must be a data frame.")
  }

  if (!refQ %in% names(.data)) {
    stop("The variable name passed to the 'source_var' argument does not exist in the data.")
  }

  if (newQ %in% names(.data)){
    warning("The variable name passed to the 'new_var' argument already exists in the data. It will be overwritten.")
  }

  if (!is.integer(n)){
    stop("The 'n' argument must be an integer. Include an 'L' suffix to specify an integer value - e.x. 4L.")
  }

  if (n < 2L){
    stop("The 'n' argument must be 2L or greater.")
  }

  if (!return %in% c("label", "factor")){
    stop("The 'return' argument must be either 'label' or 'factor'.")
  }

  ## calculate breaks
  out <- dep_map_breaks(
    .data = .data, var = !!refQ, new_var = "source_var_quantiles",
    classes = n, style = "quantile", show_warnings = FALSE
  )

  ## optionally clean-up labels
  if (return == "label"){

    ## stash levels to simplify
    levels <- levels(out[["source_var_quantiles"]])

    ## create labels
    if (n == 2L){

      out <- dplyr::mutate(out, !!newQ := dplyr::case_when(
        source_var_quantiles == levels[1] ~ "(1) Lower Than Median",
        source_var_quantiles == levels[2] ~ "(2) Greater Than Median"
      ))

    } else if (n == 3L){

      out <- dplyr::mutate(out, !!newQ := dplyr::case_when(
        source_var_quantiles == levels[1] ~ "(1) Lowest Tertile",
        source_var_quantiles == levels[2] ~ "(2) Middle Tertile",
        source_var_quantiles == levels[3] ~ "(3) Highest Tertile"
      ))

    } else {

      ## stash max level
      max_level <- length(levels)

      ## stash quantile label
      quantile_label <- dep_quantile_label(n)

      ## convert to numeric
      out <- dplyr::mutate(out, source_var_quantiles = as.numeric(source_var_quantiles))

      ## create quantile labels
      out <- dplyr::mutate(out, !!newQ := dplyr::case_when(
        source_var_quantiles == 1L ~ paste0("(1) Lowest ", quantile_label),
        source_var_quantiles == max_level ~ paste0("(", max_level, ") Highest ", quantile_label),
        TRUE ~ paste0("(", source_var_quantiles, ") ",
                      dep_ordinal(source_var_quantiles),
                      " ", quantile_label)
      ))

    }

    ## remove source_var_quantiles
    out <- dplyr::select(out, -source_var_quantiles)

  } else if (return == "factor"){

    out <- dplyr::rename(out, !!newQ := source_var_quantiles)

  }

  ## return output
  return(out)

}


dep_quantile_label <- function(x){

  ## identify quantile label
  if (x == 4L){
    out <- "Quartile"
  } else if (x == 5L){
    out <- "Quintile"
  } else if (x == 6L){
    out <- "Sextile"
  } else if (x == 7L){
    out <- "Septile"
  } else if (x == 8L){
    out <- "Octile"
  } else if (x == 9L){
    out <- "Nonile"
  } else if (x == 10L){
    out <- "Decile"
  } else {
    out <- "Quantile"
  }

  ## return output
  return(out)

}

dep_ordinal <- function(x){

  ## create title case after ordinal conversion
  out <- paste0(toupper(substr(english::ordinal(x), 1, 1)),
                substr(english::ordinal(x), 2,
                       nchar(english::ordinal(x))
                       )
                )

  ## return output
  return(out)

}
