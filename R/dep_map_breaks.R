#' Calculating Map Breaks
#'
#' @description Create "bins" for choropleth maps creating using either
#'     \code{ggplot2} or \code{leaflet}. The function can create the bins
#'     automatically or will accept pre-specified breaks.
#'
#'
#' @param .data A data object, either sf, tibble, or data.frame
#' @param var Variable breaks should be based on, can be quoted or unquoted
#' @param new_var Optional name of new variable to store breaks in, can be quoted
#'     or unquoted. This is required if you are returning a column, but can be
#'     omitted if you are returning breaks instead of a column.
#' @param classes Optional integer scalar; count of the number of classes to create.
#'     If you are supplying breaks manually, this can be omitted.
#' @param style String scalar; one of the classes supported by \code{classInt::classIntervals()}.
#'     "jenks" is the ArcGIS default. "quantile" and "fisher" are better
#'     alternatives. As with \code{classes}, this can be omitted if you are
#'     supplying breaks manually.
#' @param breaks Optional numeric vector if you want to pre-specify the cut
#'     points for your breaks. Provide the lower and upper bounds of your
#'     distribution. Any values supplied in between the bounds will be the
#'     upper bound of individual bins.
#' @param sig_digits Integer; how many significant digits should be applied when
#'     calculating breaks and constructing labels?
#' @param return String scalar; one of either \code{"col"} (default) or
#'     \code{"breaks"}. The default behavior adds a factor containing the bins
#'     to the data object to facilitate mapping. Specifying \code{"breaks"}
#'     will return the calculated breaks instead, which can be modified and
#'     passed to the \code{breaks} argument later in a script to make the
#'     final map.
#' @param show_warnings Logical scalar; if \code{TRUE}, warnings created by
#'     \code{classInt::classIntervals()} if \code{NA} values are identified while
#'     findings classes.
#'
#' @return Either a data object (if \code{return} is \code{"col"}) or a vector
#'     of breaks (if \code{return} is \code{"breaks"}). If a data object is
#'     returned, the new column will be placed directly after the input variable
#'     specified in \code{var}.
#'
#' @examples
#' # prep data
#' ## load sample data
#' ndi_m <- dep_sample_data(index = "ndi_m")
#'
#' ## calculate NDI with sample data
#' ndi_m <- dep_calc_index(ndi_m, geography = "county", index = "ndi_m", year = 2022,
#'   return_percentiles = TRUE)
#'
#' # calculate breaks using a built-in algorithm
#' dep_map_breaks(ndi_m, var = "NDI_M", new_var = "map_breaks", classes = 5,
#'   style = "fisher")
#'
#' # use manually specified breaks
#' ## set breaks
#' breaks <- c(0, 25, 50, 75, max(ndi_m$NDI_M))
#'
#' ## calculate breaks
#' dep_map_breaks(ndi_m, var = "NDI_M", new_var = "map_breaks", breaks = breaks)
#'
#' @export
dep_map_breaks <- function(.data, var, new_var, classes, style, breaks,
                           sig_digits = 2, return = "col", show_warnings = TRUE){

  # check return
  if (!(return %in% c("col", "breaks"))){
    cli::cli_abort("{.arg return} only accepts {.val col} or {.val breaks}.")
  }

  # check for correct combination of parameters
  if (!missing(breaks) & return == "breaks"){
    cli::cli_abort(c("Returning breaks is only possible when {.arg breaks} is not supplied.", "i" = "Omit {.arg breaks} or set {.arg return} to {.val col}."))
  }

  if ((missing(classes) | missing(style)) & missing(breaks)){
    cli::cli_abort(c("Supply values for both {.arg classes} and {.arg style}, or supply {.arg breaks}.", "i" = "Use {.arg classes} with {.arg style} to compute breaks, or provide {.arg breaks} directly."))
  }

  if (return == "col" & missing(new_var)){
    cli::cli_abort(c("A name for {.arg new_var} must be supplied when {.arg return} is {.val col}.", "i" = "Provide {.arg new_var} or set {.arg return} to {.val breaks}."))
  }

  # save parameters to list
  paramList <- as.list(match.call())

  # quote input variables
  if (!is.character(paramList$var)) {
    ref <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    ref <- rlang::quo(!! rlang::sym(var))
  }

  refQ <- rlang::quo_name(rlang::enquo(ref))

  if (return == "col"){
    if (!is.character(paramList$new_var)) {
      new <- rlang::enquo(new_var)
    } else if (is.character(paramList$new_var)) {
      new <- rlang::quo(!! rlang::sym(new_var))
    }

    newQ <- rlang::quo_name(rlang::enquo(new))
  }

  # check that source variable exists and is numeric
  if (!(refQ %in% names(.data))){
    cli::cli_abort("The variable supplied for {.arg var} cannot be found in {.arg .data}.")
  }

  if (!(class(.data[[refQ]]) %in% c("numeric", "integer"))){
    cli::cli_abort("The variable supplied for {.arg var} must be numeric or integer.")
  }

  # check other arguments
  if (!missing(classes)){
    if (!(class(classes) %in% c("numeric", "integer"))){
      cli::cli_abort("The value supplied for {.arg classes} must be numeric or integer.")
    }
  }

  if (!missing(style)){
    styles <- c("fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust",
                "bclust", "fisher", "jenks", "dpih", "headtails", "maximum", "box")

    if (!(style %in% styles)){
      cli::cli_abort(c("{.arg style} is not supported by classInt::classIntervals().", "i" = "Use one of: {.val fixed}, {.val sd}, {.val equal}, {.val pretty}, {.val quantile}, {.val kmeans}, {.val hclust}, {.val bclust}, {.val fisher}, {.val jenks}, {.val dpih}, {.val headtails}, {.val maximum}, or {.val box}."))
    }
  }

  if (!missing(breaks)){
    if (!(class(breaks) %in% c("numeric", "integer"))){
      cli::cli_abort("The vector supplied for {.arg breaks} must be numeric or integer.")
    }

    if (length(breaks) < 3){
      cli::cli_abort(c("At least three values must be supplied for {.arg breaks}.", "i" = "This is the minimum needed to create two choropleth intervals."))
    }
  }

  if (!(class(sig_digits) %in% c("numeric", "integer"))){
    cli::cli_abort("The value supplied for {.arg sig_digits} must be numeric or integer.")
  }

  if (!is.logical(show_warnings)){
    cli::cli_abort(c("The value supplied for {.arg show_warnings} must be logical.", "i" = "Use either {.val TRUE} or {.val FALSE}."))
  }

  # create breaks object, rounded to specified significant digits
  ## create breaks_values
  if (missing(breaks)){

    if (show_warnings){
      breaks_values <- classInt::classIntervals(.data[[refQ]], n = classes,
                                                style = style)
    } else if (!show_warnings){
      breaks_values <- suppressWarnings(classInt::classIntervals(.data[[refQ]],
                                                                 n = classes,
                                                                 style = style))
    }

    breaks_values <- breaks_values$brks

  } else if (!missing(breaks)){
    breaks_values <- breaks
  }

  ## round breaks_values
  breaks_round <- round(breaks_values, digits = sig_digits)

  # create output
  if (return == "col"){

    ## create labels
    ### create data.frame with breaks
    labels <- data.frame(cbind(breaks_round[-length(breaks_round)], breaks_round[-1]))
    labels$id <- seq.int(nrow(labels))

    ### to create unique labels, add a value to each lower break other than the first
    if (sig_digits == 0){
      add <- 1
    } else if (sig_digits > 0){
      add <- 1/(10^sig_digits)
    }

    labels$X1 <- ifelse(labels$id > 1, labels$X1 + add, labels$X1)

    ### convert labels to strings to preserve significant digits even if value
    ### ends with zeros
    labels$X1 <- sprintf(paste0("%.", sig_digits, "f"), labels$X1)
    labels$X2 <- sprintf(paste0("%.", sig_digits, "f"), labels$X2)

    ### create labels
    labels <- paste0(labels$X1, " - ", labels$X2)

    ## create categories
    categories <- cut(.data[[refQ]],
                      breaks = breaks_values,
                      labels = labels,
                      include.lowest = TRUE)

    ## create new variable with categories applied
    out <- dplyr::mutate(.data, !!newQ := categories, .after = dplyr::all_of(refQ))

  } else if (return == "breaks"){
    out <- breaks_values
  }

  # return result
  return(out)

}
