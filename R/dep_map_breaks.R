#' Calculating Map Breaks
#'
#' @description Create "bins" for choropleth maps creating using either
#'     \code{ggplot2} or \code{leaflet}. The function can create the bins
#'     automatically or will accept pre-specified breaks.
#'
#' @usage dep_map_breaks(.data, var, new_var, classes, style, breaks,
#'     sig_digits = 2, return = "col", show_warnings = TRUE)
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
  if (return %in% c("col", "breaks") == FALSE){
    stop("The 'return' argument only accepts 'col' or 'breaks' as arguments.")
  }

  # check for correct combination of parameters
  if (missing(breaks) == FALSE & return == "breaks"){
    stop("Returning breaks is only possible if breaks are not first supplied.")
  }

  if ((missing(classes) == TRUE | missing(style) == TRUE) & missing(breaks) == TRUE){
    stop("Values must be supplied for both 'classes' and 'style', or 'breaks'.")
  }

  if (return == "col" & missing(new_var) == TRUE){
    stop("A variable name for a new column must be supplied if 'return' is 'col'.")
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
  if (refQ %in% names(.data) == FALSE){
    stop("The variable supplied for 'var' cannot be found in the data object.")
  }

  if (class(.data[[refQ]]) %in% c("numeric", "integer") == FALSE){
    stop("The variable supplied for 'var' is not formatted as a numeric or integer variable.")
  }

  # check other arguments
  if (missing(classes) == FALSE){
    if (class(classes) %in% c("numeric", "integer") == FALSE){
      stop("The value supplied for 'classes' is not formatted as a numeric or integer value.")
    }
  }

  if (missing(style) == FALSE){
    styles <- c("fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust",
                "bclust", "fisher", "jenks", "dpih", "headtails", "maximum", "box")

    if (style %in% styles == FALSE){
      stop("The style provided is not supported by classIn::classIntervals. Please select an alternative.")
    }
  }

  if (missing(breaks) == FALSE){
    if (class(breaks) %in% c("numeric", "integer") == FALSE){
      stop("The vector supplied for 'breaks' is not formatted as a numeric or integer vector.")
    }

    if (length(breaks) < 3){
      stop("At least three values must be supplied to create a choropleth map with two breaks, which is the minimum supported.")
    }
  }

  if (class(sig_digits) %in% c("numeric", "integer") == FALSE){
    stop("The value supplied for 'sig_digits' is not formatted as a numeric or integer value.")
  }

  if (is.logical(show_warnings) == FALSE){
    stop("The value supplied for 'show_warnings' is not valid - please provide a logical value.")
  }

  # create breaks object, rounded to specified significant digits
  ## create breaks_values
  if (missing(breaks) == TRUE){

    if (show_warnings == TRUE){
      breaks_values <- classInt::classIntervals(.data[[refQ]], n = classes,
                                                style = style)
    } else if (show_warnings == FALSE){
      breaks_values <- suppressWarnings(classInt::classIntervals(.data[[refQ]],
                                                                 n = classes,
                                                                 style = style))
    }

    breaks_values <- breaks_values$brks

  } else if (missing(breaks) == FALSE){
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
