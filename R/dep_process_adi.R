# Processing Function for ADI #####

dep_process_adi <- function(.data, geography, year, survey,
                            keep_subscales, keep_components,
                            return_percentiles){

  ## create variable list
  varlist <- dep_expand_varlist(geography = geography, index = "adi",
                                year = year, survey = survey,
                                estimates_only = TRUE)
  varlist <- c("GEOID", varlist)

  ## subset
  .data <- subset(.data, select = varlist)

  ## prep names
  x <- names(.data)
  x <- gsub("E$", "", x)
  names(.data) <- x

  ## call sociome
  .data <- sociome::calculate_adi(.data, keep_indicators = keep_components)

  ## clean-up
  .data <- subset(.data, select = -B11005_001)

  ## prep output
  if (return_percentiles == TRUE){
    .data$ADI <- dep_percent_rank(.data$ADI)
    .data$ADI <- .data$ADI*100
  }

  if (keep_subscales == FALSE){

    .data <- subset(.data, select = -c(Financial_Strength,
                                       Economic_Hardship_and_Inequality,
                                       Educational_Attainment))

  } else if (keep_subscales == TRUE){

    names(.data)[names(.data) == "Financial_Strength"] <- "ADI3_FINS"
    names(.data)[names(.data) == "Economic_Hardship_and_Inequality"] <- "ADI3_ECON"
    names(.data)[names(.data) == "Educational_Attainment"] <- "ADI3_EDU"

    if (return_percentiles == TRUE){
      .data$ADI3_FINS <- dep_percent_rank(.data$ADI3_FINS)
      .data$ADI3_FINS <- .data$ADI3_FINS*100

      .data$ADI3_ECON <- dep_percent_rank(.data$ADI3_ECON)
      .data$ADI3_ECON <- .data$ADI3_ECON*100

      .data$ADI3_EDU <- dep_percent_rank(.data$ADI3_EDU)
      .data$ADI3_EDU <- .data$ADI3_EDU*100
    }
  }

  ### clean-up component variable names

  ## return output
  return(.data)

}
