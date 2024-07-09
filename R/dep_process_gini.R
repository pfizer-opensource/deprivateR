# Processing Function for Gini #####

dep_process_gini <- function(.data, geography, year, survey){

  ## create variable list
  varlist <- dep_expand_varlist(geography = geography, index = "gini",
                                year = year, survey = survey)
  varlist <- c("GEOID", varlist)

  ## subset
  .data <- subset(.data, select = varlist)

  ## rename
  names(.data)[names(.data) == "B19083_001E"] <- "E_GINI"
  names(.data)[names(.data) == "B19083_001M"] <- "M_GINI"

  ## return output
  return(.data)

}
