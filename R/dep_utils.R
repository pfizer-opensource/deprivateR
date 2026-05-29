pivot_demos <- function(.data, vars){

  out <- tidyr::pivot_longer(.data, cols = dplyr::all_of(vars), names_to = "variable", values_to = "values")
  out$variable <- ifelse(grepl("E", out$variable, fixed = TRUE) == TRUE, "estimate", "moe")

  out <- suppressWarnings(tidyr::pivot_wider(out, id_cols = GEOID, names_from = variable, values_from = values))
  out <- tidyr::unchop(out, cols = c(GEOID, estimate, moe))

  out <- dplyr::group_by(out, GEOID)
  out <- dplyr::summarise(out, estimate = sum(estimate, na.rm = TRUE), moe = sum(moe, na.rm = TRUE))

  return(out)

}


# Safe percentage calculation ---------------------------------------------
# Returns NA_real_ when denominator is 0 or NA instead of Inf/NaN
dep_safe_pct <- function(numerator, denominator) {
  result <- ifelse(denominator == 0 | is.na(denominator),
                   NA_real_,
                   numerator / denominator * 100)
  return(result)
}


# Derived margin of error for proportions ---------------------------------
# Census-standard derived MOE formula:
#   ((sqrt(M^2 - ((EP/100)^2 * DM^2))) / DM) * 100
# Returns NA_real_ when denominator is 0/NA or radicand is negative
dep_derived_moe <- function(estimate_moe, proportion, denominator_moe, denominator) {
  radicand <- estimate_moe^2 - ((proportion / 100)^2 * denominator_moe^2)
  invalid <- denominator == 0 | is.na(denominator) | radicand < 0 | is.na(radicand)
  # Use pmax to avoid sqrt(negative) warnings; invalid cases are replaced with NA below
  safe_radicand <- ifelse(invalid, 0, radicand)
  result <- ifelse(invalid, NA_real_, (sqrt(safe_radicand) / denominator) * 100)
  return(result)
}


# Shared input validation -------------------------------------------------
# Validates parameters common to dep_get_index() and dep_calc_index()
dep_validate_inputs <- function(geography, index, year, survey,
                                return_percentiles, keep_subscales,
                                keep_components, output,
                                valid_output = c("tidy", "wide", "sf")) {


  # geography
  if (missing(geography) || is.null(geography)) {
    cli::cli_abort(c(
      "A {.arg geography} value must be provided.",
      "i" = "Choose one of: {.val county}, {.val zcta3}, {.val zcta5}, or {.val tract}."
    ))
  }

  if (geography %in% c("county", "zcta3", "zcta5", "tract") == FALSE) {
    cli::cli_abort(c(
      "Invalid {.arg geography} provided: {.val {geography}}.",
      "i" = "Choose one of: {.val county}, {.val zcta3}, {.val zcta5}, or {.val tract}."
    ))
  }

  # index
  if (missing(index) || is.null(index)) {
    cli::cli_abort(c(
      "A {.arg index} value must be provided.",
      "i" = "Choose one of: {.val adi}, {.val gini}, {.val ndi_m}, {.val ndi_pw}, {.val svi10}, {.val svi14}, {.val svi20}, or {.val svi20s}."
    ))
  }

  if (all(index %in% c("adi", "gini", "ndi_m", "ndi_pw", "svi10", "svi14", "svi20", "svi20s")) == FALSE) {
    cli::cli_abort(c(
      "Invalid {.arg index} provided: {.val {index}}.",
      "i" = "Choose one of: {.val adi}, {.val gini}, {.val ndi_m}, {.val ndi_pw}, {.val svi10}, {.val svi14}, {.val svi20}, or {.val svi20s}."
    ))
  }

  # year
  if (missing(year) || is.null(year)) {
    cli::cli_abort(c(
      "A {.arg year} value must be provided.",
      "i" = "Choose a numeric value between 2010 and 2022."
    ))
  }

  if (is.numeric(year) == FALSE | (min(year) < 2010 | max(year) > 2022)) {
    cli::cli_abort(c(
      "The {.arg year} value provided is invalid.",
      "i" = "Please provide a numeric value between 2010 and 2022."
    ))
  }

  if (any(index %in% c("svi14", "svi20")) == TRUE & min(year) < 2012) {
    cli::cli_abort(c(
      "The {.arg year} value is not valid for 2014 or 2020 SVI specifications.",
      "i" = "Each of those can be calculated from 2012 onward. Use {.val svi10} for 2010 or 2011."
    ))
  }

  if (any(index == "svi20s") == TRUE & min(year) < 2019) {
    cli::cli_abort(c(
      "The {.arg year} value is not valid for the 2020 SVI specification with the alternate single parent measure.",
      "i" = "This can only be calculated for 2019 onward."
    ))
  }

  # survey
  if (survey %in% c("acs1", "acs3", "acs5") == FALSE) {
    cli::cli_abort(c(
      "The {.arg survey} value provided is not valid: {.val {survey}}.",
      "i" = "Choose one of: {.val acs1}, {.val acs3}, or {.val acs5}."
    ))
  }

  if (survey == "acs3" & max(year) > 2013) {
    cli::cli_abort(c(
      "The {.val acs3} survey was discontinued after 2013.",
      "i" = "Please select one of {.val acs1} or {.val acs5}."
    ))
  }

  # logical scalars
  if (is.logical(return_percentiles) == FALSE) {
    cli::cli_abort("Please provide a logical scalar for {.arg return_percentiles}.")
  }

  if (is.logical(keep_subscales) == FALSE) {
    cli::cli_abort("Please provide a logical scalar for {.arg keep_subscales}.")
  }

  if (is.logical(keep_components) == FALSE) {
    cli::cli_abort("Please provide a logical scalar for {.arg keep_components}.")
  }

  # output
  if (output %in% valid_output == FALSE) {
    cli::cli_abort(c(
      "The {.arg output} value provided is not valid: {.val {output}}.",
      "i" = "Choose one of: {.val {valid_output}}."
    ))
  }

  if (output == "tidy" & keep_components == TRUE) {
    cli::cli_abort(c(
      "The {.arg output} requested is invalid.",
      "i" = "Tidy output is only available if {.arg keep_components} is {.code FALSE}."
    ))
  }

  invisible(TRUE)
}


# the functions below are from the tigris package that are not exported
# https://github.com/walkerke/tigris/blob/master/R/utils.R
# used based on terms of the MIT License used by the package's author, Kyle Walker
# https://github.com/walkerke/tigris/blob/master/DESCRIPTION

# validate state
validate_state <- function(state, .msg=interactive()) {

  # global variables
  simpleCapSO = NULL

  # original tigris function
  if (is.null(state)) return(NULL)

  state <- tolower(trimws(state)) # forgive white space

  if (grepl("^[[:digit:]]+$", state)) { # we prbly have FIPS

    state <- sprintf("%02d", as.numeric(state)) # forgive 1-digit FIPS codes

    if (state %in% states_lookup$fips) {
      return(state)
    } else {
      # perhaps they passed in a county FIPS by accident so forgive that, too,
      # but warn the caller
      state_sub <- substr(state, 1, 2)
      if (state_sub %in% states_lookup$fips) {
        cli::cli_inform("Using first two digits of {state} - {.val {state_sub}} ({states_lookup[states_lookup$fips == state_sub, 'name']}) - for FIPS code.")
        return(state_sub)
      } else {
        cli::cli_warn("{.val {state}} is not a valid FIPS code or state name/abbreviation.")
        return(NULL)
      }
    }

  } else if (grepl("^[[:alpha:]]+", state)) { # we might have state abbrev or name

    if (nchar(state) == 2 & state %in% states_lookup$abb) { # yay, an abbrev!

      if (.msg)
        cli::cli_inform("Using FIPS code {.val {states_lookup[states_lookup$abb == state, 'fips']}} for state {.val {toupper(state)}}.")
      return(states_lookup[states_lookup$abb == state, "fips"])

    } else if (nchar(state) > 2 & state %in% states_lookup$name) { # yay, a name!

      if (.msg)
        cli::cli_inform("Using FIPS code {.val {states_lookup[states_lookup$name == state, 'fips']}} for state {.val {simpleCapSO(state)}}.")
      return(states_lookup[states_lookup$name == state, "fips"])

    } else {
      cli::cli_warn("{.val {state}} is not a valid FIPS code or state name/abbreviation.")
      return(NULL)
    }

  } else {
    cli::cli_warn("{.val {state}} is not a valid FIPS code or state name/abbreviation.")
    return(NULL)
  }

}

# Capitalization
simpleCapSO <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
