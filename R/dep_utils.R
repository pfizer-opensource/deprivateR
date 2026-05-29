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
