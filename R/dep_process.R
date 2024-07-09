# Process

dep_process <- function(.data, geography, index, year, survey,
                        return_percentiles, keep_subscales, keep_components,
                        state, county, puerto_rico, zcta, zcta_geo_method, zcta3_method,
                        geometry, cb, keep_geo_vars, shift_geo, key,
                        debug, input, output, label_year, multi_svi){

  # create vector of variables to pull
  varlist <- dep_build_varlist(geography = geography, index = index,
                               year = year, survey = survey, output = "vector")


  # optionally create demo and geo tables

  if (input == "download"){

    ## create geometry flag
    if (output == "sf"){
      geometry <- TRUE
    } else {
      geometry <- FALSE
    }

    ## create demo and geo tables
    demo <- dep_get_data(geography = geography, varlist = varlist, year = year,
                         survey = survey, state = state, county = county,
                         puerto_rico = puerto_rico,
                         zcta = zcta, zcta_geo_method = zcta_geo_method, zcta3_method = zcta3_method,
                         geometry = geometry, cb = cb, keep_geo_vars = keep_geo_vars,
                         shift_geo = shift_geo, key = key, debug = debug)

    geo <- demo$geo
    demo <- demo$demo

  } else if (input == "user"){

    ## prep user data
    geo <- subset(.data, select = "GEOID")
    demo <- .data

  }

  # optionally add year
  if (label_year == TRUE){
    geo$YEAR <- year
  }

  # create gini output
  if ("gini" %in% index == TRUE){
    out <- dep_process_gini(demo, geography = geography, year = year, survey = survey)
    out <- merge(x = geo, y = out, by = "GEOID", all.x = TRUE)
  } else if ("gini" %in% index == FALSE){
    out <- geo
  }

  # create svi output
  ## 2010 SVI style
  if ("svi10" %in% index == TRUE){

    ## build svi scores
    svi <- dep_process_svi(demo, style = "svi10",
                           geography = geography, year = year, survey = survey,
                           keep_subscales = keep_subscales,
                           keep_components = keep_components,
                           return_percentiles = return_percentiles,
                           multi_svi = multi_svi, debug = debug)

    ## combine with output
    out <- merge(x = out, y = svi, by = "GEOID", all.x = TRUE)
  }

  ## 2014 SVI style
  if ("svi14" %in% index == TRUE){

    ## build svi scores
    svi <- dep_process_svi(demo, style = "svi14",
                           geography = geography, year = year, survey = survey,
                           keep_subscales = keep_subscales,
                           keep_components = keep_components,
                           return_percentiles = return_percentiles,
                           multi_svi = multi_svi, debug = debug)

    ## optionally limit output to unique cols
    if (multi_svi == TRUE){
      excess_vars <- c("GEOID", setdiff(names(svi), names(out)))
      svi <- subset(svi, select = excess_vars)
    }

    ## combine with output
    out <- merge(x = out, y = svi, by = "GEOID", all.x = TRUE)

  }

  ## 2020 SVI style
  if ("svi20" %in% index == TRUE){

    ## build svi scores
    svi <- dep_process_svi(demo, style = "svi20",
                           geography = geography, year = year, survey = survey,
                           keep_subscales = keep_subscales,
                           keep_components = keep_components,
                           return_percentiles = return_percentiles,
                           multi_svi = multi_svi, debug = debug)

    ## optionally limit output to unique cols
    if (multi_svi == TRUE){
      excess_vars <- c("GEOID", setdiff(names(svi), names(out)))
      svi <- subset(svi, select = excess_vars)
    }

    ## combine with output
    out <- merge(x = out, y = svi, by = "GEOID", all.x = TRUE)
  }

  ## 2020 svi style with alternate single parent definition
  if ("svi20s" %in% index == TRUE){

    ## build svi scores
    svi <- dep_process_svi(demo, style = "svi20s",
                           geography = geography, year = year, survey = survey,
                           keep_subscales = keep_subscales,
                           keep_components = keep_components,
                           return_percentiles = return_percentiles,
                           multi_svi = multi_svi, debug = debug)

    ## optionally limit output to unique cols
    if (multi_svi == TRUE){
      excess_vars <- c("GEOID", setdiff(names(svi), names(out)))
      svi <- subset(svi, select = excess_vars)
    }

    ## combine with output
    out <- merge(x = out, y = svi, by = "GEOID", all.x = TRUE)
  }

  # create adi output
  if ("adi" %in% index == TRUE){
    adi <- dep_process_adi(demo, geography = geography, year = year, survey = survey,
                           keep_subscales = keep_subscales,
                           keep_components = keep_components,
                           return_percentiles = return_percentiles)
    out <- merge(x = out, y = adi, by = "GEOID", all.x = TRUE)
  }

  # create ndi output
  if ("ndi_m" %in% index == TRUE){
    ndi_m <- dep_process_ndi_m(demo, geography = geography, year = year, survey = survey,
                               keep_components = keep_components,
                               return_percentiles = return_percentiles)

    out <- merge(x = out, y = ndi_m, by = "GEOID", all.x = TRUE)
  }

  if ("ndi_pw" %in% index == TRUE){
    ndi_pw <- dep_process_ndi_pw(demo, geography = geography, year = year, survey = survey,
                                 keep_components = keep_components,
                                 return_percentiles = return_percentiles)
    out <- merge(x = out, y = ndi_pw, by = "GEOID", all.x = TRUE)
  }

  # prep output
  if (output == "wide"){

    out <- tibble::as_tibble(out)

  } else if (output == "tidy"){

    cols <- names(out)[names(out) %in% c("GEOID", "NAME") == FALSE]
    out <- tidyr::pivot_longer(out, cols = tidyr::all_of(cols), names_to = "index",
                               values_to = "estimate")

  }

  # fix id if zcta3
  if (geography == "zcta3"){
    names(out)[names(out) == "GEOID"] <- "ZCTA3"
  }

  # return output
  return(out)

}
