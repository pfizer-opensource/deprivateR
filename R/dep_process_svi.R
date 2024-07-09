# Processing Function for SVI #####

dep_process_svi <- function(.data, style, geography, year, survey,
                            keep_subscales, keep_components, return_percentiles,
                            multi_svi, debug){

  ## subscale creation
  if ("warnings" %in% debug == TRUE){

    if (keep_components == TRUE){
      pri <- dep_process_svi_pri(.data, style = style, geography = geography,
                                 year = year, survey = survey)
    }

    theme1 <- dep_process_svi_ses(.data, style = style, geography = geography,
                                  year = year,survey = survey, keep_components = keep_components)
    theme2 <- dep_process_svi_hhd(.data, style = style, geography = geography, year = year,
                                  survey = survey, keep_components = keep_components)
    theme3 <- dep_process_svi_msl(.data, style = style, geography = geography, year = year,
                                  survey = survey, keep_components = keep_components)
    theme4 <- dep_process_svi_htt(.data, style = style, geography = geography, year = year,
                                  survey = survey, keep_components = keep_components)

  } else if ("warnings" %in% debug == FALSE){
    if (keep_components == TRUE){
      pri <- suppressWarnings(dep_process_svi_pri(.data, style = style, geography = geography, year = year,
                                                  survey = survey))
    }

    theme1 <- suppressWarnings(dep_process_svi_ses(.data, style = style, geography = geography, year = year,
                                                   survey = survey, keep_components = keep_components))
    theme2 <- suppressWarnings(dep_process_svi_hhd(.data, style = style, geography = geography, year = year,
                                                   survey = survey, keep_components = keep_components))
    theme3 <- suppressWarnings(dep_process_svi_msl(.data, style = style, geography = geography, year = year,
                                  survey = survey, keep_components = keep_components))
    theme4 <- suppressWarnings(dep_process_svi_htt(.data, style = style, geography = geography, year = year,
                                  survey = survey, keep_components = keep_components))
  }

  ## combine subscales
  if (keep_components == TRUE){

    out <- merge(x = pri, y = theme1, by = "GEOID", all.x = TRUE)
    out <- merge(x = out, y = theme2, by = "GEOID", all.x = TRUE)

  } else if (keep_components == FALSE){

    out <- merge(x = theme1, y = theme2, by = "GEOID", all.x = TRUE)

  }

  out <- merge(x = out, y = theme3, by = "GEOID", all.x = TRUE)
  out <- merge(x = out, y = theme4, by = "GEOID", all.x = TRUE)

  ## calculate svi
  out$SPL_THEMES <- out$SPL_THEME1 + out$SPL_THEME2 + out$SPL_THEME3 + out$SPL_THEME4

  ## calculate svi percentile
  out$SVI <- dep_percent_rank(out$SPL_THEMES)

  if (return_percentiles == TRUE){
    out$SVI <- out$SVI*100
  }

  ## format output
  final_vars <- c("GEOID", "SVI", "SVI_SES", "SVI_HCD", "SVI_MSL", "SVI_HTT")

  if (keep_components == FALSE){

    if (keep_subscales == FALSE){

      out <- subset(out, select = c("GEOID", "SVI"))

    } else if (keep_subscales == TRUE){

      out <- subset(out, select = final_vars)

      if (return_percentiles == TRUE){

        out$SVI_SES <- out$SVI_SES*100
        out$SVI_HCD <- out$SVI_HCD*100
        out$SVI_MSL <- out$SVI_MSL*100
        out$SVI_HTT <- out$SVI_HTT*100
      }

    }

  } else if (keep_components == TRUE){

    ## store other variable names in vector
    other_vars <- names(out)[names(out) %in% final_vars == FALSE]

    ## combine with final variable vector
    final_vars <- c(final_vars, other_vars)

    ## subset
    out <- subset(out, select = final_vars)

  }

  ## create unique variable names
  if (multi_svi == TRUE){

    ## get year of style to create postfix
    if (style != "svi20s"){
      post <- paste0("_",substr(style, 4, 5))
    } else if (style == "svi20s"){
      post <- paste0("_",toupper(substr(style, 4, 6)))
    }

    ## update columns that need to remain unique with postfix
    names(out)[names(out) == "SVI"] <- paste0("SVI", post)

    if (keep_subscales == TRUE){
      names(out)[names(out) == "SVI_SES"] <- paste0("SVI_SES", post)
      names(out)[names(out) == "SVI_HTT"] <- paste0("SVI_HTT", post)

      if (style %in% c("svi20", "svi20s") == TRUE){
        names(out)[names(out) == "SVI_HCD"] <- paste0("SVI_HOU", post)
        names(out)[names(out) == "SVI_MSL"] <- paste0("SVI_REM", post)
      } else if (style %in% c("svi10", "svi14") == TRUE){
        names(out)[names(out) == "SVI_HCD"] <- paste0("SVI_HCD", post)
        names(out)[names(out) == "SVI_MSL"] <- paste0("SVI_MSL", post)
      }
    }

    if (keep_components == TRUE){
      names(out)[names(out) == "SP_THEME1"] <- paste0("SP_THEME1", post)
      names(out)[names(out) == "SP_THEME2"] <- paste0("SP_THEME2", post)
      names(out)[names(out) == "SP_THEME3"] <- paste0("SP_THEME3", post)
      names(out)[names(out) == "SP_THEME4"] <- paste0("SP_THEME4", post)
      names(out)[names(out) == "SP_THEMES"] <- paste0("SP_THEMES", post)
    }

  } else if (multi_svi == FALSE){
    if (style %in% c("svi20", "svi20s") == TRUE & keep_subscales == TRUE){
      names(out)[names(out) == "SVI_HCD"] <- "SVI_HOU"
      names(out)[names(out) == "SVI_MSL"] <- "SVI_REM"
    }
  }

  ## return output
  return(out)

}

### svi, primary variables
dep_process_svi_pri <- function(.data, style, geography, year, survey){

  ## create variable list
  varlist <- dep_expand_varlist(geography = geography, index = paste0(style, ", pri"),
                                year = year, survey = survey)
  varlist <- c("GEOID", varlist)

  ## subset
  .data <- subset(.data, select = varlist)

  ## rename components
  names(.data)[names(.data) == "S1101_C01_001E"] <- "E_HH"
  names(.data)[names(.data) == "S1101_C01_001M"] <- "M_HH"
  names(.data)[names(.data) == "DP04_0001E"] <- "E_HU"
  names(.data)[names(.data) == "DP04_0001M"] <- "M_HU"
  names(.data)[names(.data) == "S0101_C01_001E"] <- "E_TOTPOP"
  names(.data)[names(.data) == "S0101_C01_001M"] <- "M_TOTPOP"

  ## update output order
  .data <- subset(.data, select = c(GEOID, E_TOTPOP, M_TOTPOP,
                                    E_HU, M_HU, E_HH, M_HH))

  ## return output
  return(.data)

}

### svi, ses variables
dep_process_svi_ses <- function(.data, style, geography, year, survey, keep_components){

  ## create variable list
  varlist <- dep_expand_varlist(geography = geography, index = paste0(style, ", ses"),
                                year = year, survey = survey)

  varlist <- c("GEOID", varlist)

  ## subset
  out <- subset(.data, select = varlist)

  ## rename components
  if (year < 2015){
    names(out)[names(out) == "B15002_001E"] <- "D_NOHSDP"
    names(out)[names(out) == "B15002_001M"] <- "DM_NOHSDP"
  } else if (year >= 2015){
    names(out)[names(out) == "S1501_C01_006E"] <- "D_NOHSDP"
    names(out)[names(out) == "S1501_C01_006M"] <- "DM_NOHSDP"
  }

  names(out)[names(out) == "DP03_0003E"] <- "D_UNEMP"
  names(out)[names(out) == "DP03_0003M"] <- "DM_UNEMP"
  names(out)[names(out) == "DP03_0005E"] <- "E_UNEMP"
  names(out)[names(out) == "DP03_0005M"] <- "M_UNEMP"

  if (style %in% c("svi10", "svi14") == TRUE){

    names(out)[names(out) == "B17001_001E"] <- "D_POV"
    names(out)[names(out) == "B17001_001M"] <- "DM_POV"
    names(out)[names(out) == "B17001_002E"] <- "E_POV"
    names(out)[names(out) == "B17001_002M"] <- "M_POV"
    names(out)[names(out) == "B19301_001E"] <- "E_PCI"
    names(out)[names(out) == "B19301_001M"] <- "M_PCI"

  } else if (style %in% c("svi20", "svi20s") == TRUE){

    names(out)[names(out) == "S1701_C01_001E"] <- "D_POV150"
    names(out)[names(out) == "S1701_C01_001M"] <- "DM_POV150"
    names(out)[names(out) == "S2503_C01_001E"] <- "D_HBURD"
    names(out)[names(out) == "S2503_C01_001M"] <- "DM_HBURD"
    names(out)[names(out) == "S2701_C01_001E"] <- "D_UNINSUR"
    names(out)[names(out) == "S2701_C01_001M"] <- "DM_UNINSUR"

    if (year < 2015){

      names(out)[names(out) == "S1701_C01_036E"] <- "E_POV150"
      names(out)[names(out) == "S1701_C01_036M"] <- "M_POV150"

      if (year == 2012){

        names(out)[names(out) == "S2701_C02_001E"] <- "E_UNINSUR"
        names(out)[names(out) == "S2701_C02_001M"] <- "M_UNINSUR"

      } else if (year %in% c(2013, 2014) == TRUE){

        names(out)[names(out) == "S2701_C04_062E"] <- "E_UNINSUR"
        names(out)[names(out) == "S2701_C04_062M"] <- "M_UNINSUR"

      }

    } else if (year >= 2015){

      names(out)[names(out) == "S1701_C01_040E"] <- "E_POV150"
      names(out)[names(out) == "S1701_C01_040M"] <- "M_POV150"
      names(out)[names(out) == "S2701_C04_001E"] <- "E_UNINSUR"
      names(out)[names(out) == "S2701_C04_001M"] <- "M_UNINSUR"

    }

  }

  ## calculate components
  if (year < 2015){

    ### create variable list
    varlist <- dep_expand_varlist(geography = geography, index = paste0(style, ", edu"),
                                  year = year, survey = survey)
    varlist_subset <- c("GEOID", varlist)

    ### subset estimates
    edu <- subset(.data, select = varlist_subset)

    ### pivot and rename
    edu <- pivot_demos(edu, vars = varlist)
    names(edu)[names(edu) == "estimate"] <- "E_NOHSDP"
    names(edu)[names(edu) == "moe"] <- "M_NOHSDP"

    ### combine
    out <- merge(x = out, y = edu, by = "GEOID", all.x = TRUE)

  } else if (year >= 2015){

    out$E_NOHSDP <- out$S1501_C01_007E+out$S1501_C01_008E
    out$M_NOHSDP <- sqrt(out$S1501_C01_007M^2+out$S1501_C01_008M^2)

  }

  if (style %in% c("svi20", "svi20s") == TRUE){

    if (year < 2015){
      out$E_HBURD <- out$S2503_C01_032E+out$S2503_C01_036E+out$S2503_C01_040E+out$S2503_C01_044E
      out$M_HBURD <- sqrt(out$S2503_C01_032M^2+out$S2503_C01_036M^2+out$S2503_C01_040M^2+out$S2503_C01_044M^2)
    } else if (year >= 2015){
      out$E_HBURD <- out$S2503_C01_028E+out$S2503_C01_032E+out$S2503_C01_036E+out$S2503_C01_040E
      out$M_HBURD <- sqrt(out$S2503_C01_028M^2+out$S2503_C01_032M^2+out$S2503_C01_036M^2+out$S2503_C01_040M^2)
    }

  }

  ## calculate metrics
  out$EP_NOHSDP <- out$E_NOHSDP/out$D_NOHSDP*100
  out$MP_NOHSDP <- ((sqrt(out$M_NOHSDP^2-((out$EP_NOHSDP/100)^2*out$DM_NOHSDP^2)))/out$DM_NOHSDP)*100

  out$EP_UNEMP <- out$E_UNEMP/out$D_UNEMP*100
  out$MP_UNEMP <- ((sqrt(out$M_UNEMP^2-((out$EP_UNEMP/100)^2*out$DM_UNEMP^2)))/out$DM_UNEMP)*100

  if (style %in% c("svi10", "svi14") == TRUE){

    out$EP_POV <- out$E_POV/out$D_POV*100
    out$MP_POV <- ((sqrt(out$M_POV^2-((out$EP_POV/100)^2*out$DM_POV^2)))/out$DM_POV)*100

  } else if (style %in% c("svi20", "svi20s") == TRUE){

    out$EP_POV150 <- out$E_POV150/out$D_POV150*100
    out$MP_POV150 <- ((sqrt(out$M_POV150^2-((out$EP_POV150/100)^2*out$DM_POV150^2)))/out$DM_POV150)*100

    out$EP_UNINSUR <- out$E_UNINSUR/out$D_UNINSUR*100
    out$MP_UNINSUR <- ((sqrt(out$M_UNINSUR^2-((out$EP_UNINSUR/100)^2*out$DM_UNINSUR^2)))/out$DM_UNINSUR)*100

    if (year < 2017){
      out$EP_HBURD <- out$E_HBURD
      out$MP_HBURD <- out$M_HBURD
    } else if (year >= 2017){
      out$EP_HBURD <- out$E_HBURD/out$D_HBURD*100
      out$MP_HBURD <- ((sqrt(out$M_HBURD^2-((out$EP_HBURD/100)^2*out$DM_HBURD^2)))/out$DM_HBURD)*100
    }

  }

  ## calculate percentiles
  out$EPL_NOHSDP <- dep_percent_rank(out$EP_NOHSDP)
  out$EPL_UNEMP <- dep_percent_rank(out$EP_UNEMP)

  if (style %in% c("svi10", "svi14") == TRUE){

    out$EPL_POV <- dep_percent_rank(out$EP_POV)
    out$EPL_PCI <- 1-dep_percent_rank(out$E_PCI)

  } else if (style %in% c("svi20", "svi20s") == TRUE){

    out$EPL_POV150 <- dep_percent_rank(out$EP_POV150)
    out$EPL_UNINSUR <- 1-dep_percent_rank(out$EP_UNINSUR)
    out$EPL_HBURD <- 1-dep_percent_rank(out$EP_HBURD)

  }

  ## calculate theme
  if (style %in% c("svi10", "svi14") == TRUE){
    out$SPL_THEME1 <- out$EPL_POV + out$EPL_UNEMP + out$EPL_PCI + out$EPL_NOHSDP
  } else if (style %in% c("svi20", "svi20s") == TRUE){
    out$SPL_THEME1 <- out$EPL_POV150 + out$EPL_UNEMP + out$EPL_NOHSDP + out$EPL_UNINSUR + out$EPL_HBURD
  }

  ## calculate theme percentile
  out$SVI_SES <- dep_percent_rank(out$SPL_THEME1)

  ## update output order
  if (keep_components == TRUE){

    if (style %in% c("svi10", "svi14") == TRUE){
      out <- subset(out, select = c(GEOID,
                                    D_POV, DM_POV, E_POV, M_POV, EP_POV, MP_POV, EPL_POV,
                                    D_UNEMP, DM_UNEMP, E_UNEMP, M_UNEMP, EP_UNEMP, MP_UNEMP, EPL_UNEMP,
                                    E_PCI, M_PCI, EPL_PCI,
                                    D_NOHSDP, DM_NOHSDP, E_NOHSDP, M_NOHSDP, EP_NOHSDP, MP_NOHSDP, EPL_NOHSDP,
                                    SPL_THEME1, SVI_SES))
    } else if (style %in% c("svi20", "svi20s") == TRUE){

      if (year < 2017){
        out <- subset(out, select = c(GEOID,
                                      D_POV150, DM_POV150, E_POV150, M_POV150, EP_POV150, MP_POV150, EPL_POV150,
                                      D_UNEMP, DM_UNEMP, E_UNEMP, M_UNEMP, EP_UNEMP, MP_UNEMP, EPL_UNEMP,
                                      E_HBURD, M_HBURD, EPL_HBURD,
                                      D_NOHSDP, DM_NOHSDP, E_NOHSDP, M_NOHSDP, EP_NOHSDP, MP_NOHSDP, EPL_NOHSDP,
                                      D_UNINSUR, DM_UNINSUR, E_UNINSUR, M_UNINSUR, EP_UNINSUR, MP_UNINSUR, EPL_UNINSUR,
                                      SPL_THEME1, SVI_SES))
      } else if (year >= 2017){
        out <- subset(out, select = c(GEOID,
                                      D_POV150, DM_POV150, E_POV150, M_POV150, EP_POV150, MP_POV150, EPL_POV150,
                                      D_UNEMP, DM_UNEMP, E_UNEMP, M_UNEMP, EP_UNEMP, MP_UNEMP, EPL_UNEMP,
                                      E_HBURD, M_HBURD, EP_HBURD, MP_HBURD, EPL_HBURD,
                                      D_NOHSDP, DM_NOHSDP, E_NOHSDP, M_NOHSDP, EP_NOHSDP, MP_NOHSDP, EPL_NOHSDP,
                                      D_UNINSUR, DM_UNINSUR, E_UNINSUR, M_UNINSUR, EP_UNINSUR, MP_UNINSUR, EPL_UNINSUR,
                                      SPL_THEME1, SVI_SES))
      }

    }

  } else if (keep_components == FALSE){

    out <- subset(out, select = c(GEOID, SPL_THEME1, SVI_SES))

  }

  ## return output
  return(out)

}

### svi, ses variables
dep_process_svi_hhd <- function(.data, style, geography, year, survey, keep_components){

  ## create variable list
  varlist <- dep_expand_varlist(geography = geography, index = paste0(style, ", hhd"),
                                year = year, survey = survey)
  varlist <- c("GEOID", varlist)

  ## subset
  out <- subset(.data, select = varlist)

  ## rename components
  names(out)[names(out) == "S1101_C01_001E"] <- "D_SNGPNT"
  names(out)[names(out) == "S1101_C01_001M"] <- "DM_SNGPNT"

  names(out)[names(out) == "S0101_C01_001E"] <- "D_AGE"
  names(out)[names(out) == "S0101_C01_001M"] <- "DM_AGE"

  if (year >= 2017){
    names(out)[names(out) == "S0101_C01_022E"] <- "E_AGE17"
    names(out)[names(out) == "S0101_C01_022M"] <- "M_AGE17"
    names(out)[names(out) == "S0101_C01_030E"] <- "E_AGE65"
    names(out)[names(out) == "S0101_C01_030M"] <- "M_AGE65"
  }

  if (style %in% c("svi14", "svi20", "svi20s") == TRUE){
    names(out)[names(out) == "S2701_C01_001E"] <- "D_DISABL"
    names(out)[names(out) == "S2701_C01_001M"] <- "DM_DISABL"
  }

  if (style %in% c("svi20", "svi20s") == TRUE){
    names(out)[names(out) == "B16004_001E"] <- "D_LIMENG"
    names(out)[names(out) == "B16004_001M"] <- "DM_LIMENG"
  }

  ## calculate components
  if (style %in% c("svi10", "svi14", "svi20") == TRUE){
    out$E_SNGPNT <- out$B25115_009E+out$B25115_012E+out$B25115_022E+out$B25115_025E
    out$M_SNGPNT <- sqrt(out$B25115_009M^2+out$B25115_012M^2+out$B25115_022M^2+out$B25115_025M^2)
  } else if (style == "svi20s"){
    out$E_SNGPNT <- out$B11012_010E+out$B11012_015E
    out$M_SNGPNT <- sqrt(out$B11012_010M^2+out$B11012_015M^2)
  }

  if (year < 2017){

    ### create variable list
    varlist <- dep_expand_varlist(geography = geography, index = paste0(style, ", age_lt18"),
                                  year = year, survey = survey)
    varlist_subset <- c("GEOID", varlist)

    ### subset estimates
    age_lt18 <- subset(.data, select = varlist_subset)

    ### pivot and rename
    age_lt18 <- pivot_demos(age_lt18, vars = varlist)
    names(age_lt18)[names(age_lt18) == "estimate"] <- "E_AGE17"
    names(age_lt18)[names(age_lt18) == "moe"] <- "M_AGE17"

    ### create variable list
    varlist <- dep_expand_varlist(geography = geography, index = paste0(style, ", age_gt64"),
                                  year = year, survey = survey)
    varlist_subset <- c("GEOID", varlist)

    ### subset estimates
    age_gt64 <- subset(.data, select = varlist_subset)

    ### pivot and rename
    age_gt64 <- pivot_demos(age_gt64, vars = varlist)
    names(age_gt64)[names(age_gt64) == "estimate"] <- "E_AGE65"
    names(age_gt64)[names(age_gt64) == "moe"] <- "M_AGE65"

    ### combine
    age <- merge(x = age_lt18, y = age_gt64, by = "GEOID", all.x = TRUE)
    out <- merge(x = out, y = age, by = "GEOID", all.x = TRUE)

  }

  if (style %in% c("svi14", "svi20", "svi20s") == TRUE){

    ### create variable list
    varlist <- dep_expand_varlist(geography = geography, index = paste0(style, ", dis"),
                                  year = year, survey = survey)
    varlist_subset <- c("GEOID", varlist)

    ### subset estimates
    dis <- subset(.data, select = varlist_subset)

    ### pivot and rename
    dis <- pivot_demos(dis, vars = varlist)
    names(dis)[names(dis) == "estimate"] <- "E_DISABL"
    names(dis)[names(dis) == "moe"] <- "M_DISABL"

    ### combine
    out <- merge(x = out, y = dis, by = "GEOID", all.x = TRUE)

  }

  if (style %in% c("svi20", "svi20s") == TRUE){

    ### create variable list
    varlist <- dep_expand_varlist(geography = geography, index = paste0(style, ", eng"),
                                  year = year, survey = survey)
    varlist_subset <- c("GEOID", varlist)

    ### subset estimates
    eng <- subset(.data, select = varlist_subset)

    ### pivot and rename
    eng <- pivot_demos(eng, vars = varlist)
    names(eng)[names(eng) == "estimate"] <- "E_LIMENG"
    names(eng)[names(eng) == "moe"] <- "M_LIMENG"

    ### combine
    out <- merge(x = out, y = eng, by = "GEOID", all.x = TRUE)

  }

  ## calculate metrics
  out$EP_AGE17 <- out$E_AGE17/out$D_AGE*100
  out$MP_AGE17 <- ((sqrt(out$M_AGE17^2-((out$EP_AGE17/100)^2*out$DM_AGE^2)))/out$DM_AGE)*100

  out$EP_AGE65 <- out$E_AGE65/out$D_AGE*100
  out$MP_AGE65 <- ((sqrt(out$M_AGE65^2-((out$EP_AGE65/100)^2*out$DM_AGE^2)))/out$DM_AGE)*100

  out$EP_SNGPNT <- out$E_SNGPNT/out$D_SNGPNT*100
  out$MP_SNGPNT <- ((sqrt(out$M_SNGPNT^2-((out$EP_SNGPNT/100)^2*out$DM_SNGPNT^2)))/out$DM_SNGPNT)*100

  if (style %in% c("svi14", "svi20", "svi20s") == TRUE){
    out$EP_DISABL <- out$E_DISABL/out$D_DISABL*100
    out$MP_DISABL <- ((sqrt(out$M_DISABL^2-((out$EP_DISABL/100)^2*out$DM_DISABL^2)))/out$DM_DISABL)*100
  }

  if (style %in% c("svi20", "svi20s") == TRUE){
    out$EP_LIMENG <- out$E_LIMENG/out$D_LIMENG*100
    out$MP_LIMENG <- ((sqrt(out$M_LIMENG^2-((out$EP_LIMENG/100)^2*out$DM_LIMENG^2)))/out$DM_LIMENG)*100
  }

  ## calculate percentiles
  out$EPL_AGE17 <- dep_percent_rank(out$EP_AGE17)
  out$EPL_AGE65 <- dep_percent_rank(out$EP_AGE65)
  out$EPL_SNGPNT <- dep_percent_rank(out$EP_SNGPNT)

  if (style %in% c("svi14", "svi20", "svi20s") == TRUE){
    out$EPL_DISABL <- dep_percent_rank(out$EP_DISABL)
  }

  if (style %in% c("svi20", "svi20s") == TRUE){
    out$EPL_LIMENG <- dep_percent_rank(out$EP_LIMENG)
  }

  ## calculate theme
  if (style == "svi10"){
    out$SPL_THEME2 <- out$EPL_AGE17 + out$EPL_AGE65 + out$EPL_SNGPNT
  } else if (style == "svi14"){
    out$SPL_THEME2 <- out$EPL_AGE17 + out$EPL_AGE65 + out$EPL_DISABL + out$EPL_SNGPNT
  } else if (style %in% c("svi20", "svi20s") == TRUE){
    out$SPL_THEME2 <- out$EPL_AGE17 + out$EPL_AGE65 + out$EPL_DISABL + out$EPL_SNGPNT + out$EPL_LIMENG
  }

  ## calculate theme percentile
  out$SVI_HCD <- dep_percent_rank(out$SPL_THEME2)

  ## update output order
  if (keep_components == TRUE){

    if (style == "svi10"){

      out <- subset(out, select = c(GEOID, E_AGE17, M_AGE17, EP_AGE17, MP_AGE17, EPL_AGE17,
                                        E_AGE65, M_AGE65, EP_AGE65, MP_AGE65, EPL_AGE65,
                                        E_SNGPNT, M_SNGPNT, EP_SNGPNT, MP_SNGPNT, EPL_SNGPNT,
                                        SPL_THEME2, SVI_HCD))

    } else if (style == "svi14"){

      out <- subset(out, select = c(GEOID, E_AGE17, M_AGE17, EP_AGE17, MP_AGE17, EPL_AGE17,
                                        E_AGE65, M_AGE65, EP_AGE65, MP_AGE65, EPL_AGE65,
                                        D_DISABL, DM_DISABL, E_DISABL, M_DISABL, EP_DISABL, MP_DISABL, EPL_DISABL,
                                        E_SNGPNT, M_SNGPNT, EP_SNGPNT, MP_SNGPNT, EPL_SNGPNT,
                                        SPL_THEME2, SVI_HCD))

    } else if (style %in% c("svi20", "svi20s") == TRUE){

      out <- subset(out, select = c(GEOID, E_AGE17, M_AGE17, EP_AGE17, MP_AGE17, EPL_AGE17,
                                        E_AGE65, M_AGE65, EP_AGE65, MP_AGE65, EPL_AGE65,
                                        E_DISABL, M_DISABL, EP_DISABL, MP_DISABL, EPL_DISABL,
                                        E_SNGPNT, M_SNGPNT, EP_SNGPNT, MP_SNGPNT, EPL_SNGPNT,
                                        D_LIMENG, DM_LIMENG, E_LIMENG, M_LIMENG, EP_LIMENG, MP_LIMENG, EPL_LIMENG,
                                        SPL_THEME2, SVI_HCD))

    }

  } else if (keep_components == FALSE){
    out <- subset(out, select = c(GEOID, SPL_THEME2, SVI_HCD))
  }

  ## return output
  return(out)

}

### svi, msl variables
dep_process_svi_msl <- function(.data, style, geography, year, survey, keep_components){

  ## create variable list
  varlist <- dep_expand_varlist(geography = geography, index = paste0(style, ", msl"),
                                year = year, survey = survey)
  varlist <- c("GEOID", varlist)

  ## subset
  out <- subset(.data, select = varlist)

  ## rename components
  names(out)[names(out) == "S0101_C01_001E"] <- "D_MINRTY"
  names(out)[names(out) == "S0101_C01_001M"] <- "DM_MINRTY"

  if (style %in% c("svi10", "svi14") == TRUE){
    names(out)[names(out) == "B16004_001E"] <- "D_LIMENG"
    names(out)[names(out) == "B16004_001M"] <- "DM_LIMENG"
  }

  ### calculate components
  out$E_MINRTY <- out$D_MINRTY-out$B01001H_001E
  out$M_MINRTY <- sqrt(out$DM_MINRTY^2+out$B01001H_001M^2)

  if (style %in% c("svi10", "svi14") == TRUE){

    ### create variable list
    varlist <- dep_expand_varlist(geography = geography, index = paste0(style, ", eng"),
                                  year = year, survey = survey)
    varlist_subset <- c("GEOID", varlist)

    ### subset estimates
    eng <- subset(.data, select = varlist_subset)

    ### pivot and rename
    eng <- pivot_demos(eng, vars = varlist)
    names(eng)[names(eng) == "estimate"] <- "E_LIMENG"
    names(eng)[names(eng) == "moe"] <- "M_LIMENG"

    ### combine
    out <- merge(x = out, y = eng, by = "GEOID", all.x = TRUE)

  }

  ### calculate metrics
  out$EP_MINRTY <- out$E_MINRTY/out$D_MINRTY*100
  out$MP_MINRTY <- ((sqrt(out$M_MINRTY^2-((out$EP_MINRTY/100)^2*out$DM_MINRTY^2)))/out$DM_MINRTY)*100

  if (style %in% c("svi10", "svi14")){
    out$EP_LIMENG <- out$E_LIMENG/out$D_LIMENG*100
    out$MP_LIMENG <- ((sqrt(out$M_LIMENG^2-((out$EP_LIMENG/100)^2*out$DM_LIMENG^2)))/out$DM_LIMENG)*100
  }

  ## calculate percentiles
  out$EPL_MINRTY <- dep_percent_rank(out$EP_MINRTY)

  if (style %in% c("svi10", "svi14") == TRUE){
    out$EPL_LIMENG <- dep_percent_rank(out$EP_LIMENG)
  }

  ### calculate theme
  if (style %in% c("svi10", "svi14") == TRUE){
    out$SPL_THEME3 <- out$EPL_MINRTY + out$EPL_LIMENG
  } else if (style %in% c("svi20", "svi20s") == TRUE){
    out$SPL_THEME3 <- out$EPL_MINRTY
  }

  ## calculate theme percentile
  if (style %in% c("svi10", "svi14") == TRUE){
    out$SVI_MSL <- dep_percent_rank(out$SPL_THEME3)
  } else if (style %in% c("svi20", "svi20s") == TRUE){
    out$SVI_MSL <- out$SPL_THEME3
  }

  ### update output order
  if (keep_components == TRUE){

    if (style %in% c("svi10", "svi14")){

      out <- subset(out, select = c(GEOID, E_MINRTY, M_MINRTY, EP_MINRTY, MP_MINRTY, EPL_MINRTY,
                                    D_LIMENG, DM_LIMENG, E_LIMENG, M_LIMENG, EP_LIMENG, MP_LIMENG, EPL_LIMENG,
                                    SPL_THEME3, SVI_MSL))

    } else if (style %in% c("svi20", "svi20s") == TRUE){

      out <- subset(out, select = c(GEOID, E_MINRTY, M_MINRTY, EP_MINRTY, MP_MINRTY, EPL_MINRTY,
                                    SPL_THEME3, SVI_MSL))

    }

  } else if (keep_components == FALSE){

    out <- subset(out, select = c(GEOID, SPL_THEME3, SVI_MSL))

  }

  ## return output
  return(out)

}

### svi, htt variables
dep_process_svi_htt <- function(.data, style, geography, year, survey, keep_components){

  ## create variable list
  varlist <- dep_expand_varlist(geography = geography, index = paste0(style, ", htt"),
                                year = year, survey = survey)
  varlist <- c("GEOID", varlist)

  ## subset
  .data <- subset(.data, select = varlist)

  ## rename components
  names(.data)[names(.data) == "B26001_001E"] <- "E_GROUPQ"
  names(.data)[names(.data) == "B26001_001M"] <- "M_GROUPQ"
  names(.data)[names(.data) == "DP04_0002E"] <- "D_CROWD"
  names(.data)[names(.data) == "DP04_0002M"] <- "DM_CROWD"
  names(.data)[names(.data) == "DP04_0006E"] <- "D_HOUSE"
  names(.data)[names(.data) == "DP04_0006M"] <- "DM_HOUSE"
  names(.data)[names(.data) == "DP04_0014E"] <- "E_MOBILE"
  names(.data)[names(.data) == "DP04_0014M"] <- "M_MOBILE"

  if (year >= 2015){

    names(.data)[names(.data) == "DP04_0057E"] <- "D_NOVEH"
    names(.data)[names(.data) == "DP04_0057M"] <- "DM_NOVEH"
    names(.data)[names(.data) == "DP04_0058E"] <- "E_NOVEH"
    names(.data)[names(.data) == "DP04_0058M"] <- "M_NOVEH"

  } else if (year < 2015){

    names(.data)[names(.data) == "DP04_0056E"] <- "D_NOVEH"
    names(.data)[names(.data) == "DP04_0056M"] <- "DM_NOVEH"
    names(.data)[names(.data) == "DP04_0057E"] <- "E_NOVEH"
    names(.data)[names(.data) == "DP04_0057M"] <- "M_NOVEH"

  }

  ## calculate components
  .data$E_MUNIT <- .data$DP04_0012E+.data$DP04_0013E
  .data$M_MUNIT <- sqrt(.data$DP04_0012M^2+.data$DP04_0013M^2)

  if (year >= 2015){

    .data$E_CROWD <- .data$DP04_0078E+.data$DP04_0079E
    .data$M_CROWD <- sqrt(.data$DP04_0078M^2+.data$DP04_0079M^2)

  } else if (year < 2015){

    .data$E_CROWD <- .data$DP04_0077E+.data$DP04_0078E
    .data$M_CROWD <- sqrt(.data$DP04_0077M^2+.data$DP04_0078M^2)

  }

  ## calculate metrics
  .data$EP_MUNIT <- .data$E_MUNIT/.data$D_HOUSE*100
  .data$MP_MUNIT <- ((sqrt(.data$M_MUNIT^2-((.data$EP_MUNIT/100)^2*.data$DM_HOUSE^2)))/.data$DM_HOUSE)*100

  .data$EP_MOBILE <- .data$E_MOBILE/.data$D_HOUSE*100
  .data$MP_MOBILE <- ((sqrt(.data$M_MOBILE^2-((.data$EP_MOBILE/100)^2*.data$DM_HOUSE^2)))/.data$DM_HOUSE)*100

  .data$EP_CROWD <- .data$E_CROWD/.data$D_CROWD*100
  .data$MP_CROWD <- ((sqrt(.data$M_CROWD^2-((.data$EP_CROWD/100)^2*.data$DM_CROWD^2)))/.data$DM_CROWD)*100

  .data$EP_NOVEH <- .data$E_NOVEH/.data$D_NOVEH*100
  .data$MP_NOVEH <- ((sqrt(.data$M_NOVEH^2-((.data$EP_NOVEH/100)^2*.data$DM_NOVEH^2)))/.data$DM_NOVEH)*100

  .data$EP_GROUPQ <- .data$E_GROUPQ/.data$S0101_C01_001E*100
  .data$MP_GROUPQ <- ((sqrt(.data$M_GROUPQ^2-((.data$EP_GROUPQ/100)^2*.data$S0101_C01_001M^2)))/.data$S0101_C01_001M)*100

  ## calculate percentiles
  .data$EPL_MUNIT <- dep_percent_rank(.data$EP_MUNIT)
  .data$EPL_MOBILE <- dep_percent_rank(.data$EP_MOBILE)
  .data$EPL_CROWD <- dep_percent_rank(.data$EP_CROWD)
  .data$EPL_NOVEH <- dep_percent_rank(.data$EP_NOVEH)
  .data$EPL_GROUPQ <- dep_percent_rank(.data$EP_GROUPQ)

  ## calculate theme
  .data$SPL_THEME4 <- .data$EPL_MUNIT + .data$EPL_MOBILE + .data$EPL_CROWD + .data$EPL_NOVEH + .data$EPL_GROUPQ

  ## calculate theme percentile
  .data$SVI_HTT <- dep_percent_rank(.data$SPL_THEME4)

  ## update output order
  if (keep_components == TRUE){

    .data <- subset(.data, select = c(GEOID, E_MUNIT, M_MUNIT, EP_MUNIT, MP_MUNIT, EPL_MUNIT,
                                      E_MOBILE, M_MOBILE, EP_MOBILE, MP_MOBILE, EPL_MOBILE,
                                      E_CROWD, M_CROWD, EP_CROWD, MP_CROWD, EPL_CROWD,
                                      E_NOVEH, M_NOVEH, EP_NOVEH, MP_NOVEH, EPL_NOVEH,
                                      E_GROUPQ, M_GROUPQ, EP_GROUPQ, MP_GROUPQ, EPL_GROUPQ,
                                      SPL_THEME4, SVI_HTT))

  } else if (keep_components == FALSE){

    .data <- subset(.data, select = c(GEOID, SPL_THEME4, SVI_HTT))

  }

  ## return output
  return(.data)

}
