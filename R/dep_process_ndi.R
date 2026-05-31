# Processing Function for NDI #####

## Messer ####
dep_process_ndi_m <- function(.data, geography, year, survey, keep_components,
                              return_percentiles){

  ## create variable list
  varlist <- dep_expand_varlist(geography = geography, index = "ndi_m",
                                year = year, survey = survey)
  varlist <- c("GEOID", varlist)

  ## subset
  .data <- subset(.data, select = varlist)

  ## rename components
  names(.data)[names(.data) == "DP04_0002E"] <- "E_HH"
  names(.data)[names(.data) == "C24030_002E"] <- "D_MPRO"
  names(.data)[names(.data) == "B17017_002E"] <- "E_POVH"
  names(.data)[names(.data) == "B19058_002E"] <- "E_ASSIST"
  names(.data)[names(.data) == "B06009_001E"] <- "D_EDU"
  names(.data)[names(.data) == "B06009_002E"] <- "E_LTHS"
  names(.data)[names(.data) == "DP03_0003E"] <- "D_UNEMP"
  names(.data)[names(.data) == "DP03_0005E"] <- "E_UNEMP"

  ## calculate components
  .data$E_MPRO <- .data$C24030_018E+.data$C24030_019E
  .data$E_CROWD <- .data$DP04_0078E+.data$DP04_0079E
  .data$E_FFH <- .data$B25115_012E+.data$B25115_025E
  .data$E_LOWINC <- .data$B19001_002E+.data$B19001_003E+.data$B19001_004E+
    .data$B19001_005E+.data$B19001_006E

  ## calculate metrics
  .data$EP_MPRO <- dep_safe_pct(.data$E_MPRO, .data$D_MPRO)
  .data$EP_CROWD <- dep_safe_pct(.data$E_CROWD, .data$E_HH)
  .data$EP_POVH <- dep_safe_pct(.data$E_POVH, .data$E_HH)
  .data$EP_FFH <- dep_safe_pct(.data$E_FFH, .data$E_HH)
  .data$EP_ASSIST <- dep_safe_pct(.data$E_ASSIST, .data$E_HH)
  .data$EP_LOWINC <- dep_safe_pct(.data$E_LOWINC, .data$E_HH)
  .data$EP_LTHS <- dep_safe_pct(.data$E_LTHS, .data$D_EDU)
  .data$EP_UNEMP <- dep_safe_pct(.data$E_UNEMP, .data$D_UNEMP)

  ## prep for scoring
  ndi_m_score <- subset(.data, select = c(GEOID, EP_MPRO, EP_CROWD, EP_POVH, EP_FFH,
                                          EP_ASSIST, EP_LOWINC, EP_LTHS, EP_UNEMP))

  ## calculate ndi
  ### calculate
  ndi_m_score <- ndi::messer(geo = geography, year = year, df = ndi_m_score)[[1]]

  ### subset
  ndi_m_score <- subset(ndi_m_score, select = c(GEOID, NDI))

  ### rename
  names(ndi_m_score)[names(ndi_m_score) == "NDI"] <- "NDI_M"

  ### optionally convert to percentiles
  if (return_percentiles){
    ndi_m_score$NDI_M <- dep_percent_rank(ndi_m_score$NDI_M)*100
  }

  ## optionally add components back into data
  if (!keep_components){

    out <- ndi_m_score

  } else if (keep_components){

    ### rename moe components
    names(.data)[names(.data) == "DP04_0002M"] <- "M_HH"
    names(.data)[names(.data) == "C24030_002M"] <- "DM_MPRO"
    names(.data)[names(.data) == "B17017_002M"] <- "M_POVH"
    names(.data)[names(.data) == "B19058_002M"] <- "M_ASSIST"
    names(.data)[names(.data) == "B06009_001M"] <- "DM_EDU"
    names(.data)[names(.data) == "B06009_002M"] <- "M_LTHS"
    names(.data)[names(.data) == "DP03_0003M"] <- "DM_UNEMP"
    names(.data)[names(.data) == "DP03_0005M"] <- "M_UNEMP"

    ### calculate margins of error
    .data$M_MPRO <- sqrt(.data$C24030_018M^2+.data$C24030_019M^2)
    .data$M_CROWD <- sqrt(.data$DP04_0078M^2+.data$DP04_0079M^2)
    .data$M_FFH <- sqrt(.data$B25115_012M^2+.data$B25115_025M^2)
    .data$M_LOWINC <- sqrt(.data$B19001_002M^2+.data$B19001_003M^2+.data$B19001_004M^2+
                             .data$B19001_005M^2+.data$B19001_006M^2)

    .data$MP_MPRO <- dep_derived_moe(.data$M_MPRO, .data$EP_MPRO, .data$DM_MPRO, .data$DM_MPRO)
    .data$MP_CROWD <- dep_derived_moe(.data$M_CROWD, .data$EP_CROWD, .data$M_HH, .data$M_HH)
    .data$MP_POVH <- dep_derived_moe(.data$M_POVH, .data$EP_POVH, .data$M_HH, .data$M_HH)
    .data$MP_FFH <- dep_derived_moe(.data$M_FFH, .data$EP_FFH, .data$M_HH, .data$M_HH)
    .data$MP_ASSIST <- dep_derived_moe(.data$M_ASSIST, .data$EP_ASSIST, .data$M_HH, .data$M_HH)
    .data$MP_LOWINC <- dep_derived_moe(.data$M_LOWINC, .data$EP_LOWINC, .data$M_HH, .data$M_HH)
    .data$MP_LTHS <- dep_derived_moe(.data$M_LTHS, .data$EP_LTHS, .data$DM_EDU, .data$DM_EDU)
    .data$MP_UNEMP <- dep_derived_moe(.data$M_UNEMP, .data$EP_UNEMP, .data$DM_UNEMP, .data$DM_UNEMP)

    ### reorder variables
    .data <- subset(.data, select = c(GEOID, E_HH, M_HH,
                                      D_MPRO, DM_MPRO, E_MPRO, M_MPRO, EP_MPRO, MP_MPRO,
                                      E_CROWD, M_CROWD, EP_CROWD, MP_CROWD,
                                      E_POVH, M_POVH, EP_POVH, MP_POVH,
                                      E_FFH, M_FFH, EP_FFH, MP_FFH,
                                      E_ASSIST, M_ASSIST, EP_ASSIST, MP_ASSIST,
                                      E_LOWINC, M_LOWINC, EP_LOWINC, MP_LOWINC,
                                      D_EDU, DM_EDU, E_LTHS, M_LTHS, EP_LTHS, MP_LTHS,
                                      D_UNEMP, DM_UNEMP, E_UNEMP, M_UNEMP, EP_UNEMP, MP_UNEMP))

    ### combine with ndi
    out <- merge(ndi_m_score, .data, by = "GEOID")

  }

  ## return output
  return(out)

}

## Powell-Wiley ####
dep_process_ndi_pw <- function(.data, geography, year, survey, keep_components,
                               return_percentiles){

  ## create variable list
  varlist <- dep_expand_varlist(geography = geography, index = "ndi_pw",
                                year = year, survey = survey)
  varlist <- c("GEOID", varlist)

  ## subset
  .data <- subset(.data, select = varlist)

  ## rename components
  names(.data)[names(.data) == "S0101_C01_001E"] <- "E_TOTPOP"
  names(.data)[names(.data) == "DP04_0002E"] <- "E_HH"
  names(.data)[names(.data) == "B19013_001E"] <- "E_HHINC"
  names(.data)[names(.data) == "B25077_001E"] <- "E_HOMEV"
  names(.data)[names(.data) == "B19054_002E"] <- "E_INVINC"
  names(.data)[names(.data) == "B19058_002E"] <- "E_ASSIST"
  names(.data)[names(.data) == "C24060_001E"] <- "D_MANAGE"
  names(.data)[names(.data) == "C24060_002E"] <- "E_MANAGE"
  names(.data)[names(.data) == "DP04_0046E"] <- "E_OWNOCC"
  names(.data)[names(.data) == "DP04_0073E"] <- "E_NOPLUMB"
  names(.data)[names(.data) == "DP04_0075E"] <- "E_NOPHONE"
  names(.data)[names(.data) == "S1501_C01_006E"] <- "D_HIGHDEG"
  names(.data)[names(.data) == "S1501_C01_014E"] <- "E_HSPLUS"
  names(.data)[names(.data) == "S1501_C01_015E"] <- "E_BAPLUS"
  names(.data)[names(.data) == "S1702_C02_001E"] <- "EP_FAMPOV"
  names(.data)[names(.data) == "DP03_0003E"] <- "D_UNEMP"
  names(.data)[names(.data) == "DP03_0005E"] <- "E_UNEMP"

  ## calculate components
  .data$EP_INVINC <- dep_safe_pct(.data$E_INVINC, .data$E_HH)
  .data$EP_NO_INVINC <- 100 - .data$EP_INVINC
  .data$EP_ASSIST <- dep_safe_pct(.data$E_ASSIST, .data$E_HH)
  .data$EP_NO_ASSIST <- 100 - .data$EP_ASSIST
  .data$EP_MANAGE <- dep_safe_pct(.data$E_MANAGE, .data$D_MANAGE)
  .data$EP_WORKER <- 100 - .data$EP_MANAGE
  .data$E_FEMHH <- .data$B11005_007E + .data$B11005_010E
  .data$EP_FEMHH <- dep_safe_pct(.data$E_FEMHH, .data$E_HH)
  .data$EP_OWNOCC <- dep_safe_pct(.data$E_OWNOCC, .data$E_HH)
  .data$EP_NO_OWNOCC <- 100 - .data$EP_OWNOCC
  .data$EP_NOPLUMB <- dep_safe_pct(.data$E_NOPLUMB, .data$E_HH)
  .data$EP_NOPHONE <- dep_safe_pct(.data$E_NOPHONE, .data$E_HH)
  .data$EP_HSPLUS <- dep_safe_pct(.data$E_HSPLUS, .data$D_HIGHDEG)
  .data$EP_LTHS <- 100 - .data$EP_HSPLUS
  .data$EP_BAPLUS <- dep_safe_pct(.data$E_BAPLUS, .data$D_HIGHDEG)
  .data$EP_LTBA <- 100 - .data$EP_BAPLUS
  .data$EP_UNEMP <- dep_safe_pct(.data$E_UNEMP, .data$D_UNEMP)

  ## calculate metrics
  .data$EL_HHINC <- log(.data$E_HHINC)
  .data$EL_HOMEV <- log(.data$E_HOMEV)
  .data$ES_NO_INVINC <- scale(.data$EP_NO_INVINC)[,1]
  .data$ES_NO_ASSIST <- scale(.data$EP_NO_ASSIST)[,1]
  .data$ES_WORKER <- scale(.data$EP_WORKER)[,1]
  .data$ES_FEMHH <- scale(.data$EP_FEMHH)[,1]
  .data$ES_NO_OWNOCC <- scale(.data$EP_NO_OWNOCC)[,1]
  .data$ES_NOPLUMB <- scale(.data$EP_NOPLUMB)[,1]
  .data$ES_NOPHONE <- scale(.data$EP_NOPHONE)[,1]
  .data$ES_LTHS <- scale(.data$EP_LTHS)[,1]
  .data$ES_LTBA <- scale(.data$EP_LTBA)[,1]
  .data$ES_FAMPOV <- scale(.data$EP_FAMPOV)[,1]
  .data$ES_UNEMP <- scale(.data$EP_UNEMP)[,1]

  ## prep for scoring
  ndi_pw_score <- subset(.data, select = c(GEOID, E_TOTPOP, EL_HHINC, EL_HOMEV, ES_NO_INVINC,
                                           ES_NO_ASSIST, ES_WORKER, ES_FEMHH, ES_NO_OWNOCC,
                                           ES_NOPLUMB, ES_NOPHONE, ES_LTHS, ES_LTBA,
                                           ES_FAMPOV, ES_UNEMP))

  ## calculate ndi
  ### calculate
  ndi_pw_score <- ndi::powell_wiley(geo = geography, year = year, df = ndi_pw_score)[[1]]

  ### subset
  ndi_pw_score <- subset(ndi_pw_score, select = c(GEOID, NDI))

  ### rename
  names(ndi_pw_score)[names(ndi_pw_score) == "NDI"] <- "NDI_PW"

  ### optionally convert to percentiles
  if (return_percentiles){
    ndi_tot <- subset(.data, select = c(GEOID, E_TOTPOP))
    ndi_pw_score <- merge(x = ndi_pw_score, y = ndi_tot, by = "GEOID", all.x = TRUE)

    ndi_pw_score$NDI_PW_L <- ndi_pw_score$NDI_PW*log(ndi_pw_score$E_TOTPOP)
    ndi_pw_score$NDI_PW <- dep_percent_rank(ndi_pw_score$NDI_PW_L)*100
    ndi_pw_score <- subset(ndi_pw_score, select = -c(E_TOTPOP, NDI_PW_L))
  }

  ## optionally add components back into data
  if (!keep_components){

    out <- ndi_pw_score

  } else if (keep_components){

    ### rename moe components
    names(.data)[names(.data) == "S0101_C01_001M"] <- "M_TOTPOP"
    names(.data)[names(.data) == "DP04_0002M"] <- "M_HH"
    names(.data)[names(.data) == "B19013_001M"] <- "M_HHINC"
    names(.data)[names(.data) == "B25077_001M"] <- "M_HOMEV"
    names(.data)[names(.data) == "B19054_002M"] <- "M_INVINC"
    names(.data)[names(.data) == "B19058_002M"] <- "M_ASSIST"
    names(.data)[names(.data) == "C24060_001M"] <- "DM_MANAGE"
    names(.data)[names(.data) == "C24060_002M"] <- "M_MANAGE"
    names(.data)[names(.data) == "DP04_0046M"] <- "M_OWNOCC"
    names(.data)[names(.data) == "DP04_0073M"] <- "M_NOPLUMB"
    names(.data)[names(.data) == "DP04_0075M"] <- "M_NOPHONE"
    names(.data)[names(.data) == "S1501_C01_006M"] <- "DM_HIGHDEG"
    names(.data)[names(.data) == "S1501_C01_014M"] <- "M_HSPLUS"
    names(.data)[names(.data) == "S1501_C01_015M"] <- "M_BAPLUS"
    names(.data)[names(.data) == "S1702_C02_001M"] <- "MP_FAMPOV"
    names(.data)[names(.data) == "DP03_0003M"] <- "DM_UNEMP"
    names(.data)[names(.data) == "DP03_0005M"] <- "M_UNEMP"

    ### calculate margins of error
    .data$MP_INVINC <- dep_derived_moe(.data$M_INVINC, .data$EP_INVINC, .data$M_HH, .data$M_HH)
    .data$MP_ASSIST <- dep_derived_moe(.data$M_ASSIST, .data$EP_ASSIST, .data$M_HH, .data$M_HH)
    .data$MP_MANAGE <- dep_derived_moe(.data$M_MANAGE, .data$EP_MANAGE, .data$DM_MANAGE, .data$DM_MANAGE)
    .data$M_FEMHH <- sqrt(.data$B11005_007M^2+.data$B11005_010M^2)
    .data$MP_FEMHH <- dep_derived_moe(.data$M_FEMHH, .data$EP_FEMHH, .data$M_HH, .data$M_HH)
    .data$MP_OWNOCC <- dep_derived_moe(.data$M_OWNOCC, .data$EP_OWNOCC, .data$M_HH, .data$M_HH)
    .data$MP_NOPLUMB <- dep_derived_moe(.data$M_NOPLUMB, .data$EP_NOPLUMB, .data$M_HH, .data$M_HH)
    .data$MP_NOPHONE <- dep_derived_moe(.data$M_NOPHONE, .data$EP_NOPHONE, .data$M_HH, .data$M_HH)
    .data$MP_HSPLUS <- dep_derived_moe(.data$M_HSPLUS, .data$EP_HSPLUS, .data$DM_HIGHDEG, .data$DM_HIGHDEG)
    .data$MP_BAPLUS <- dep_derived_moe(.data$M_BAPLUS, .data$EP_BAPLUS, .data$DM_HIGHDEG, .data$DM_HIGHDEG)
    .data$MP_UNEMP <- dep_derived_moe(.data$M_UNEMP, .data$EP_UNEMP, .data$DM_UNEMP, .data$DM_UNEMP)

    ### reorder variables
    .data <- subset(.data, select = c(GEOID, E_TOTPOP, M_TOTPOP, E_HH, M_HH,
                                      E_HHINC, M_HHINC, EL_HHINC, E_HOMEV, M_HOMEV, EL_HOMEV,
                                      E_INVINC, M_INVINC, EP_INVINC, MP_INVINC, EP_NO_INVINC, ES_NO_INVINC,
                                      E_ASSIST, M_ASSIST, EP_ASSIST, MP_ASSIST, EP_NO_ASSIST, ES_NO_ASSIST,
                                      D_MANAGE, DM_MANAGE, E_MANAGE, M_MANAGE, EP_MANAGE, MP_MANAGE, EP_WORKER, ES_WORKER,
                                      E_FEMHH, M_FEMHH, EP_FEMHH, MP_FEMHH, ES_FEMHH,
                                      E_OWNOCC, M_OWNOCC, EP_OWNOCC, MP_OWNOCC, EP_NO_OWNOCC, ES_NO_OWNOCC,
                                      E_NOPLUMB, M_NOPLUMB, EP_NOPLUMB, MP_NOPLUMB, ES_NOPLUMB,
                                      E_NOPHONE, M_NOPHONE, EP_NOPHONE, MP_NOPHONE, ES_NOPHONE,
                                      D_HIGHDEG, DM_HIGHDEG, E_HSPLUS, M_HSPLUS, EP_HSPLUS, MP_HSPLUS, EP_LTHS, ES_LTHS,
                                      E_BAPLUS, M_BAPLUS, EP_BAPLUS, MP_BAPLUS, EP_LTBA, ES_LTBA,
                                      EP_FAMPOV, MP_FAMPOV, ES_FAMPOV,
                                      D_UNEMP, DM_UNEMP, E_UNEMP, M_UNEMP, EP_UNEMP, MP_UNEMP, ES_UNEMP))

    ### combine with ndi
    out <- merge(ndi_pw_score, .data, by = "GEOID")

  }

  ## return output
  return(out)

}
