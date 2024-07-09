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
  .data$EP_MPRO <- .data$E_MPRO/.data$D_MPRO*100
  .data$EP_CROWD <- .data$E_CROWD/.data$E_HH*100
  .data$EP_POVH <- .data$E_POVH/.data$E_HH*100
  .data$EP_FFH <- .data$E_FFH/.data$E_HH*100
  .data$EP_ASSIST <- .data$E_ASSIST/.data$E_HH*100
  .data$EP_LOWINC <- .data$E_LOWINC/.data$E_HH*100
  .data$EP_LTHS <- .data$E_LTHS/.data$D_EDU*100
  .data$EP_UNEMP <- .data$E_UNEMP/.data$D_UNEMP*100

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
  if (return_percentiles == TRUE){
    ndi_m_score$NDI_M <- dep_percent_rank(ndi_m_score$NDI_M)*100
  }

  ## optionally add components back into data
  if (keep_components == FALSE){

    out <- ndi_m_score

  } else if (keep_components == TRUE){

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

    .data$MP_MPRO <- ((sqrt(.data$M_MPRO^2-((.data$EP_MPRO/100)^2*.data$DM_MPRO^2)))/.data$DM_MPRO)*100
    .data$MP_CROWD <- ((sqrt(.data$M_CROWD^2-((.data$EP_CROWD/100)^2*.data$M_HH^2)))/.data$M_HH)*100
    .data$MP_POVH <- ((sqrt(.data$M_POVH^2-((.data$EP_POVH/100)^2*.data$M_HH^2)))/.data$M_HH)*100
    .data$MP_FFH <- ((sqrt(.data$M_FFH^2-((.data$EP_FFH/100)^2*.data$M_HH^2)))/.data$M_HH)*100
    .data$MP_ASSIST <- ((sqrt(.data$M_ASSIST^2-((.data$EP_ASSIST/100)^2*.data$M_HH^2)))/.data$M_HH)*100
    .data$MP_LOWINC <- ((sqrt(.data$M_LOWINC^2-((.data$EP_LOWINC/100)^2*.data$M_HH^2)))/.data$M_HH)*100
    .data$MP_LTHS <- ((sqrt(.data$M_LTHS^2-((.data$EP_LTHS/100)^2*.data$DM_EDU^2)))/.data$DM_EDU)*100
    .data$MP_UNEMP <- ((sqrt(.data$M_UNEMP^2-((.data$EP_UNEMP/100)^2*.data$DM_UNEMP^2)))/.data$DM_UNEMP)*100

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
  .data$EP_INVINC <- .data$E_INVINC/.data$E_HH*100
  .data$EP_NO_INVINC <- 100 - .data$EP_INVINC
  .data$EP_ASSIST <- .data$E_ASSIST/.data$E_HH*100
  .data$EP_NO_ASSIST <- 100 - .data$EP_ASSIST
  .data$EP_MANAGE <- .data$E_MANAGE/.data$D_MANAGE*100
  .data$EP_WORKER <- 100 - .data$EP_MANAGE
  .data$E_FEMHH <- .data$B11005_007E + .data$B11005_010E
  .data$EP_FEMHH <- .data$E_FEMHH/.data$E_HH*100
  .data$EP_OWNOCC <- .data$E_OWNOCC/.data$E_HH*100
  .data$EP_NO_OWNOCC <- 100 - .data$EP_OWNOCC
  .data$EP_NOPLUMB <- .data$E_NOPLUMB/.data$E_HH*100
  .data$EP_NOPHONE <- .data$E_NOPHONE/.data$E_HH*100
  .data$EP_HSPLUS <- .data$E_HSPLUS/.data$D_HIGHDEG*100
  .data$EP_LTHS <- 100 - .data$EP_HSPLUS
  .data$EP_BAPLUS <- .data$E_BAPLUS/.data$D_HIGHDEG*100
  .data$EP_LTBA <- 100 - .data$EP_BAPLUS
  .data$EP_UNEMP <- .data$E_UNEMP/.data$D_UNEMP*100

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
  if (return_percentiles == TRUE){
    ndi_tot <- subset(.data, select = c(GEOID, E_TOTPOP))
    ndi_pw_score <- merge(x = ndi_pw_score, y = ndi_tot, by = "GEOID", all.x = TRUE)

    ndi_pw_score$NDI_PW_L <- ndi_pw_score$NDI_PW*log(ndi_pw_score$E_TOTPOP)
    ndi_pw_score$NDI_PW <- dep_percent_rank(ndi_pw_score$NDI_PW_L)*100
    ndi_pw_score <- subset(ndi_pw_score, select = -c(E_TOTPOP, NDI_PW_L))
  }

  ## optionally add components back into data
  if (keep_components == FALSE){

    out <- ndi_pw_score

  } else if (keep_components == TRUE){

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
    .data$MP_INVINC <- ((sqrt(.data$M_INVINC^2-((.data$EP_INVINC/100)^2*.data$M_HH^2)))/.data$M_HH)*100
    .data$MP_ASSIST <- ((sqrt(.data$M_ASSIST^2-((.data$EP_ASSIST/100)^2*.data$M_HH^2)))/.data$M_HH)*100
    .data$MP_MANAGE <- ((sqrt(.data$M_MANAGE^2-((.data$EP_MANAGE/100)^2*.data$DM_MANAGE^2)))/.data$DM_MANAGE)*100
    .data$M_FEMHH <- sqrt(.data$B11005_007M^2+.data$B11005_010M^2)
    .data$MP_FEMHH <- ((sqrt(.data$M_FEMHH^2-((.data$EP_FEMHH/100)^2*.data$M_HH^2)))/.data$M_HH)*100
    .data$MP_OWNOCC <- ((sqrt(.data$M_OWNOCC^2-((.data$EP_OWNOCC/100)^2*.data$M_HH^2)))/.data$M_HH)*100
    .data$MP_NOPLUMB <- ((sqrt(.data$M_NOPLUMB^2-((.data$EP_NOPLUMB/100)^2*.data$M_HH^2)))/.data$M_HH)*100
    .data$MP_NOPHONE <- ((sqrt(.data$M_NOPHONE^2-((.data$EP_NOPHONE/100)^2*.data$M_HH^2)))/.data$M_HH)*100
    .data$MP_HSPLUS <- ((sqrt(.data$M_HSPLUS^2-((.data$EP_HSPLUS/100)^2*.data$DM_HIGHDEG^2)))/.data$DM_HIGHDEG)*100
    .data$MP_BAPLUS <- ((sqrt(.data$M_BAPLUS^2-((.data$EP_BAPLUS/100)^2*.data$DM_HIGHDEG^2)))/.data$DM_HIGHDEG)*100
    .data$MP_UNEMP <- ((sqrt(.data$M_UNEMP^2-((.data$EP_UNEMP/100)^2*.data$DM_UNEMP^2)))/.data$DM_UNEMP)*100

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
