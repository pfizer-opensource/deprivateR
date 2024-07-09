### rename moe components
names(.data)[names(.data) == "S0601_C01_001M"] <- "M_TOTPOP"
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
.data$MP_MANAGE <- ((sqrt(.data$M_ASSIST^2-((.data$EP_MANAGE/100)^2*.data$DM_MANGE^2)))/.data$DM_MANGE)*100
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
                                  D_MANAGE, DM_MANGE, E_MANAGE, M_MANAGE, EP_MANAGE, MP_MANAGE, EP_WORKER, ES_WORKER,
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
