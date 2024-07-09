# Define global variables for use in deprivateR functions

## general
utils::globalVariables(c("GEOID", "ZCTA3", "NAME", "geometry"))

## dep_build_varlist.R
utils::globalVariables(c("set1", "set2", "set3", "set4", "set5",
                         "variable", "description", "name", "label", "concept"))

## dep_process.R
### ADI
utils::globalVariables(c("ADI", "B11005_001",
                         "Financial_Strength", "Economic_Hardship_and_Inequality", "Educational_Attainment"))

### SVI Global
utils::globalVariables(c("SVI_HCD", "SVI_HTT", "SVI_MSL", "SVI_SES"))

### SVI Primary
utils::globalVariables(c("E_TOTPOP", "M_TOTPOP", "E_HU", "M_HU", "E_HH", "M_HH"))


### SVI SES
utils::globalVariables(c("D_POV", "DM_POV", "E_POV", "M_POV", "EP_POV", "MP_POV", "EPL_POV",
                         "D_POV150", "DM_POV150", "E_POV150", "M_POV150", "EP_POV150", "MP_POV150", "EPL_POV150",
                         "D_UNEMP", "DM_UNEMP", "E_UNEMP", "M_UNEMP", "EP_UNEMP", "MP_UNEMP", "EPL_UNEMP",
                         "D_HBURD", "DM_HBURD", "E_HBURD", "M_HBURD", "EP_HBURD", "MP_HBURD", "EPL_HBURD",
                         "D_UNINSUR", "DM_UNINSUR", "E_UNINSUR", "M_UNINSUR", "EP_UNINSUR", "MP_UNINSUR", "EPL_UNINSUR",
                         "E_PCI", "M_PCI", "EPL_PCI",
                         "D_NOHSDP", "DM_NOHSDP", "E_NOHSDP", "M_NOHSDP", "EP_NOHSDP", "MP_NOHSDP", "EPL_NOHSDP",
                         "SPL_THEME1", "SVI_RPL_THEME1"))

### SVI HHD/HOU
utils::globalVariables(c("D_AGE", "DM_AGE", "E_AGE17", "M_AGE17", "EP_AGE17", "MP_AGE17", "EPL_AGE17",
                         "E_AGE65", "M_AGE65", "EP_AGE65", "MP_AGE65", "EPL_AGE65",
                         "D_DISABL", "DM_DISABL", "E_DISABL", "M_DISABL", "EP_DISABL", "MP_DISABL", "EPL_DISABL",
                         "D_SNGPNT", "DM_SNGPNT", "E_SNGPNT", "M_SNGPNT", "EP_SNGPNT", "MP_SNGPNT", "EPL_SNGPNT",
                         "SPL_THEME2", "SVI_RPL_THEME2"))

### SVI MSL/REM
utils::globalVariables(c("E_MINRTY", "M_MINRTY", "EP_MINRTY", "MP_MINRTY", "EPL_MINRTY",
                         "D_LIMENG", "DM_LIMENG", "E_LIMENG", "M_LIMENG", "EP_LIMENG", "MP_LIMENG", "EPL_LIMENG",
                         "SPL_THEME3", "SVI_RPL_THEME3"))

### SVI HTT
utils::globalVariables(c("D_HOUSE", "DM_HOUSE", "E_MUNIT", "M_MUNIT", "EP_MUNIT", "MP_MUNIT", "EPL_MUNIT",
                         "E_MOBILE", "M_MOBILE", "EP_MOBILE", "MP_MOBILE", "EPL_MOBILE",
                         "D_CROWD", "DM_CROWD", "E_CROWD", "M_CROWD", "EP_CROWD", "MP_CROWD", "EPL_CROWD",
                         "D_NOVEH", "DM_NOVEH", "E_NOVEH", "M_NOVEH", "EP_NOVEH", "MP_NOVEH", "EPL_NOVEH",
                         "E_GROUPQ", "M_GROUPQ", "EP_GROUPQ", "MP_GROUPQ", "EPL_GROUPQ",
                         "SPL_THEME4", "SVI_RPL_THEME4"))

### NDI
utils::globalVariables(c("DM_EDU", "DM_HIGHDEG", "DM_MANAGE", "DM_MPRO", "D_EDU", "D_HIGHDEG", "D_MANAGE", "D_MPRO",
                         "EL_HHINC", "EL_HOMEV", "EP_ASSIST", "EP_BAPLUS", "EP_FAMPOV", "EP_FEMHH", "EP_FFH",
                         "EP_HSPLUS", "EP_INVINC", "EP_LOWINC", "EP_LTBA", "EP_LTHS", "EP_MANAGE", "EP_MPRO",
                         "EP_NOPHONE", "EP_NOPLUMB", "EP_NO_ASSIST", "EP_NO_INVINC", "EP_NO_OWNOCC",
                         "EP_OWNOCC", "EP_POVH", "EP_WORKER", "ES_FAMPOV", "ES_FEMHH", "ES_LTBA", "ES_LTHS",
                         "ES_NOPHONE", "ES_NOPLUMB", "ES_NO_ASSIST", "ES_NO_INVINC", "ES_NO_OWNOCC", "ES_UNEMP",
                         "ES_WORKER", "E_ASSIST", "E_BAPLUS", "E_FEMHH", "E_FFH", "E_HHINC", "E_HOMEV", "E_HSPLUS",
                         "E_INVINC", "E_LOWINC", "E_LTHS", "E_MANAGE", "E_MPRO", "E_NOPHONE", "E_NOPLUMB", "E_OWNOCC",
                         "E_POVH", "MP_ASSIST", "MP_BAPLUS", "MP_FAMPOV", "MP_FEMHH", "MP_FFH", "MP_HSPLUS",
                         "MP_INVINC", "MP_LOWINC", "MP_LTHS", "MP_MANAGE", "MP_MPRO", "MP_NOPHONE", "MP_NOPLUMB",
                         "MP_OWNOCC", "MP_POVH", "M_ASSIST", "M_BAPLUS", "M_FEMHH", "M_FFH", "M_HHINC", "M_HOMEV",
                         "M_HSPLUS", "M_INVINC", "M_LOWINC", "M_LTHS", "M_MANAGE", "M_MPRO", "M_NOPHONE", "M_NOPLUMB",
                         "M_OWNOCC", "M_POVH", "NDI", "NDI_PW_L"))

## dep_utils.R
utils::globalVariables(c("states_lookup", "state.abb", "estimate", "moe", "values"))

## dep_quantiles.R
utils::globalVariables(c(":=", "source_var_quantiles"))
