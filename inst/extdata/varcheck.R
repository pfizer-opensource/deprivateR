library(tidycensus)

table2022_base <- load_variables(2022, "acs5", cache = TRUE)
table2021_base <- load_variables(2021, "acs5", cache = TRUE)

table2022_base_s <- load_variables(2022, "acs5/subject", cache = TRUE)
table2021_base_s <- load_variables(2021, "acs5/subject", cache = TRUE)

table2022_base_d <- load_variables(2022, "acs5/profile", cache = TRUE)
table2021_base_d <- load_variables(2021, "acs5/profile", cache = TRUE)

lowercase_all_except_variable <- function(df) {
  df[] <- lapply(names(df), function(name) {
    if (name == "name") {
      return(df[[name]])
    } else {
      return(tolower(df[[name]]))
    }
  })
  return(df)
}

table2022_base <- lowercase_all_except_variable(table2022_base)
table2021_base <- lowercase_all_except_variable(table2021_base)
table2022_base_s <- lowercase_all_except_variable(table2022_base_s)
table2021_base_s <- lowercase_all_except_variable(table2021_base_s)
table2022_base_d <- lowercase_all_except_variable(table2022_base_d)
table2021_base_d <- lowercase_all_except_variable(table2021_base_d)


b_varz <- c("B19083_001","B17001_001", "B16004_001", "B25115_009", "B17001_002", "B19301_001", "B26001_001",
            "B25115_012", "B25115_022", "B25115_025", "B11012_010", "B11012_015", "B01001H_001",
            "B18101_004", "B18101_010", "B18101_013", "B18101_016", "B18101_019", "B18101_023", "B18101_026",
              "B18101_029", "B18101_032", "B18101_035", "B18101_038", "B16004_007", "B16004_008", "B16004_012", 
              "B16004_013", "B16004_017", "B16004_018", "B16004_022", "B16004_023", "B16004_029", "B16004_030", 
              "B16004_034", "B16004_035", "B16004_039", "B16004_040", "B16004_044", "B16004_045", "B16004_051", 
              "B16004_056", "B16004_057", "B16004_061", "B16004_062", "B16004_066", "B16004_067")

d_varz <- c("DP04_0001", "DP04_0002", "DP04_0003", "DP04_0005", "DP04_0006", "DP04_0012",
            "DP04_0013", "DP04_0014", "DP04_0056", "DP04_0057", "DP04_0077", "DP04_0078", "DP04_0057", "DP04_0058",
            "DP04_0078", "DP04_0079")

s_varz <- c("S0101_C01_001", "S1101_C01_001", "S1501_C01_006", "S1501_C01_007", "S1501_C01_008", "S1701_C01_001",
            "S2701_C01_001", "S2503_C01_001", "S1701_C01_036", "S2503_C01_032", "S2503_C01_036", "S2503_C01_040",
            "S2503_C01_044", "S1701_C01_040", "S2503_C01_028", "S2503_C01_032", "S2503_C01_036", "S2503_C01_040",
            "S2701_C02_001", "S2701_C04_062", "S2701_C04_001")



length(s_varz)
num<-21
setdiff(table2021_base_s[table2021_base_s$name == s_varz[num], ], table2022_base_s[table2022_base_s$name == s_varz[num], ])
