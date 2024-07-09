# Create Internal Data ####

# === === === === === === === === === === === === === === === === === === === ===

# sample documentation links
# https://www.atsdr.cdc.gov/placeandhealth/svi/documentation/SVI_documentation_2018.html
# https://api.census.gov/data/2020/acs/acs5/profile/variables.html
# https://api.census.gov/data/2020/acs/acs5/subject/variables.html
# https://api.census.gov/data/2020/acs/acs5/variables.html

# === === === === === === === === === === === === === === === === === === === ===

## Dependencies ####

library(dplyr)
library(sf)

# === === === === === === === === === === === === === === === === === === === ===

## Measure Variables ####

# === === === === === === === === === === === === === === === === === === === ===

### Gini ####
gini10 <- "B19083_001" # (2010-2021)

# === === === === === === === === === === === === === === === === === === === ===

### SVI ####
### primary variables to download
pri_vars <- c("DP04_0001", # total housing units (2009-2021)
              "S0101_C01_001", # total population (2010-2021)
              "S1101_C01_001") # total households (2010-2021)

### theme 1 - socioeconomic status
ses_vars_core <- c("DP03_0003",  # >= 16 civilian labor force (2009-2021)
                   "DP03_0005")  # unemployed (>= 16 civilians) (2009-2021)

edu_denom10 <- c("B15002_001") # sex by educational attainment for >= 25 years (2010)

edu_vars10 <- c("B15002_003", # male, no schooling, sex by educational attainment for >= 25 years (2010)
                "B15002_004", # male, nursery to 4th grade, sex by educational attainment for >= 25 years (2010)
                "B15002_005", # male, 5th and 6th grade, sex by educational attainment for >= 25 years (2010)
                "B15002_006", # male, 7th and 8th grade, sex by educational attainment for >= 25 years (2010)
                "B15002_007", # male, 9th grade, sex by educational attainment for >= 25 years (2010)
                "B15002_008", # male, 10th grade, sex by educational attainment for >= 25 years (2010)
                "B15002_009", # male, 11th grade, sex by educational attainment for >= 25 years (2010)
                "B15002_010", # male, 12th grade, no diploma, sex by educational attainment for >= 25 years (2010)
                "B15002_020", # female, no schooling, sex by educational attainment for >= 25 years (2010)
                "B15002_021", # female, nursery to 4th grade, sex by educational attainment for >= 25 years (2010)
                "B15002_022", # female, 5th and 6th grade, sex by educational attainment for >= 25 years (2010)
                "B15002_023", # female, 7th and 8th grade, sex by educational attainment for >= 25 years (2010)
                "B15002_024", # female, 9th grade, sex by educational attainment for >= 25 years (2010)
                "B15002_025", # female, 10th grade, sex by educational attainment for >= 25 years (2010)
                "B15002_026", # female, 11th grade, sex by educational attainment for >= 25 years (2010)
                "B15002_027") # female, 12th grade, no diploma, sex by educational attainment for >= 25 years (2010)

edu_vars15 <- c("S1501_C01_006", # population >= 25 years (2010-2021)
                "S1501_C01_007", # population >= 25 years, less than a 9th grade education (2010-2021)
                "S1501_C01_008") # population >= 25 years, 9th to 12th grade education with no diploma (2010-2021)

ses14_vars <- c("B17001_001", # poverty status in the last 12 months, total (2009-2021)
                "B17001_002", # poverty status in the last 12 months, below poverty level (2009-2021)
                "B19301_001") # per capita income in inflation-adjusted dollars (2009-2021)

ses20_vars <- c("S1701_C01_001", # total population for whom poverty status can be determined (2012-2021)
                "S2701_C01_001", # total civilian noninstitutionalized population (2012-2021)
                "S2503_C01_001") # total occupied housing units (2021-2021)

pov_vars12 <- c("S1701_C01_036", # all individuals below 150% of the federal poverty level (2012-2014)
                "S2503_C01_032", # occupied housing units, monthly housing costs >= 30%, < $20k in household income in past 12 months (2012-2014)
                "S2503_C01_036", # occupied housing units, monthly housing costs >= 30%, $20k-$34.9k in household income in past 12 months (2012-2014)
                "S2503_C01_040", # occupied housing units, monthly housing costs >= 30%, $35k-$49.9k in household income in past 12 months (2012-2014)
                "S2503_C01_044") # occupied housing units, monthly housing costs >= 30%, $50k-$74.9k in household income in past 12 months (2012-2014)

pov_vars15 <- c("S1701_C01_040", # all individuals below 150% of the federal poverty level (2015-2021)
                "S2503_C01_028", # occupied housing units, monthly housing costs >= 30%, < $20k in household income in past 12 months (2015-2021)
                "S2503_C01_032", # occupied housing units, monthly housing costs >= 30%, $20k-$34.9k in household income in past 12 months (2015-2021)
                "S2503_C01_036", # occupied housing units, monthly housing costs >= 30%, $35k-$49.9k in household income in past 12 months (2015-2021)
                "S2503_C01_040") # occupied housing units, monthly housing costs >= 30%, $50k-$74.9k in household income in past 12 months (2015-2021)

unins_vars12 <- c("S2701_C02_001") # civilian noninstitutionalized population, uninsured (2012)
unins_vars13 <- c("S2701_C04_062") # civilian noninstitutionalized population, uninsured (2013-2014)
unins_vars15 <- c("S2701_C04_001") # civilian noninstitutionalized population, uninsured (2015-2021)


### theme 2 - household composition and disability (household characteristics as of 2020)
hdd_vars_core <- c("S0101_C01_001", # total population (2010-2021)
                   "S1101_C01_001") # total households (2010-2021)

hou_vars_core <- c("B16004_001") # age by language spoken at home by ability to speak english (>= 5 years) (2009-2021)

sngpnt09 <- c("B25115_009", # owner occupied, male head, no spouse, own kids < 18 (2009-2021)
              "B25115_012", # owner occupied, female head, no spouse, own kids < 18 (2009-2021)
              "B25115_022", # renter occupied, male head, no spouse, own kids < 18 (2009-2021)
              "B25115_025") # renter occupied, female head, no spouse, own kids < 18 (2009-2021)

sngpnt19 <- c("B11012_010", # male head, no spouse, own kids < 18 (2019-2021)
              "B11012_015") # female head, no spouse, own kids < 18 (2019-2021)

dis_vars_denom <- c("S2701_C01_001") # total civilian noninstitutionalized population (2012-2021)

dis_vars <- c("B18101_004", # male, < 5 years, with a disability (2012-2021)
              "B18101_007", # male, 5-17 years, with a disability (2012-2021)
              "B18101_010", # male, 18-34 years, with a disability (2012-2021)
              "B18101_013", # male, 35-64 years, with a disability (2012-2021)
              "B18101_016", # male, 65-74 years, with a disability (2012-2021)
              "B18101_019", # male, >= 75 years, with a disability (2012-2021)
              "B18101_023", # female, < 5 years, with a disability (2012-2021)
              "B18101_026", # female, 5-17 years, with a disability (2012-2021)
              "B18101_029", # female, 18-34 years, with a disability (2012-2021)
              "B18101_032", # female, 35-64 years, with a disability (2012-2021)
              "B18101_035", # female, 65-74 years, with a disability (2012-2021)
              "B18101_038") # female, >= 75 years, with a disability (2012-2021)

age_vars10_lt18 <- c("B01001_003", # population by age, male (< 5 years) (2010-2016)
                     "B01001_004", # population by age, male (5-9 years) (2010-2016)
                     "B01001_005", # population by age, male (10-14 years) (2010-2016)
                     "B01001_006", # population by age, male (15-17 years) (2010-2016)
                     "B01001_027", # population by age, female (< 5 years) (2010-2016)
                     "B01001_028", # population by age, female (5-9 years) (2010-2016)
                     "B01001_029", # population by age, female (10-14 years) (2010-2016)
                     "B01001_030") # population by age, female (15-17 years) (2010-2016)

age_vars10_gt64 <- c("B01001_020", # population by age, male (65 & 66 years) (2010-2016)
                     "B01001_021", # population by age, male (67-69 years) (2010-2016)
                     "B01001_022", # population by age, male (70-74 years) (2010-2016)
                     "B01001_023", # population by age, male (75-79 years) (2010-2016)
                     "B01001_024", # population by age, male (80-84 years) (2010-2016)
                     "B01001_025", # population by age, male (> 84 years) (2010-2016)
                     "B01001_044", # population by age, female (65 & 66 years) (2010-2016)
                     "B01001_045", # population by age, female (67-69 years) (2010-2016)
                     "B01001_046", # population by age, female (70-74 years) (2010-2016)
                     "B01001_047", # population by age, female (75-79 years) (2010-2016)
                     "B01001_048", # population by age, female (80-84 years) (2010-2016)
                     "B01001_049") # population by age, female (> 84 years) (2010-2016)

age_vars17 <- c("S0101_C01_001", # total population, confirmed in pri_vars (2010-2021)
                "S0101_C01_022", # population by age (< 18 years) (2017-2021)
                "S0101_C01_030") # population by age (>= 65 years) (2017-2021)


### theme 2/3 - english langauge variables (part of theme 3 until 2020, then theme 2)
### all valid 2009-2021
eng_vars <- c("B16004_007", # Spanish speaker, speaks English not well (5-17 years)
              "B16004_008", # Spanish speaker, speaks English not at all (5-17 years)
              "B16004_012", # Speaks other Indo-European languages, speaks English not well (5-17 years)
              "B16004_013", # Speaks other Indo-European languages, speaks English not at all (5-17 years)
              "B16004_017", # Speaks Asian and Pacific Islander languages, speaks English not well (5-17 years)
              "B16004_018", # Speaks Asian and Pacific Islander languages, speaks English not at all (5-17 years)
              "B16004_022", # Speaks other languages, speaks English not well (5-17 years)
              "B16004_023", # Speaks other languages, speaks English not at all (5-17 years)
              "B16004_029", # Spanish speaker, speaks English not well (18-64 years)
              "B16004_030", # Spanish speaker, speaks English not at all (18-64 years)
              "B16004_034", # Speaks other Indo-European languages, speaks English not well (18-64 years)
              "B16004_035", # Speaks other Indo-European languages, speaks English not at all (18-64 years)
              "B16004_039", # Speaks Asian and Pacific Islander languages, speaks English not well (18-64 years)
              "B16004_040", # Speaks Asian and Pacific Islander languages, speaks English not at all (18-64 years)
              "B16004_044", # Speaks other languages, speaks English not well (18-64 years)
              "B16004_045", # Speaks other languages, speaks English not at all (18-64 years)
              "B16004_051", # Spanish speaker, speaks English not well (>= 65 years)
              "B16004_052", # Spanish speaker, speaks English not at all (>= 65 years)
              "B16004_056", # Speaks other Indo-European languages, speaks English not well (>= 65 years)
              "B16004_057", # Speaks other Indo-European languages, speaks English not at all (>= 65 years)
              "B16004_061", # Speaks Asian and Pacific Islander languages, speaks English not well (>= 65 years)
              "B16004_062", # Speaks Asian and Pacific Islander languages, speaks English not at all (>= 65 years)
              "B16004_066", # Speaks other languages, speaks English not well (>= 65 years)
              "B16004_067") # Speaks other languages, speaks English not at all (>= 65 years)

### theme 3 - minority status and language (Racial & Ethnic Minority Status as of 2020)
#### core variables
msl_vars_core <- c("B01001H_001", # sex by age, white, non-hispanic (2009-2021)
                   "B16004_001", # age by language spoken at home by ability to speak english (>= 5 years) (2009-2021)
                   "S0101_C01_001") # total population, confirmed in pri_vars) (2010-2021)

rem_vars_core <- c("B01001H_001", # sex by age, white, non-hispanic (2009-2021)
                   "S0101_C01_001") # total population, confirmed in pri_vars) (2010-2021)

### theme 4 - housing type and transportation
htt_vars_core <- c("B26001_001", # group quarters population (2009-2021)
                   "DP04_0001", # total housing units, confirmed in pri_vars (2009-2021)
                   "DP04_0002", # total occupied housing units (2009-2021)*
                   "DP04_0006", # total housing units in structure (2009-2021)
                   "DP04_0012", # 10 to 19 units in structure (2009-2021)
                   "DP04_0013", # 20 or more units in structure (2009-2021)
                   "DP04_0014", # mobile home units (2009-2021)
                   "S0101_C01_001", # total population, confirmed in pri_vars (2010-2021)
                   "S1101_C01_001") # total households (2010-2021)

occ_vars09 <- c("DP04_0056", # total occupied housing units (2009-2014)
                "DP04_0057", # no vehicles available, occupied housing units (2009-2014)
                "DP04_0077", # occupants per room (1.01 to 1.5) (2009-2014)
                "DP04_0078") # occupants per room (>= 1.51) (2009-2014)

occ_vars15 <- c("DP04_0057", # total occupied housing units (2015-2021)
                "DP04_0058", # no vehicles available, occupied housing units (2015-2021)
                "DP04_0078", # occupants per room (1.01 to 1.5) (2015-2021)
                "DP04_0079") # occupants per room (>= 1.51) (2015-2021)

### combine
svi10_10 <- list(
  pri_vars = sort(pri_vars),
  ses_vars = sort(c(ses_vars_core, ses14_vars, edu_denom10)),
  edu_vars= sort(edu_vars10),
  hhd_vars = sort(c(hdd_vars_core, sngpnt09)),
  age_lt18_vars = sort(age_vars10_lt18),
  age_gt64_vars = sort(age_vars10_gt64),
  msl_vars = sort(msl_vars_core),
  eng_vars = sort(eng_vars),
  htt_vars = sort(c(htt_vars_core, occ_vars09))
)

svi10_15 <- list(
  pri_vars = sort(pri_vars),
  ses_vars = sort(c(ses_vars_core, ses14_vars, edu_vars15)),
  hhd_vars = sort(c(hdd_vars_core, sngpnt09)),
  age_lt18_vars = sort(age_vars10_lt18),
  age_gt64_vars = sort(age_vars10_gt64),
  msl_vars = sort(msl_vars_core),
  eng_vars = sort(eng_vars),
  htt_vars = sort(c(htt_vars_core, occ_vars15))
)

svi10_17 <- list(
  pri_vars = sort(pri_vars),
  ses_vars = sort(c(ses_vars_core, ses14_vars, edu_vars15)),
  hhd_vars = sort(c(hdd_vars_core, age_vars17, sngpnt09)),
  msl_vars = sort(msl_vars_core),
  eng_vars = sort(eng_vars),
  htt_vars = sort(c(htt_vars_core, occ_vars15))
)

svi14_12 <- list(
  pri_vars = sort(pri_vars),
  ses_vars = sort(c(ses_vars_core, ses14_vars, edu_denom10)),
  edu_vars= sort(edu_vars10),
  hhd_vars = sort(c(hdd_vars_core, sngpnt09, dis_vars_denom)),
  age_lt18_vars = sort(age_vars10_lt18),
  age_gt64_vars = sort(age_vars10_gt64),
  dis_vars = sort(dis_vars),
  msl_vars = sort(msl_vars_core),
  eng_vars = sort(eng_vars),
  htt_vars = sort(c(htt_vars_core, occ_vars09))
)

svi14_15 <- list(
  pri_vars = sort(pri_vars),
  ses_vars = sort(c(ses_vars_core, ses14_vars, edu_vars15)),
  hhd_vars = sort(c(hdd_vars_core, sngpnt09, dis_vars_denom)),
  age_lt18_vars = sort(age_vars10_lt18),
  age_gt64_vars = sort(age_vars10_gt64),
  dis_vars = sort(dis_vars),
  msl_vars = sort(msl_vars_core),
  eng_vars = sort(eng_vars),
  htt_vars = sort(c(htt_vars_core, occ_vars15))
)

svi14_17 <- list(
  pri_vars = sort(pri_vars),
  ses_vars = sort(c(ses_vars_core, ses14_vars, edu_vars15)),
  hhd_vars = sort(c(hdd_vars_core, age_vars17, sngpnt09, dis_vars_denom)),
  dis_vars = sort(dis_vars),
  msl_vars = sort(msl_vars_core),
  eng_vars = sort(eng_vars),
  htt_vars = sort(c(htt_vars_core, occ_vars15))
)

svi20_12 <- list(
  pri_vars = sort(pri_vars),
  ses_vars = sort(c(ses_vars_core, ses20_vars, pov_vars12, unins_vars12, edu_denom10)),
  edu_vars= sort(edu_vars10),
  hhd_vars = sort(c(hdd_vars_core, hou_vars_core, sngpnt09, dis_vars_denom)),
  age_lt18_vars = sort(age_vars10_lt18),
  age_gt64_vars = sort(age_vars10_gt64),
  dis_vars = sort(dis_vars),
  eng_vars = sort(eng_vars),
  msl_vars = sort(rem_vars_core),
  htt_vars = sort(c(htt_vars_core, occ_vars09))
)

svi20_13 <- list(
  pri_vars = sort(pri_vars),
  ses_vars = sort(c(ses_vars_core, ses20_vars, pov_vars12, unins_vars13, edu_denom10)),
  edu_vars= sort(edu_vars10),
  hhd_vars = sort(c(hdd_vars_core, hou_vars_core, sngpnt09, dis_vars_denom)),
  age_lt18_vars = sort(age_vars10_lt18),
  age_gt64_vars = sort(age_vars10_gt64),
  dis_vars = sort(dis_vars),
  eng_vars = sort(eng_vars),
  msl_vars = sort(rem_vars_core),
  htt_vars = sort(c(htt_vars_core, occ_vars09))
)

svi20_15 <- list(
  pri_vars = sort(pri_vars),
  ses_vars = sort(c(ses_vars_core, ses20_vars, pov_vars15, unins_vars15, edu_vars15)),
  hhd_vars = sort(c(hdd_vars_core, hou_vars_core, sngpnt09, dis_vars_denom)),
  age_lt18_vars = sort(age_vars10_lt18),
  age_gt64_vars = sort(age_vars10_gt64),
  dis_vars = sort(dis_vars),
  eng_vars = sort(eng_vars),
  msl_vars = sort(rem_vars_core),
  htt_vars = sort(c(htt_vars_core, occ_vars15))
)

svi20_17 <- list(
  pri_vars = sort(pri_vars),
  ses_vars = sort(c(ses_vars_core, ses20_vars, pov_vars15, unins_vars15, edu_vars15)),
  hhd_vars = sort(c(hdd_vars_core, hou_vars_core, age_vars17, sngpnt09, dis_vars_denom)),
  dis_vars = sort(dis_vars),
  eng_vars = sort(eng_vars),
  msl_vars = sort(rem_vars_core),
  htt_vars = sort(c(htt_vars_core, occ_vars15))
)

svi20s_19 <- list(
  pri_vars = sort(pri_vars),
  ses_vars = sort(c(ses_vars_core, ses20_vars, pov_vars15, unins_vars15, edu_vars15)),
  hhd_vars = sort(c(hdd_vars_core, hou_vars_core, age_vars17, sngpnt19, dis_vars_denom)),
  dis_vars = sort(dis_vars),
  eng_vars = sort(eng_vars),
  msl_vars = sort(rem_vars_core),
  htt_vars = sort(c(htt_vars_core, occ_vars15))
)

rm(pri_vars, ses_vars_core, ses14_vars, ses20_vars, pov_vars12, pov_vars15,
   unins_vars12, unins_vars13, unins_vars15, edu_vars15, edu_denom10, edu_vars10,
   hdd_vars_core, hou_vars_core, dis_vars_denom, dis_vars, age_vars10_lt18, age_vars10_gt64, age_vars17,
   sngpnt09, sngpnt19,
   msl_vars_core, rem_vars_core, eng_vars,
   htt_vars_core, occ_vars09, occ_vars15)

# === === === === === === === === === === === === === === === === === === === ===

### NDI - Messer ####
ndi_m_vars <- c(## men in scientific and management positions
                "C24030_002", # civilian employment >= 16, men (2020-2021)
                "C24030_018", # men employed in professional, scientific, technical services (2020-2021)
                "C24030_019", # men employed in management of companies and enterprises (2020-2021)

                ## crowding
                "DP04_0002", # total occupied housing units (2018-2021)
                "DP04_0078", # occupants per room (1.01 to 1.5) (2018-2021)
                "DP04_0079", # occupants per room (>= 1.51) (2018-2021)

                ## poverty status
                "B17017_002", # poverty status in the last 12 months, below poverty level (2018-2021)

                ## female headed households, no spouse, with own kids < 18
                "B25115_012", # owner occupied, female head, no spouse, own kids < 18 (2020-2021)
                "B25115_025", # renter occupied, female head, no spouse, own kids < 18 (2020-2021)

                ## public assistance
                "B19058_002", # households receiving public assistance, including SNAP (2020-2021)

                ## household income
                "B19001_002", # household income in past 12 months, < $10k (2020-2021)
                "B19001_003", # household income in past 12 months, $10k-$14.9k (2020-2021)
                "B19001_004", # household income in past 12 months, $15k-$19.9k (2020-2021)
                "B19001_005", # household income in past 12 months, $20k-$24.9k (2020-2021)
                "B19001_006", # household income in past 12 months, $25k-$29.9k (2020-2021)

                ## education less than HS
                "B06009_001", # place of birth by educational attainment, total (2020-2021)
                "B06009_002", # place of birth by educational attainment, less than a high school education (2020-2021)

                ## unemployment
                "DP03_0003",  # >= 16 civilian labor force (2018-2021)
                "DP03_0005")  # unemployed (>= 16 civilians) (2018-2021)

# === === === === === === === === === === === === === === === === === === === ===

### NDI PW ####
ndi_pw_vars <- c(## total population
                 "S0101_C01_001", # (2020-2021)

                 ## crowding
                 "DP04_0002", # total occupied housing units (2018-2021)

                 ## ses
                 "B19013_001", # median household income (2020-2021)
                 "B25077_001", # median home value (2020-2021)

                 ## interest, dividend, or net rental income in past 12 months
                 "B19054_002", # households with interest, dividend, or net rental income (2020-2021)

                 ## public assistance
                 "B19058_002", # households receiving public assistance, including SNAP (2020-2021)

                 ## occupational characteristics
                 "C24060_001", # civilian employment >= 16 (2020-2021)
                 "C24060_002", # management, business, science, and arts occupations (2020-2021)

                 ## female headed households, no spouse, with any kids < 18
                 "B11005_007", # female householder, no spouse present, one or more kids < 18 (2020-2021)
                 "B11005_010", # female householder, nonfamily household, one or more kids < 18 (2020-2021)

                 ## housing characteristics
                 "DP04_0046", # owner-occupied housing units (2020-2021)
                 "DP04_0073", # occupied housing units lacking complete plumbing facilities (2020-2021)
                 "DP04_0075", # occupied housing units without available telephone service (2020-2021)

                 ## educational attainment
                 "S1501_C01_006", # Population >= 25 years (2020)
                 "S1501_C01_014", # Population >= 25 years, high school / equivalent degree or higher (2020)
                 "S1501_C01_015", # Population >= 25 years, bachelor's degree or higher (2020)

                 ## family poverty
                 "S1702_C02_001",

                 ## unemployment
                 "DP03_0003",  # >= 16 civilian labor force (2018-2021)
                 "DP03_0005")  # unemployed (>= 16 civilians) (2018-2021)

# === === === === === === === === === === === === === === === === === === === ===

### Put Data Together ####
request_vars <- list(
  gini10 = gini10,
  ndi_m = ndi_m_vars,
  ndi_pw = ndi_pw_vars,
  svi10_10 = svi10_10,
  svi10_15 = svi10_15,
  svi10_17 = svi10_17,
  svi14_12 = svi14_12,
  svi14_15 = svi14_15,
  svi14_17 = svi14_17,
  svi20_12 = svi20_12,
  svi20_13 = svi20_13,
  svi20_15 = svi20_15,
  svi20_17 = svi20_17,
  svi20s_19 = svi20s_19
)

rm(gini10, svi10_10, svi10_15, svi10_17,
   svi14_12, svi14_15, svi14_17,
   svi20_12, svi20_13, svi20_15, svi20_17, svi20s_19,
   ndi_m_vars, ndi_pw_vars)

# === === === === === === === === === === === === === === === === === === === ===

## States Vector ####

states <- tigris::states(cb = TRUE)
sf::st_geometry(states) <- NULL
# states <- sort(states$STUSPS)
states_lookup <- dplyr::select(states, fips = GEOID, abb = STUSPS, name = NAME)
states_lookup <- dplyr::mutate(states_lookup, abb = tolower(abb), name = tolower(name))
states_lookup <- dplyr::arrange(states_lookup, fips)

# === === === === === === === === === === === === === === === === === === === ===

## Create Sample Data ####

devtools::load_all()

vars_a <- dep_build_varlist(geography = "tract", index = "adi", year = 2022)
vars_b <- dep_build_varlist(geography = "tract", index = "ndi_m", year = 2022)
vars_c <- dep_build_varlist(geography = "tract", index = "ndi_pw", year = 2022)
vars_d <- dep_build_varlist(geography = "tract", index = "svi10", year = 2022)
vars_e <- dep_build_varlist(geography = "tract", index = "svi14", year = 2022)
vars_f <- dep_build_varlist(geography = "tract", index = "svi20", year = 2022)
vars_g <- dep_build_varlist(geography = "tract", index = "svi20s", year = 2022)

vars <- sort(unique(c(vars_a, vars_b, vars_c, vars_d, vars_e, vars_f, vars_g)))

rm(vars_a, vars_b, vars_c, vars_d, vars_e, vars_f, vars_g)

mo <- tidycensus::get_acs(
  geography = "county",
  variables = vars,
  year = 2022, state = "MO",
  output = "wide"
)

# === === === === === === === === === === === === === === === === === === === ===

## Save Internal Data Object ####
save(request_vars, states_lookup, mo, file = "R/sysdata.rda", version = 2, compress = "xz")

rm(request_vars, states, states_lookup, mo, vars)

# === === === === === === === === === === === === === === === === === === === ===
