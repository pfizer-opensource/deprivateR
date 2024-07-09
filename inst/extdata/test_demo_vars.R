# compare existing components with NDI components ####

## dependencies ####
library(dplyr)
library(tidycensus)

## ndi - messer
### crowding
crowd <- c("B25014_001", # total
           "B25014_005", # owner occupied 1-1.5 occupants
           "B25014_006", # owner occupied 1.5-2 occupants
           "B25014_007", # owner occupied 2 or more occupants
           "B25014_011", # renter occupied 1-1.5 occupants
           "B25014_012", # renter occupied 1.5-2 occupants
           "B25014_013", # renter occupieed 2 or more occupants
           "DP04_0002",  # total
           "DP04_0076",  # total
           "DP04_0078", # occupants per room (1.01 to 1.5) (2018, 2019, 2020)
           "DP04_0079") # occupants per room (>= 1.51) (2018, 2019, 2020)

### female headed households with no spouse and kids < 18
female <- c("B25115_001", # total
            "B25115_009", # owner occupied, male head, no spouse, own kids < 18
            "B25115_012", # owner occupied, female head, no spouse, own kids < 18
            "B25115_022", # renter occupied, male head, no spouse, own kids < 18
            "B25115_025", # renter occupied, female head, no spouse, own kids < 18
            "DP02_0001", # total households, confirmed in pri_vars
            "DP02_0007",
            "DP02_0009") # male householder, no spouse, with children < 18 (2018)

### unemployment
unemploy <- c("B23025_003", # civilian labor force, total
              "B23025_005", # civilian labor force, unemployed
              "DP03_0003",
              "DP03_0005", # unemployed >=16 civilians
              "DP03_0009P", # unemployed rate
              "S2301_C04_001") # percent unemployed


## denominators
dems <- c(
  "DP04_0002", # total occupied housing units (2018, 2019, 2020)
  "B25115_001", # total households, same as DP02_0001 in SVI pri_vars (2018, 2019, 2020)
  "B19058_001", # total households, same as DP02_0001 in SVI pri_vars (2018, 2019, 2020)
  "B19001_001", # total households, same as DP02_0001 in SVI pri_vars (2018, 2019, 2020)
  "B11005_001", # total households, same as DP02_0001 in SVI pri_vars (2018, 2019, 2020)
  "DP04_0045", # occupied housing units (2020)
  "B19054_001"
)


## ndi - powell-wiley
### percentages
pcts <- c("DP04_0045",
          "DP04_0046",
          "DP04_0046P",
          "DP04_0073",
          "DP04_0073P",
          "DP04_0075",
          "DP04_0075P")

totals <- c("S0601_C01_001", "B01001_001")

### education
edu <- c(                                  "S1501_C01_009", # Population >= 25 years, high school or equivalent (2020)
                                           "S1501_C01_010", # Population >= 25 years, some college, no degree (2020)
                                           "S1501_C01_011", # Population >= 25 years, associate's degree (2020)
  "S1501_C01_012", # Population >= 25 years, bachelor's degree (2020)
                          "S1501_C01_013", # Population >= 25 years, graduate or professional degree (2020)
                          "S1501_C01_014",
                          "S1501_C01_015")

pov <- c("S1702_C02_001",
         "B17021_001", # poverty status in the last 12 months, total (2018, 2019, 2020)
         "B17021_003",
         "B17017_001",
         "B17017_002") # poverty status in the last 12 months, below poverty level (2018, 2019, 2020))

crowd_df <- get_acs(geography = "state", variables = crowd, year = 2020, output = "wide") %>%
  mutate(crowdB = B25014_005E + B25014_006E + B25014_007E + B25014_011E + B25014_012E + B25014_013E) %>%
  mutate(crowdD = DP04_0078E + DP04_0079E) %>%
  select(GEOID, NAME, crowdB_den = B25014_001E, crowdD_den = DP04_0076E, crowdD_den2 = DP04_0002E, crowdB, crowdD)

female_df <- get_acs(geography = "state", variables = female, year = 2018, output = "wide") %>%
  mutate(femB = B25115_012E + B25115_025E) %>%
  mutate(maleB = B25115_009E + B25115_022E) %>%
  select(GEOID, NAME, femB_den = B25115_001E, femD_den = DP02_0001E, femB, femD = DP02_0009E, maleB, maleD = DP02_0007E)

unemploy_df <- get_acs(geography = "state", variables = unemploy, year = 2020, output = "wide") %>%
  mutate(unemp = DP03_0005E/DP03_0003E)

pct_df <- get_acs(geography = "state", variables = pcts, year = 2020, output = "wide") %>%
  mutate(owner = DP04_0046E/DP04_0045E*100) %>%
  mutate(no_plumb = DP04_0073E/DP04_0045E*100) %>%
  mutate(no_phone = DP04_0075E/DP04_0045E*100) %>%
  select(GEOID, NAME, owner, ownerp = DP04_0046PE, no_plumb, no_plumbp = DP04_0073PE, no_phone, no_phonep = DP04_0075PE)

edu_df <- get_acs(geography = "state", variables = edu, year = 2020, output = "wide") %>%
  mutate(hs_higher = S1501_C01_009E + S1501_C01_010E + S1501_C01_011E + S1501_C01_012E + S1501_C01_013E) %>%
  mutate(bs_higher = S1501_C01_012E + S1501_C01_013E) %>%
  select(GEOID, NAME, hs_higher, hs_higher2 = S1501_C01_014E, bs_higher, bs_higher2 = S1501_C01_015E)

pov_df <- get_acs(geography = "state", variables = pov, year = 2020, output = "wide") %>%
  mutate(pov_calc = B17021_003E/B17021_001E*100) %>%
  mutate(pov_calc2 = B17017_002E/B17017_001E*100) %>%
  select(GEOID, NAME, S1702_C02_001E, pov_calc, pov_calc2)

dem_df <- get_acs(geography = "county", state = 29, variables = dems, year = 2018, output = "wide")
dem_df <- get_acs(geography = "state", variables = dems, year = 2018, output = "wide")

tot_df <- get_acs(geography = "state", variables = totals, year = 2020, output = "wide")
