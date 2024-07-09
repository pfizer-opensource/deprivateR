context("test dep_calc_index function")

# test errors ------------------------------------------------

test_that("incorrectly specified parameters trigger appropriate errors", {
  sample_df <- expect_error(
    dep_sample_data(index = "ndi_m"), NA
  )

  expect_error(dep_calc_index(sample_df, index = "ndi_m", year = 2022,
                              survey = "acs5", output = "wide"),
               "A level of geography must be provided. Please choose one of: 'county', 'zcta3', 'zcta5', or 'tract'.")
  expect_error(dep_calc_index(sample_df, geography = "ham", index = "ndi_m",
                              year = 2022, survey = "acs5", output = "wide"),
               "Invalid level of geography provided. Please choose one of: 'county', 'zcta3', 'zcta5', or 'tract'.")
  expect_error(dep_calc_index(sample_df, geography = "county", year = 2022,
                              survey = "acs5", output = "wide"),
               "A 'index' value must be provided. Please choose one of: 'adi', 'gini', 'ndi_m', 'ndi_pw', 'svi10', 'svi14', 'svi20', or 'svi20s'.")
  expect_error(dep_calc_index(sample_df, geography = "county", index = "ham",
                              year = 2022, survey = "acs5", output = "wide"),
               "Invalid index provided. Please choose one of: 'adi', 'gini', 'ndi_m', 'ndi_pw', 'svi10', 'svi14', 'svi20', or 'svi20s'.")
  expect_error(dep_calc_index(sample_df, geography = "county", index = "ndi_m",
                              year = 2000, survey = "acs5", output = "wide"),
               "The 'year' value provided is invalid. Please provide a numeric value between 2010 and 2022.")
  expect_error(dep_calc_index(sample_df, geography = "county", index = "ndi_m",
                              year = "ham", survey = "acs5", output = "wide"),
               "The 'year' value provided is invalid. Please provide a numeric value between 2010 and 2022.")
  expect_error(dep_calc_index(sample_df, geography = "county", index = "ndi_m",
                              year = 2022, survey = "acs3", output = "wide"),
               "The 'acs3' survey was discontinued after 2013. Please select one of 'acs1' or 'acs5'.")
  expect_error(dep_calc_index(sample_df, geography = "county", index = "ndi_m",
                              year = 2022, survey = "acs3", output = "wide"),
               "The 'acs3' survey was discontinued after 2013. Please select one of 'acs1' or 'acs5'.")
  expect_error(dep_calc_index(sample_df, geography = "county", index = "ndi_m",
                              year = 2022, survey = "ham", output = "wide"),
               "The 'survey' value provided is not valid. Please choose one of 'acs1', 'acs3', or 'acs5'.")
  expect_error(dep_calc_index(sample_df, geography = "county", index = "ndi_m",
                              year = 2022, survey = "acs5", output = "ham"),
               "The 'output' value provided is not valid. Please choose one of 'wide' or 'tidy'.")
})

# test errors ------------------------------------------------

test_that("correctly specified parameters execute without error", {
  sample_df <- expect_error(
    dep_sample_data(index = "ndi_m"), NA
  )

  expect_error(
    dep_calc_index(sample_df, geography = "county", index = "ndi_m", year = 2022), NA
  )

})
