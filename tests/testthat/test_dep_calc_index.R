
# test errors ------------------------------------------------

test_that("incorrectly specified parameters trigger appropriate errors", {
  sample_df <- expect_error(
    dep_sample_data(index = "ndi_m"), NA
  )

  expect_error(dep_calc_index(sample_df, index = "ndi_m", year = 2022,
                              survey = "acs5", output = "wide"),
               "geography.*must be provided")
  expect_error(dep_calc_index(sample_df, geography = "ham", index = "ndi_m",
                              year = 2022, survey = "acs5", output = "wide"),
               "Invalid.*geography")
  expect_error(dep_calc_index(sample_df, geography = "county", year = 2022,
                              survey = "acs5", output = "wide"),
               "index.*must be provided")
  expect_error(dep_calc_index(sample_df, geography = "county", index = "ham",
                              year = 2022, survey = "acs5", output = "wide"),
               "Invalid.*index")
  expect_error(dep_calc_index(sample_df, geography = "county", index = "ndi_m",
                              year = 2000, survey = "acs5", output = "wide"),
               "year.*invalid")
  expect_error(dep_calc_index(sample_df, geography = "county", index = "ndi_m",
                              year = "ham", survey = "acs5", output = "wide"),
               "year.*invalid")
  expect_error(dep_calc_index(sample_df, geography = "county", index = "ndi_m",
                              year = 2022, survey = "acs3", output = "wide"),
               "acs3.*discontinued")
  expect_error(dep_calc_index(sample_df, geography = "county", index = "ndi_m",
                              year = 2022, survey = "acs3", output = "wide"),
               "acs3.*discontinued")
  expect_error(dep_calc_index(sample_df, geography = "county", index = "ndi_m",
                              year = 2022, survey = "ham", output = "wide"),
               "survey.*not valid")
  expect_error(dep_calc_index(sample_df, geography = "county", index = "ndi_m",
                              year = 2022, survey = "acs5", output = "ham"),
               "output.*not valid")
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
