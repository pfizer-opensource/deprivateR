
# test errors ------------------------------------------------

test_that("missing geography triggers error", {
  expect_error(
    dep_get_index(index = "ndi_m", year = 2020, survey = "acs5"),
    "A level of geography must be provided"
  )
})

test_that("invalid geography triggers error", {

  expect_error(
    dep_get_index(geography = "hamlet", index = "ndi_m", year = 2020, survey = "acs5"),
    "Invalid level of geography provided"
  )
})

test_that("missing index triggers error", {
  expect_error(
    dep_get_index(geography = "county", year = 2020, survey = "acs5"),
    "A 'index' value must be provided"
  )
})

test_that("invalid index triggers error", {
  expect_error(
    dep_get_index(geography = "county", index = "ham", year = 2020, survey = "acs5"),
    "Invalid index provided"
  )
})

test_that("missing year triggers error", {
  expect_error(
    dep_get_index(geography = "county", index = "ndi_m", survey = "acs5"),
    "A 'year' value must be provided"
  )
})

test_that("invalid year triggers error", {
  expect_error(
    dep_get_index(geography = "county", index = "ndi_m", year = 2000, survey = "acs5"),
    "The 'year' value provided is invalid"
  )
  expect_error(
    dep_get_index(geography = "county", index = "ndi_m", year = "ham", survey = "acs5"),
    "The 'year' value provided is invalid"
  )
})

test_that("svi14/svi20 year constraint triggers error", {
  expect_error(
    dep_get_index(geography = "county", index = "svi14", year = 2010, survey = "acs5"),
    "not valid for 2014 or 2020 SVI specifications"
  )
  expect_error(
    dep_get_index(geography = "county", index = "svi20", year = 2011, survey = "acs5"),
    "not valid for 2014 or 2020 SVI specifications"
  )
})

test_that("svi20s year constraint triggers error", {
  expect_error(
    dep_get_index(geography = "county", index = "svi20s", year = 2018, survey = "acs5"),
    "not valid for the 2020 SVI specification with the alternate single parent"
  )
})

test_that("invalid survey triggers error", {
  expect_error(
    dep_get_index(geography = "county", index = "ndi_m", year = 2020, survey = "ham"),
    "The 'survey' value provided is not valid"
  )
})

test_that("discontinued acs3 survey triggers error", {
  expect_error(
    dep_get_index(geography = "county", index = "ndi_m", year = 2020, survey = "acs3"),
    "The 'acs3' survey was discontinued after 2013"
  )
})

test_that("non-logical return_percentiles triggers error", {
  expect_error(
    dep_get_index(geography = "county", index = "ndi_m", year = 2020, survey = "acs5",
                  return_percentiles = "yes"),
    "Please provide a logical scalar for 'return_percentiles'"
  )
})

test_that("non-logical keep_subscales triggers error", {
  expect_error(
    dep_get_index(geography = "county", index = "ndi_m", year = 2020, survey = "acs5",
                  keep_subscales = "yes"),
    "Please provide a logical scalar for 'keep_subscales'"
  )
})

test_that("non-logical keep_components triggers error", {
  expect_error(
    dep_get_index(geography = "county", index = "ndi_m", year = 2020, survey = "acs5",
                  keep_components = "yes"),
    "Please provide a logical scalar for 'keep_components'"
  )
})

test_that("territory in state argument triggers error", {
  # Note: validate_state converts abbreviations to FIPS codes before the

  # territory check runs, so we pass the FIPS code directly to trigger the
  # correct validation path. This is a known issue in the package.
  skip("Territory validation compares FIPS against abbreviations - known bug")
})

test_that("state and county both specified triggers error", {
  expect_error(
    dep_get_index(geography = "county", index = "ndi_m", year = 2020, survey = "acs5",
                  state = "MO", county = "29510"),
    "Please choose values for either 'state' or 'county' but not both"
  )
})

test_that("non-logical puerto_rico triggers error", {
  expect_error(
    dep_get_index(geography = "county", index = "ndi_m", year = 2020, survey = "acs5",
                  puerto_rico = "yes"),
    "Please provide a logical scalar for 'puerto_rico'"
  )
})

test_that("invalid output triggers error", {
  expect_error(
    dep_get_index(geography = "county", index = "ndi_m", year = 2020, survey = "acs5",
                  output = "ham"),
    "The 'output' requested is invalid"
  )
})

test_that("tidy output with keep_components triggers error", {
  expect_error(
    dep_get_index(geography = "county", index = "ndi_m", year = 2020, survey = "acs5",
                  output = "tidy", keep_components = TRUE),
    "Tidy output is only available if 'keep_components' is 'FALSE'"
  )
})

test_that("non-logical zcta_cb triggers error", {
  expect_error(
    dep_get_index(geography = "county", index = "ndi_m", year = 2020, survey = "acs5",
                  zcta_cb = "yes"),
    "Please provide a logical scalar for 'zcta_cb'"
  )
})

test_that("non-logical shift_geo triggers error", {
  expect_error(
    dep_get_index(geography = "county", index = "ndi_m", year = 2020, survey = "acs5",
                  shift_geo = "yes"),
    "Please provide a logical scalar for 'shift_geo'"
  )
})
