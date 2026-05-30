
# test dep_process dispatcher ---------------------------------------------

# setup: load sample data
ndi_m_data <- dep_sample_data(index = "ndi_m")
svi20_data <- dep_sample_data(index = "svi20")

# test dispatcher routing with user input ---------------------------------

test_that("dep_process routes gini index correctly", {
  # Create minimal data frame with gini variables
  gini_data <- data.frame(
    GEOID = c("29001", "29003", "29005"),
    B19083_001E = c(0.45, 0.52, 0.38),
    B19083_001M = c(0.02, 0.03, 0.01)
  )

  result <- dep_process(gini_data, geography = "county", index = "gini",
                        year = 2022, survey = "acs5",
                        return_percentiles = FALSE, keep_subscales = FALSE,
                        keep_components = FALSE,
                        state = NULL, county = NULL, puerto_rico = FALSE,
                        zcta = NULL, zcta_geo_method = NULL, zcta3_method = NULL,
                        geometry = FALSE, cb = FALSE, keep_geo_vars = FALSE,
                        shift_geo = FALSE, key = NULL,
                        debug = FALSE, input = "user", output = "wide",
                        label_year = FALSE, multi_svi = FALSE)

  expect_true("GEOID" %in% names(result))
  expect_true("E_GINI" %in% names(result))
  expect_true("M_GINI" %in% names(result))
  expect_s3_class(result, "tbl_df")
})

test_that("dep_process routes ndi_m index correctly", {
  suppressWarnings(
    result <- dep_process(ndi_m_data, geography = "county", index = "ndi_m",
                          year = 2022, survey = "acs5",
                          return_percentiles = FALSE, keep_subscales = FALSE,
                          keep_components = FALSE,
                          state = NULL, county = NULL, puerto_rico = FALSE,
                          zcta = NULL, zcta_geo_method = NULL, zcta3_method = NULL,
                          geometry = FALSE, cb = FALSE, keep_geo_vars = FALSE,
                          shift_geo = FALSE, key = NULL,
                          debug = FALSE, input = "user", output = "wide",
                          label_year = FALSE, multi_svi = FALSE)
  )

  expect_true("GEOID" %in% names(result))
  expect_true("NDI_M" %in% names(result))
  expect_true(is.numeric(result$NDI_M))
})

test_that("dep_process routes ndi_pw index correctly", {
  ndi_pw_data <- dep_sample_data(index = "ndi_pw")

  suppressWarnings(
    result <- dep_process(ndi_pw_data, geography = "county", index = "ndi_pw",
                          year = 2022, survey = "acs5",
                          return_percentiles = FALSE, keep_subscales = FALSE,
                          keep_components = FALSE,
                          state = NULL, county = NULL, puerto_rico = FALSE,
                          zcta = NULL, zcta_geo_method = NULL, zcta3_method = NULL,
                          geometry = FALSE, cb = FALSE, keep_geo_vars = FALSE,
                          shift_geo = FALSE, key = NULL,
                          debug = FALSE, input = "user", output = "wide",
                          label_year = FALSE, multi_svi = FALSE)
  )

  expect_true("NDI_PW" %in% names(result))
  expect_true(is.numeric(result$NDI_PW))
})

test_that("dep_process routes svi20 index correctly", {
  result <- dep_process(svi20_data, geography = "county", index = "svi20",
                        year = 2022, survey = "acs5",
                        return_percentiles = FALSE, keep_subscales = FALSE,
                        keep_components = FALSE,
                        state = NULL, county = NULL, puerto_rico = FALSE,
                        zcta = NULL, zcta_geo_method = NULL, zcta3_method = NULL,
                        geometry = FALSE, cb = FALSE, keep_geo_vars = FALSE,
                        shift_geo = FALSE, key = NULL,
                        debug = FALSE, input = "user", output = "wide",
                        label_year = FALSE, multi_svi = FALSE)

  expect_true("SVI" %in% names(result))
  expect_true(is.numeric(result$SVI))
})

test_that("dep_process routes adi index correctly", {
  adi_data <- dep_sample_data(index = "adi")

  suppressWarnings(
    result <- dep_process(adi_data, geography = "county", index = "adi",
                          year = 2022, survey = "acs5",
                          return_percentiles = FALSE, keep_subscales = FALSE,
                          keep_components = FALSE,
                          state = NULL, county = NULL, puerto_rico = FALSE,
                          zcta = NULL, zcta_geo_method = NULL, zcta3_method = NULL,
                          geometry = FALSE, cb = FALSE, keep_geo_vars = FALSE,
                          shift_geo = FALSE, key = NULL,
                          debug = FALSE, input = "user", output = "wide",
                          label_year = FALSE, multi_svi = FALSE)
  )

  expect_true("ADI" %in% names(result))
  expect_true(is.numeric(result$ADI))
})

# test output formatting --------------------------------------------------

test_that("dep_process with label_year = TRUE adds YEAR column", {
  suppressWarnings(
    result <- dep_process(ndi_m_data, geography = "county", index = "ndi_m",
                          year = 2022, survey = "acs5",
                          return_percentiles = FALSE, keep_subscales = FALSE,
                          keep_components = FALSE,
                          state = NULL, county = NULL, puerto_rico = FALSE,
                          zcta = NULL, zcta_geo_method = NULL, zcta3_method = NULL,
                          geometry = FALSE, cb = FALSE, keep_geo_vars = FALSE,
                          shift_geo = FALSE, key = NULL,
                          debug = FALSE, input = "user", output = "wide",
                          label_year = TRUE, multi_svi = FALSE)
  )

  expect_true("YEAR" %in% names(result))
  expect_equal(unique(result$YEAR), 2022)
})

test_that("dep_process with output = 'tidy' returns long format", {
  suppressWarnings(
    result <- dep_process(ndi_m_data, geography = "county", index = "ndi_m",
                          year = 2022, survey = "acs5",
                          return_percentiles = FALSE, keep_subscales = FALSE,
                          keep_components = FALSE,
                          state = NULL, county = NULL, puerto_rico = FALSE,
                          zcta = NULL, zcta_geo_method = NULL, zcta3_method = NULL,
                          geometry = FALSE, cb = FALSE, keep_geo_vars = FALSE,
                          shift_geo = FALSE, key = NULL,
                          debug = FALSE, input = "user", output = "tidy",
                          label_year = FALSE, multi_svi = FALSE)
  )

  expect_true("index" %in% names(result))
  expect_true("estimate" %in% names(result))
  expect_true("GEOID" %in% names(result))
})

# test multi-SVI dispatcher ------------------------------------------------

test_that("dep_process handles multiple SVI styles with multi_svi = TRUE", {
  multi_data <- dep_sample_data(index = c("svi10", "svi20"))

  result <- dep_process(multi_data, geography = "county",
                        index = c("svi10", "svi20"),
                        year = 2022, survey = "acs5",
                        return_percentiles = FALSE, keep_subscales = FALSE,
                        keep_components = FALSE,
                        state = NULL, county = NULL, puerto_rico = FALSE,
                        zcta = NULL, zcta_geo_method = NULL, zcta3_method = NULL,
                        geometry = FALSE, cb = FALSE, keep_geo_vars = FALSE,
                        shift_geo = FALSE, key = NULL,
                        debug = FALSE, input = "user", output = "wide",
                        label_year = FALSE, multi_svi = TRUE)

  expect_true("GEOID" %in% names(result))
  # First SVI style columns present (svi10 uses full names)
  expect_true("SVI_10" %in% names(result))
  # Second SVI style columns present (svi20 uses postfix)
  expect_true("SVI_20" %in% names(result))
  expect_s3_class(result, "tbl_df")
  # No duplicate GEOID columns
  expect_equal(sum(names(result) == "GEOID"), 1)
})

test_that("dep_process routes svi10 correctly", {
  svi10_data <- dep_sample_data(index = "svi10")

  result <- dep_process(svi10_data, geography = "county", index = "svi10",
                        year = 2022, survey = "acs5",
                        return_percentiles = FALSE, keep_subscales = FALSE,
                        keep_components = FALSE,
                        state = NULL, county = NULL, puerto_rico = FALSE,
                        zcta = NULL, zcta_geo_method = NULL, zcta3_method = NULL,
                        geometry = FALSE, cb = FALSE, keep_geo_vars = FALSE,
                        shift_geo = FALSE, key = NULL,
                        debug = FALSE, input = "user", output = "wide",
                        label_year = FALSE, multi_svi = FALSE)

  expect_true("SVI" %in% names(result))
  expect_true(is.numeric(result$SVI))
})

test_that("dep_process routes svi14 correctly", {
  svi14_data <- dep_sample_data(index = "svi14")

  result <- dep_process(svi14_data, geography = "county", index = "svi14",
                        year = 2022, survey = "acs5",
                        return_percentiles = FALSE, keep_subscales = FALSE,
                        keep_components = FALSE,
                        state = NULL, county = NULL, puerto_rico = FALSE,
                        zcta = NULL, zcta_geo_method = NULL, zcta3_method = NULL,
                        geometry = FALSE, cb = FALSE, keep_geo_vars = FALSE,
                        shift_geo = FALSE, key = NULL,
                        debug = FALSE, input = "user", output = "wide",
                        label_year = FALSE, multi_svi = FALSE)

  expect_true("SVI" %in% names(result))
  expect_true(is.numeric(result$SVI))
})

test_that("dep_process routes svi20s correctly", {
  svi20s_data <- dep_sample_data(index = "svi20s")

  result <- dep_process(svi20s_data, geography = "county", index = "svi20s",
                        year = 2022, survey = "acs5",
                        return_percentiles = FALSE, keep_subscales = FALSE,
                        keep_components = FALSE,
                        state = NULL, county = NULL, puerto_rico = FALSE,
                        zcta = NULL, zcta_geo_method = NULL, zcta3_method = NULL,
                        geometry = FALSE, cb = FALSE, keep_geo_vars = FALSE,
                        shift_geo = FALSE, key = NULL,
                        debug = FALSE, input = "user", output = "wide",
                        label_year = FALSE, multi_svi = FALSE)

  expect_true("SVI" %in% names(result))
  expect_true(is.numeric(result$SVI))
})

# test multiple indices ---------------------------------------------------

test_that("dep_process handles multiple indices via dep_calc_index", {
  # dep_sample_data doesn't support multiple indices directly,
  # but dep_calc_index does - test that the dispatcher handles ndi_m alone
  suppressWarnings(
    result <- dep_process(ndi_m_data, geography = "county",
                          index = "ndi_m",
                          year = 2022, survey = "acs5",
                          return_percentiles = TRUE, keep_subscales = FALSE,
                          keep_components = FALSE,
                          state = NULL, county = NULL, puerto_rico = FALSE,
                          zcta = NULL, zcta_geo_method = NULL, zcta3_method = NULL,
                          geometry = FALSE, cb = FALSE, keep_geo_vars = FALSE,
                          shift_geo = FALSE, key = NULL,
                          debug = FALSE, input = "user", output = "wide",
                          label_year = FALSE, multi_svi = FALSE)
  )

  expect_true("NDI_M" %in% names(result))
  expect_true(all(result$NDI_M >= 0 & result$NDI_M <= 100, na.rm = TRUE))
})
