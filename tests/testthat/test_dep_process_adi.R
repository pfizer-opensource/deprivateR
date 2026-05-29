
# test dep_process_adi and dep_process_gini -------------------------------

# setup: load sample data
adi_data <- dep_sample_data(index = "adi")

# test dep_process_adi ----------------------------------------------------

test_that("dep_process_adi returns ADI column", {
  expect_warning(
    result <- dep_process_adi(adi_data, geography = "county",
                              year = 2022, survey = "acs5",
                              keep_subscales = FALSE,
                              keep_components = FALSE,
                              return_percentiles = FALSE),
    "C24010_039.*C24010_040"
  )

  expect_true("GEOID" %in% names(result))
  expect_true("ADI" %in% names(result))
  expect_true(is.numeric(result$ADI))
  expect_equal(nrow(result), nrow(adi_data))
})

test_that("dep_process_adi with keep_subscales = TRUE returns subscale columns", {
  expect_warning(
    result <- dep_process_adi(adi_data, geography = "county",
                              year = 2022, survey = "acs5",
                              keep_subscales = TRUE,
                              keep_components = FALSE,
                              return_percentiles = FALSE),
    "C24010_039.*C24010_040"
  )

  expect_true("ADI" %in% names(result))
  expect_true("ADI3_FINS" %in% names(result))
  expect_true("ADI3_ECON" %in% names(result))
  expect_true("ADI3_EDU" %in% names(result))
})

test_that("dep_process_adi with keep_components = TRUE returns indicator columns", {
  expect_warning(
    result <- dep_process_adi(adi_data, geography = "county",
                              year = 2022, survey = "acs5",
                              keep_subscales = FALSE,
                              keep_components = TRUE,
                              return_percentiles = FALSE),
    "C24010_039.*C24010_040"
  )

  expect_true("ADI" %in% names(result))
  # should have more columns than just GEOID + ADI
  expect_gt(ncol(result), 2)
})

test_that("dep_process_adi with return_percentiles = TRUE returns values in [0, 100]", {
  expect_warning(
    result <- dep_process_adi(adi_data, geography = "county",
                              year = 2022, survey = "acs5",
                              keep_subscales = FALSE,
                              keep_components = FALSE,
                              return_percentiles = TRUE),
    "C24010_039.*C24010_040"
  )

  expect_true(all(result$ADI >= 0 & result$ADI <= 100, na.rm = TRUE))
})

test_that("dep_process_adi with percentiles + subscales scales subscales to [0, 100]", {
  expect_warning(
    result <- dep_process_adi(adi_data, geography = "county",
                              year = 2022, survey = "acs5",
                              keep_subscales = TRUE,
                              keep_components = FALSE,
                              return_percentiles = TRUE),
    "C24010_039.*C24010_040"
  )

  expect_true(all(result$ADI >= 0 & result$ADI <= 100, na.rm = TRUE))
  expect_true(all(result$ADI3_FINS >= 0 & result$ADI3_FINS <= 100, na.rm = TRUE))
  expect_true(all(result$ADI3_ECON >= 0 & result$ADI3_ECON <= 100, na.rm = TRUE))
  expect_true(all(result$ADI3_EDU >= 0 & result$ADI3_EDU <= 100, na.rm = TRUE))
})

test_that("dep_process_adi output has no duplicate GEOID values", {
  expect_warning(
    result <- dep_process_adi(adi_data, geography = "county",
                              year = 2022, survey = "acs5",
                              keep_subscales = FALSE,
                              keep_components = FALSE,
                              return_percentiles = FALSE),
    "C24010_039.*C24010_040"
  )

  expect_equal(length(unique(result$GEOID)), nrow(result))
})

# test dep_process_gini ---------------------------------------------------

test_that("dep_process_gini returns E_GINI and M_GINI columns", {
  # Create minimal data frame with gini variables
  gini_data <- data.frame(
    GEOID = c("29001", "29003", "29005"),
    B19083_001E = c(0.45, 0.52, 0.38),
    B19083_001M = c(0.02, 0.03, 0.01)
  )

  result <- dep_process_gini(gini_data, geography = "county",
                             year = 2022, survey = "acs5")

  expect_true("GEOID" %in% names(result))
  expect_true("E_GINI" %in% names(result))
  expect_true("M_GINI" %in% names(result))
  expect_true(is.numeric(result$E_GINI))
  expect_true(is.numeric(result$M_GINI))
})

test_that("dep_process_gini Gini values are preserved correctly", {
  gini_data <- data.frame(
    GEOID = c("29001", "29003", "29005"),
    B19083_001E = c(0.45, 0.52, 0.38),
    B19083_001M = c(0.02, 0.03, 0.01)
  )

  result <- dep_process_gini(gini_data, geography = "county",
                             year = 2022, survey = "acs5")

  expect_equal(result$E_GINI, c(0.45, 0.52, 0.38))
  expect_equal(result$M_GINI, c(0.02, 0.03, 0.01))
})
