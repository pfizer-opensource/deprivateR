
# test dep_process_ndi functions ------------------------------------------

# setup: load sample data
ndi_m_data <- dep_sample_data(index = "ndi_m")
ndi_pw_data <- dep_sample_data(index = "ndi_pw")

# test dep_process_ndi_m --------------------------------------------------

test_that("dep_process_ndi_m returns NDI_M column", {
  suppressWarnings(
    result <- dep_process_ndi_m(ndi_m_data, geography = "county",
                                year = 2022, survey = "acs5",
                                keep_components = FALSE,
                                return_percentiles = FALSE)
  )

  expect_true("GEOID" %in% names(result))
  expect_true("NDI_M" %in% names(result))
  expect_true(is.numeric(result$NDI_M))
  expect_equal(nrow(result), nrow(ndi_m_data))
})

test_that("dep_process_ndi_m with keep_components = TRUE returns component columns", {
  suppressWarnings(
    result <- dep_process_ndi_m(ndi_m_data, geography = "county",
                                year = 2022, survey = "acs5",
                                keep_components = TRUE,
                                return_percentiles = FALSE)
  )

  expect_true("NDI_M" %in% names(result))
  expect_true("EP_MPRO" %in% names(result))
  expect_true("EP_CROWD" %in% names(result))
  expect_true("EP_POVH" %in% names(result))
  expect_true("EP_FFH" %in% names(result))
  expect_true("EP_ASSIST" %in% names(result))
  expect_true("EP_LOWINC" %in% names(result))
  expect_true("EP_LTHS" %in% names(result))
  expect_true("EP_UNEMP" %in% names(result))
  # MOE columns should also be present
  expect_true("M_HH" %in% names(result))
  expect_true("MP_MPRO" %in% names(result))
})

test_that("dep_process_ndi_m with return_percentiles = TRUE returns values in [0, 100]", {
  suppressWarnings(
    result <- dep_process_ndi_m(ndi_m_data, geography = "county",
                                year = 2022, survey = "acs5",
                                keep_components = FALSE,
                                return_percentiles = TRUE)
  )

  expect_true(all(result$NDI_M >= 0 & result$NDI_M <= 100, na.rm = TRUE))
})

# test dep_process_ndi_pw -------------------------------------------------

test_that("dep_process_ndi_pw returns NDI_PW column", {
  suppressWarnings(
    result <- dep_process_ndi_pw(ndi_pw_data, geography = "county",
                                 year = 2022, survey = "acs5",
                                 keep_components = FALSE,
                                 return_percentiles = FALSE)
  )

  expect_true("GEOID" %in% names(result))
  expect_true("NDI_PW" %in% names(result))
  expect_true(is.numeric(result$NDI_PW))
  expect_equal(nrow(result), nrow(ndi_pw_data))
})

test_that("dep_process_ndi_pw with keep_components = TRUE returns component columns", {
  suppressWarnings(
    result <- dep_process_ndi_pw(ndi_pw_data, geography = "county",
                                 year = 2022, survey = "acs5",
                                 keep_components = TRUE,
                                 return_percentiles = FALSE)
  )

  expect_true("NDI_PW" %in% names(result))
  expect_true("E_TOTPOP" %in% names(result))
  expect_true("E_HH" %in% names(result))
  expect_true("E_HHINC" %in% names(result))
  expect_true("EP_FAMPOV" %in% names(result))
  expect_true("EP_UNEMP" %in% names(result))
  expect_true("EL_HHINC" %in% names(result))
  expect_true("EL_HOMEV" %in% names(result))
  # MOE columns should be present
  expect_true("M_TOTPOP" %in% names(result))
  expect_true("M_HH" %in% names(result))
})

test_that("dep_process_ndi_pw with return_percentiles = TRUE returns values in [0, 100]", {
  suppressWarnings(
    result <- dep_process_ndi_pw(ndi_pw_data, geography = "county",
                                 year = 2022, survey = "acs5",
                                 keep_components = FALSE,
                                 return_percentiles = TRUE)
  )

  expect_true(all(result$NDI_PW >= 0 & result$NDI_PW <= 100, na.rm = TRUE))
})

# test output consistency -------------------------------------------------

test_that("dep_process_ndi_m output has no duplicate GEOID values", {
  suppressWarnings(
    result <- dep_process_ndi_m(ndi_m_data, geography = "county",
                                year = 2022, survey = "acs5",
                                keep_components = FALSE,
                                return_percentiles = FALSE)
  )

  expect_equal(length(unique(result$GEOID)), nrow(result))
})

test_that("dep_process_ndi_pw output has no duplicate GEOID values", {
  suppressWarnings(
    result <- dep_process_ndi_pw(ndi_pw_data, geography = "county",
                                 year = 2022, survey = "acs5",
                                 keep_components = FALSE,
                                 return_percentiles = FALSE)
  )

  expect_equal(length(unique(result$GEOID)), nrow(result))
})
