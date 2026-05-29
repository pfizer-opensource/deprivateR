
# Baseline snapshot tests for EP_ (percent) and MP_ (derived MOE) calculations
# These tests capture exact numeric output of the current implementation to detect
# drift when refactoring the formulas into helper functions (#60, #61).

# setup: load sample data ------------------------------------------------

ndi_m_data <- dep_sample_data(index = "ndi_m")
ndi_pw_data <- dep_sample_data(index = "ndi_pw")
svi20_data <- dep_sample_data(index = "svi20")
svi10_data <- dep_sample_data(index = "svi10")

# test NDI-M percent calculations (EP_) ----------------------------------

test_that("dep_process_ndi_m EP_ columns match baseline values", {
  result <- suppressWarnings(
    dep_process_ndi_m(ndi_m_data, geography = "county",
                      year = 2022, survey = "acs5",
                      keep_components = TRUE, return_percentiles = FALSE)
  )

  # Row 1: GEOID 29001
  expect_equal(result$EP_MPRO[1], 3.6293294031, tolerance = 1e-6)
  expect_equal(result$EP_CROWD[1], 1.4440433213, tolerance = 1e-6)
  expect_equal(result$EP_POVH[1], 22.111913357, tolerance = 1e-6)
  expect_equal(result$EP_UNEMP[1], 5.464480874, tolerance = 1e-6)

  # Row 2: GEOID 29003
  expect_equal(result$EP_MPRO[2], 3.3920704846, tolerance = 1e-6)
  expect_equal(result$EP_CROWD[2], 0.3204661326, tolerance = 1e-6)
  expect_equal(result$EP_POVH[2], 9.934450109, tolerance = 1e-6)
  expect_equal(result$EP_UNEMP[2], 4.148496030, tolerance = 1e-6)

  # Row 3: GEOID 29005
  expect_equal(result$EP_MPRO[3], 1.6061452514, tolerance = 1e-6)
  expect_equal(result$EP_CROWD[3], 1.8715440238, tolerance = 1e-6)
  expect_equal(result$EP_POVH[3], 12.505316886, tolerance = 1e-6)
  expect_equal(result$EP_UNEMP[3], 2.601377200, tolerance = 1e-6)
})

# test NDI-M derived MOE calculations (MP_) ------------------------------

test_that("dep_process_ndi_m MP_ columns match baseline values", {
  result <- suppressWarnings(
    dep_process_ndi_m(ndi_m_data, geography = "county",
                      year = 2022, survey = "acs5",
                      keep_components = TRUE, return_percentiles = FALSE)
  )

  # Row 1: GEOID 29001
  expect_equal(result$MP_MPRO[1], 33.50595526, tolerance = 1e-4)
  expect_equal(result$MP_CROWD[1], 24.012277771, tolerance = 1e-4)
  expect_equal(result$MP_POVH[1], 91.91134564, tolerance = 1e-4)
  expect_equal(result$MP_UNEMP[1], 48.13439383, tolerance = 1e-4)

  # Row 2: GEOID 29003
  expect_equal(result$MP_MPRO[2], 26.83444100, tolerance = 1e-4)
  expect_equal(result$MP_CROWD[2], 8.798527786, tolerance = 1e-4)
  expect_equal(result$MP_POVH[2], 118.07993792, tolerance = 1e-4)
  expect_equal(result$MP_UNEMP[2], 32.96256297, tolerance = 1e-4)

  # Row 3: GEOID 29005
  expect_equal(result$MP_MPRO[3], 23.22321693, tolerance = 1e-4)
  expect_equal(result$MP_CROWD[3], 39.985055277, tolerance = 1e-4)
  expect_equal(result$MP_POVH[3], 71.05992922, tolerance = 1e-4)
  expect_equal(result$MP_UNEMP[3], 32.21838271, tolerance = 1e-4)
})

# test SVI20 SES theme percent calculations (EP_) ------------------------

test_that("dep_process_svi_ses EP_ columns match baseline for svi20", {
  result <- suppressWarnings(
    dep_process_svi_ses(svi20_data, style = "svi20", geography = "county",
                        year = 2022, survey = "acs5", keep_components = TRUE)
  )

  # Row 1: GEOID 29001
  expect_equal(result$EP_NOHSDP[1], 8.00, tolerance = 0.01)
  expect_equal(result$EP_UNEMP[1], 5.46, tolerance = 0.01)
  expect_equal(result$EP_POV150[1], 32.9, tolerance = 0.1)
  expect_equal(result$EP_UNINSUR[1], 8.06, tolerance = 0.01)

  # Row 2: GEOID 29003
  expect_equal(result$EP_NOHSDP[2], 5.53, tolerance = 0.01)
  expect_equal(result$EP_UNEMP[2], 4.15, tolerance = 0.01)
  expect_equal(result$EP_POV150[2], 14.7, tolerance = 0.1)
  expect_equal(result$EP_UNINSUR[2], 6.31, tolerance = 0.01)
})

# test SVI20 SES theme derived MOE calculations (MP_) --------------------

test_that("dep_process_svi_ses MP_ columns match baseline for svi20", {
  result <- suppressWarnings(
    dep_process_svi_ses(svi20_data, style = "svi20", geography = "county",
                        year = 2022, survey = "acs5", keep_components = TRUE)
  )

  # Row 1: GEOID 29001
  expect_equal(result$MP_NOHSDP[1], 91.9, tolerance = 0.1)
  expect_equal(result$MP_UNEMP[1], 48.1, tolerance = 0.1)
  expect_equal(result$MP_POV150[1], 1144, tolerance = 1)

  # Row 2: GEOID 29003
  expect_equal(result$MP_NOHSDP[2], 161, tolerance = 1)
  expect_equal(result$MP_UNEMP[2], 33.0, tolerance = 0.1)
  expect_equal(result$MP_POV150[2], 389, tolerance = 1)
})

# test SVI20 HTT theme percent calculations (EP_) ------------------------

test_that("dep_process_svi_htt EP_ columns match baseline for svi20", {
  result <- suppressWarnings(
    dep_process_svi_htt(svi20_data, style = "svi20", geography = "county",
                        year = 2022, survey = "acs5", keep_components = TRUE)
  )

  # Row 1: GEOID 29001
  expect_equal(result$EP_MUNIT[1], 3.082614057, tolerance = 1e-6)
  expect_equal(result$EP_MOBILE[1], 7.310199049, tolerance = 1e-6)
  expect_equal(result$EP_CROWD[1], 1.4440433213, tolerance = 1e-6)
  expect_equal(result$EP_NOVEH[1], 7.017148014, tolerance = 1e-6)
  expect_equal(result$EP_GROUPQ[1], 9.447013716, tolerance = 1e-6)

  # Row 2: GEOID 29003
  expect_equal(result$EP_MUNIT[2], 2.243759958, tolerance = 1e-6)
  expect_equal(result$EP_MOBILE[2], 6.186935741, tolerance = 1e-6)
  expect_equal(result$EP_CROWD[2], 0.3204661326, tolerance = 1e-6)
  expect_equal(result$EP_NOVEH[2], 3.481427531, tolerance = 1e-6)
  expect_equal(result$EP_GROUPQ[2], 1.012784327, tolerance = 1e-6)
})

# test SVI20 HTT theme derived MOE calculations (MP_) --------------------

test_that("dep_process_svi_htt MP_ columns match baseline for svi20", {
  result <- suppressWarnings(
    dep_process_svi_htt(svi20_data, style = "svi20", geography = "county",
                        year = 2022, survey = "acs5", keep_components = TRUE)
  )

  # Row 1: GEOID 29001
  expect_equal(result$MP_MUNIT[1], 432.21900777, tolerance = 1e-2)
  expect_equal(result$MP_MOBILE[1], 442.36384490, tolerance = 1e-2)
  expect_equal(result$MP_CROWD[1], 24.012277771, tolerance = 1e-4)
  expect_equal(result$MP_NOVEH[1], 49.01796355, tolerance = 1e-4)
  # MP_GROUPQ is NA for row 1 (edge case — negative radicand)
  expect_true(is.na(result$MP_GROUPQ[1]) || is.nan(result$MP_GROUPQ[1]))

  # Row 2: GEOID 29003
  expect_equal(result$MP_MUNIT[2], 367.29622379, tolerance = 1e-2)
  expect_equal(result$MP_MOBILE[2], 309.61560988, tolerance = 1e-2)
  expect_equal(result$MP_CROWD[2], 8.798527786, tolerance = 1e-4)
  expect_equal(result$MP_NOVEH[2], 67.54039074, tolerance = 1e-4)
})

# test SVI10 style also produces consistent results ----------------------

test_that("dep_process_svi_ses EP_ columns match baseline for svi10", {
  result <- suppressWarnings(
    dep_process_svi_ses(svi10_data, style = "svi10", geography = "county",
                        year = 2022, survey = "acs5", keep_components = TRUE)
  )

  # svi10 uses EP_POV instead of EP_POV150

  expect_true("EP_POV" %in% names(result))
  expect_true("EP_NOHSDP" %in% names(result))
  expect_true("EP_UNEMP" %in% names(result))
  expect_true("MP_POV" %in% names(result))
  expect_true("MP_NOHSDP" %in% names(result))
  expect_true("MP_UNEMP" %in% names(result))

  # Verify numeric and finite for valid data
  expect_true(is.numeric(result$EP_POV))
  expect_true(is.numeric(result$MP_POV))
  expect_true(all(is.finite(result$EP_POV) | is.na(result$EP_POV)))
})

# test NDI-PW percent calculations (EP_) ---------------------------------

test_that("dep_process_ndi_pw EP_ columns match baseline values", {
  result <- suppressWarnings(
    dep_process_ndi_pw(ndi_pw_data, geography = "county",
                       year = 2022, survey = "acs5",
                       keep_components = TRUE, return_percentiles = FALSE)
  )

  # Verify key columns exist and have expected types
  expect_true("EP_FAMPOV" %in% names(result))
  expect_true("EP_UNEMP" %in% names(result))
  expect_true(is.numeric(result$EP_FAMPOV))
  expect_true(is.numeric(result$EP_UNEMP))

  # Values should be in valid percent range [0, 100] for valid data
  expect_true(all(result$EP_FAMPOV >= 0 & result$EP_FAMPOV <= 100, na.rm = TRUE))
  expect_true(all(result$EP_UNEMP >= 0 & result$EP_UNEMP <= 100, na.rm = TRUE))
})

# test that no Inf/NaN are produced in sample data -----------------------

test_that("NDI-M produces no Inf or NaN in EP/MP columns for sample data", {
  result <- suppressWarnings(
    dep_process_ndi_m(ndi_m_data, geography = "county",
                      year = 2022, survey = "acs5",
                      keep_components = TRUE, return_percentiles = FALSE)
  )

  ep_mp_cols <- grep("^EP_|^MP_", names(result), value = TRUE)
  for (col in ep_mp_cols) {
    inf_count <- sum(is.infinite(result[[col]]), na.rm = TRUE)
    nan_count <- sum(is.nan(result[[col]]), na.rm = TRUE)
    expect_equal(inf_count, 0, info = paste("Inf found in", col))
    expect_equal(nan_count, 0, info = paste("NaN found in", col))
  }
})

test_that("SVI20 SES produces no Inf in EP/MP columns for sample data", {
  result <- suppressWarnings(
    dep_process_svi_ses(svi20_data, style = "svi20", geography = "county",
                        year = 2022, survey = "acs5", keep_components = TRUE)
  )

  ep_mp_cols <- grep("^EP_|^MP_", names(result), value = TRUE)
  for (col in ep_mp_cols) {
    inf_count <- sum(is.infinite(result[[col]]), na.rm = TRUE)
    expect_equal(inf_count, 0, info = paste("Inf found in", col))
  }
})

test_that("SVI20 HTT produces no Inf in EP/MP columns for sample data", {
  result <- suppressWarnings(
    dep_process_svi_htt(svi20_data, style = "svi20", geography = "county",
                        year = 2022, survey = "acs5", keep_components = TRUE)
  )

  ep_mp_cols <- grep("^EP_|^MP_", names(result), value = TRUE)
  for (col in ep_mp_cols) {
    inf_count <- sum(is.infinite(result[[col]]), na.rm = TRUE)
    expect_equal(inf_count, 0, info = paste("Inf found in", col))
  }
})
