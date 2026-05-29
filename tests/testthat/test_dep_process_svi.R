
# test dep_process_svi functions ------------------------------------------

# setup: load sample data for SVI styles
svi10_data <- dep_sample_data(index = "svi10")
svi14_data <- dep_sample_data(index = "svi14")
svi20_data <- dep_sample_data(index = "svi20")
svi20s_data <- dep_sample_data(index = "svi20s")

# test dep_process_svi_pri ------------------------------------------------

test_that("dep_process_svi_pri returns expected columns", {
  result <- dep_process_svi_pri(svi20_data, style = "svi20",
                                geography = "county", year = 2022, survey = "acs5")

  expect_true("GEOID" %in% names(result))
  expect_true("E_TOTPOP" %in% names(result))
  expect_true("M_TOTPOP" %in% names(result))
  expect_true("E_HU" %in% names(result))
  expect_true("M_HU" %in% names(result))

  expect_true("E_HH" %in% names(result))
  expect_true("M_HH" %in% names(result))
  expect_equal(nrow(result), nrow(svi20_data))
})

test_that("dep_process_svi_pri values are numeric and non-negative", {
  result <- dep_process_svi_pri(svi20_data, style = "svi20",
                                geography = "county", year = 2022, survey = "acs5")

  expect_true(is.numeric(result$E_TOTPOP))
  expect_true(all(result$E_TOTPOP >= 0, na.rm = TRUE))
  expect_true(is.numeric(result$E_HH))
  expect_true(all(result$E_HH >= 0, na.rm = TRUE))
})

# test dep_process_svi_ses ------------------------------------------------

test_that("dep_process_svi_ses returns SPL_THEME1 and SVI_SES for svi10", {
  result <- suppressWarnings(
    dep_process_svi_ses(svi10_data, style = "svi10", geography = "county",
                        year = 2022, survey = "acs5", keep_components = FALSE)
  )

  expect_true("SPL_THEME1" %in% names(result))
  expect_true("SVI_SES" %in% names(result))
  expect_true("GEOID" %in% names(result))
  expect_true(is.numeric(result$SVI_SES))
  expect_true(all(result$SVI_SES >= 0 & result$SVI_SES <= 1, na.rm = TRUE))
})

test_that("dep_process_svi_ses returns SPL_THEME1 and SVI_SES for svi20", {
  result <- suppressWarnings(
    dep_process_svi_ses(svi20_data, style = "svi20", geography = "county",
                        year = 2022, survey = "acs5", keep_components = FALSE)
  )

  expect_true("SPL_THEME1" %in% names(result))
  expect_true("SVI_SES" %in% names(result))
  expect_true(is.numeric(result$SVI_SES))
})

test_that("dep_process_svi_ses with keep_components returns detail columns for svi10", {
  result <- suppressWarnings(
    dep_process_svi_ses(svi10_data, style = "svi10", geography = "county",
                        year = 2022, survey = "acs5", keep_components = TRUE)
  )

  expect_true("EP_POV" %in% names(result))
  expect_true("EP_UNEMP" %in% names(result))
  expect_true("E_PCI" %in% names(result))
  expect_true("EP_NOHSDP" %in% names(result))
})

# test dep_process_svi_hhd ------------------------------------------------

test_that("dep_process_svi_hhd returns SPL_THEME2 and SVI_HCD for svi20", {
  result <- suppressWarnings(
    dep_process_svi_hhd(svi20_data, style = "svi20", geography = "county",
                        year = 2022, survey = "acs5", keep_components = FALSE)
  )

  expect_true("SPL_THEME2" %in% names(result))
  expect_true("SVI_HCD" %in% names(result))
  expect_true("GEOID" %in% names(result))
  expect_true(is.numeric(result$SVI_HCD))
  expect_true(all(result$SVI_HCD >= 0 & result$SVI_HCD <= 1, na.rm = TRUE))
})

test_that("dep_process_svi_hhd returns SPL_THEME2 and SVI_HCD for svi10", {
  result <- suppressWarnings(
    dep_process_svi_hhd(svi10_data, style = "svi10", geography = "county",
                        year = 2022, survey = "acs5", keep_components = FALSE)
  )

  expect_true("SPL_THEME2" %in% names(result))
  expect_true("SVI_HCD" %in% names(result))
})

# test dep_process_svi_msl ------------------------------------------------

test_that("dep_process_svi_msl returns SPL_THEME3 and SVI_MSL for svi20", {
  result <- suppressWarnings(
    dep_process_svi_msl(svi20_data, style = "svi20", geography = "county",
                        year = 2022, survey = "acs5", keep_components = FALSE)
  )

  expect_true("SPL_THEME3" %in% names(result))
  expect_true("SVI_MSL" %in% names(result))
  expect_true("GEOID" %in% names(result))
  expect_true(is.numeric(result$SVI_MSL))
  expect_true(all(result$SVI_MSL >= 0 & result$SVI_MSL <= 1, na.rm = TRUE))
})

test_that("dep_process_svi_msl returns SPL_THEME3 and SVI_MSL for svi14", {
  result <- suppressWarnings(
    dep_process_svi_msl(svi14_data, style = "svi14", geography = "county",
                        year = 2022, survey = "acs5", keep_components = FALSE)
  )

  expect_true("SPL_THEME3" %in% names(result))
  expect_true("SVI_MSL" %in% names(result))
})

# test dep_process_svi_htt ------------------------------------------------

test_that("dep_process_svi_htt returns SPL_THEME4 and SVI_HTT for svi20", {
  result <- suppressWarnings(
    dep_process_svi_htt(svi20_data, style = "svi20", geography = "county",
                        year = 2022, survey = "acs5", keep_components = FALSE)
  )

  expect_true("SPL_THEME4" %in% names(result))
  expect_true("SVI_HTT" %in% names(result))
  expect_true("GEOID" %in% names(result))
  expect_true(is.numeric(result$SVI_HTT))
  expect_true(all(result$SVI_HTT >= 0 & result$SVI_HTT <= 1, na.rm = TRUE))
})

test_that("dep_process_svi_htt returns SPL_THEME4 and SVI_HTT for svi10", {
  result <- suppressWarnings(
    dep_process_svi_htt(svi10_data, style = "svi10", geography = "county",
                        year = 2022, survey = "acs5", keep_components = FALSE)
  )

  expect_true("SPL_THEME4" %in% names(result))
  expect_true("SVI_HTT" %in% names(result))
})

# test dep_process_svi (composite) ----------------------------------------

test_that("dep_process_svi returns SVI score in [0,1] for svi20", {
  result <- suppressWarnings(
    dep_process_svi(svi20_data, style = "svi20", geography = "county",
                    year = 2022, survey = "acs5", keep_subscales = FALSE,
                    keep_components = FALSE, return_percentiles = FALSE,
                    multi_svi = FALSE, debug = FALSE)
  )

  expect_true("SVI" %in% names(result))
  expect_true("GEOID" %in% names(result))
  expect_true(is.numeric(result$SVI))
  expect_true(all(result$SVI >= 0 & result$SVI <= 1, na.rm = TRUE))
})

test_that("dep_process_svi returns subscales when keep_subscales = TRUE", {
  result <- suppressWarnings(
    dep_process_svi(svi20_data, style = "svi20", geography = "county",
                    year = 2022, survey = "acs5", keep_subscales = TRUE,
                    keep_components = FALSE, return_percentiles = FALSE,
                    multi_svi = FALSE, debug = FALSE)
  )

  expect_true("SVI" %in% names(result))
  expect_true("SVI_SES" %in% names(result))
  expect_true("SVI_HOU" %in% names(result))
  expect_true("SVI_REM" %in% names(result))
  expect_true("SVI_HTT" %in% names(result))
})

test_that("dep_process_svi returns percentiles scaled to 100 when return_percentiles = TRUE", {
  result <- suppressWarnings(
    dep_process_svi(svi20_data, style = "svi20", geography = "county",
                    year = 2022, survey = "acs5", keep_subscales = TRUE,
                    keep_components = FALSE, return_percentiles = TRUE,
                    multi_svi = FALSE, debug = FALSE)
  )

  expect_true(all(result$SVI >= 0 & result$SVI <= 100, na.rm = TRUE))
  expect_true(all(result$SVI_SES >= 0 & result$SVI_SES <= 100, na.rm = TRUE))
})

test_that("dep_process_svi with multi_svi = TRUE adds style postfix", {
  result <- suppressWarnings(
    dep_process_svi(svi20_data, style = "svi20", geography = "county",
                    year = 2022, survey = "acs5", keep_subscales = FALSE,
                    keep_components = FALSE, return_percentiles = FALSE,
                    multi_svi = TRUE, debug = FALSE)
  )

  expect_true("SVI_20" %in% names(result))
})

test_that("dep_process_svi works for svi10 style", {
  result <- suppressWarnings(
    dep_process_svi(svi10_data, style = "svi10", geography = "county",
                    year = 2022, survey = "acs5", keep_subscales = TRUE,
                    keep_components = FALSE, return_percentiles = FALSE,
                    multi_svi = FALSE, debug = FALSE)
  )

  expect_true("SVI" %in% names(result))
  expect_true("SVI_SES" %in% names(result))
  expect_true("SVI_HCD" %in% names(result))
  expect_true("SVI_MSL" %in% names(result))
  expect_true("SVI_HTT" %in% names(result))
})

test_that("dep_process_svi with keep_components = TRUE returns detailed output", {
  result <- suppressWarnings(
    dep_process_svi(svi20_data, style = "svi20", geography = "county",
                    year = 2022, survey = "acs5", keep_subscales = TRUE,
                    keep_components = TRUE, return_percentiles = FALSE,
                    multi_svi = FALSE, debug = FALSE)
  )

  expect_true("E_TOTPOP" %in% names(result))
  expect_true("SPL_THEME1" %in% names(result))
  expect_true("SPL_THEME2" %in% names(result))
  expect_true("SPL_THEME3" %in% names(result))
  expect_true("SPL_THEME4" %in% names(result))
})
