
# test dep_quantile_label ------------------------------------------------

test_that("dep_quantile_label returns correct labels", {
  expect_equal(deprivateR:::dep_quantile_label(4L), "Quartile")
  expect_equal(deprivateR:::dep_quantile_label(5L), "Quintile")
  expect_equal(deprivateR:::dep_quantile_label(6L), "Sextile")
  expect_equal(deprivateR:::dep_quantile_label(7L), "Septile")
  expect_equal(deprivateR:::dep_quantile_label(8L), "Octile")
  expect_equal(deprivateR:::dep_quantile_label(9L), "Nonile")
  expect_equal(deprivateR:::dep_quantile_label(10L), "Decile")
})

test_that("dep_quantile_label returns generic for unsupported values", {
  expect_equal(deprivateR:::dep_quantile_label(11L), "Quantile")
  expect_equal(deprivateR:::dep_quantile_label(20L), "Quantile")
})

# test dep_ordinal ------------------------------------------------

test_that("dep_ordinal returns correctly formatted ordinals", {
  result_2 <- deprivateR:::dep_ordinal(2)
  result_3 <- deprivateR:::dep_ordinal(3)

  # Should be title-cased
  expect_true(grepl("^[A-Z]", result_2))
  expect_true(grepl("^[A-Z]", result_3))
  # Should contain ordinal suffix
  expect_true(grepl("nd$", result_2, ignore.case = TRUE))
  expect_true(grepl("rd$", result_3, ignore.case = TRUE))
})

# test validate_state ------------------------------------------------

test_that("validate_state handles valid FIPS codes", {
  result <- deprivateR:::validate_state("29", .msg = FALSE)
  expect_equal(result, "29")
})

test_that("validate_state handles single-digit FIPS codes", {
  result <- deprivateR:::validate_state("6", .msg = FALSE)
  expect_equal(result, "06")
})

test_that("validate_state handles valid abbreviations", {
  result <- deprivateR:::validate_state("mo", .msg = FALSE)
  expect_equal(result, "29")
})

test_that("validate_state returns NULL for invalid input", {
  expect_warning(
    result <- deprivateR:::validate_state("ZZ", .msg = FALSE),
    "not a valid FIPS code"
  )
  expect_null(result)
})

test_that("validate_state returns NULL for NULL input", {
  result <- deprivateR:::validate_state(NULL, .msg = FALSE)
  expect_null(result)
})

# test pivot_demos ------------------------------------------------

test_that("pivot_demos aggregates estimate and moe columns", {
  # Create minimal test data mimicking Census format
  test_df <- dplyr::tibble(
    GEOID = rep(c("29001", "29003"), each = 1),
    B01001_001E = c(100, 200),
    B01001_001M = c(10, 20)
  )

  result <- deprivateR:::pivot_demos(test_df, vars = c("B01001_001E", "B01001_001M"))

  expect_true("GEOID" %in% names(result))
  expect_true("estimate" %in% names(result))
  expect_true("moe" %in% names(result))
  expect_equal(nrow(result), 2)
})

# test dep_safe_pct ------------------------------------------------

test_that("dep_safe_pct computes correct percentages", {
  expect_equal(deprivateR:::dep_safe_pct(50, 200), 25)
  expect_equal(deprivateR:::dep_safe_pct(1, 4), 25)
  expect_equal(deprivateR:::dep_safe_pct(0, 100), 0)
})

test_that("dep_safe_pct is vectorized", {
  result <- deprivateR:::dep_safe_pct(c(10, 20, 30), c(100, 200, 300))
  expect_equal(result, c(10, 10, 10))
})

test_that("dep_safe_pct returns NA for zero denominator", {
  expect_true(is.na(deprivateR:::dep_safe_pct(50, 0)))
  result <- deprivateR:::dep_safe_pct(c(10, 20), c(0, 100))
  expect_true(is.na(result[1]))
  expect_equal(result[2], 20)
})

test_that("dep_safe_pct returns NA for NA denominator", {
  expect_true(is.na(deprivateR:::dep_safe_pct(50, NA)))
})

test_that("dep_safe_pct handles NA numerator", {
  expect_true(is.na(deprivateR:::dep_safe_pct(NA, 100)))
})

# test dep_derived_moe ------------------------------------------------

test_that("dep_derived_moe computes correct values for valid inputs", {
  # Manual calculation: ((sqrt(10^2 - ((50/100)^2 * 8^2))) / 8) * 100
  # = ((sqrt(100 - (0.25 * 64))) / 8) * 100
  # = ((sqrt(100 - 16)) / 8) * 100
  # = ((sqrt(84)) / 8) * 100
  # = (9.165151 / 8) * 100
  # = 114.5644
  result <- deprivateR:::dep_derived_moe(10, 50, 8, 8)
  expect_equal(result, (sqrt(100 - 16) / 8) * 100, tolerance = 1e-10)
})

test_that("dep_derived_moe is vectorized", {
  result <- deprivateR:::dep_derived_moe(
    estimate_moe = c(10, 20),
    proportion = c(50, 25),
    denominator_moe = c(8, 16),
    denominator = c(8, 16)
  )
  expect_length(result, 2)
  expect_true(all(is.finite(result)))
})

test_that("dep_derived_moe returns NA for zero denominator", {
  result <- deprivateR:::dep_derived_moe(10, 50, 8, 0)
  expect_true(is.na(result))
})

test_that("dep_derived_moe returns NA for NA denominator", {
  result <- deprivateR:::dep_derived_moe(10, 50, 8, NA)
  expect_true(is.na(result))
})

test_that("dep_derived_moe returns NA for negative radicand", {
  # radicand = 5^2 - ((90/100)^2 * 10^2) = 25 - 81 = -56

  result <- deprivateR:::dep_derived_moe(5, 90, 10, 10)
  expect_true(is.na(result))
})

test_that("dep_derived_moe handles mixed valid/invalid in vector", {
  result <- deprivateR:::dep_derived_moe(
    estimate_moe = c(10, 5),
    proportion = c(50, 90),
    denominator_moe = c(8, 10),
    denominator = c(8, 10)
  )
  expect_true(is.finite(result[1]))
  expect_true(is.na(result[2]))  # negative radicand
})
