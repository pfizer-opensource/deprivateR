
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
  test_df <- tibble::tibble(
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
