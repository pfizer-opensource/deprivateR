
# setup ------------------------------------------------

test_data <- function() {
  dep_sample_data(index = "ndi_m")
}

# test errors ------------------------------------------------

test_that("non-data-frame .data triggers error", {
  expect_error(
    dep_percentiles(.data = "not a data frame", source_var = x, new_var = y),
    ".data.*not a data frame"
  )
})

test_that("nonexistent source_var triggers error", {
  df <- test_data()
  expect_error(
    dep_percentiles(df, source_var = FAKE_VAR, new_var = pctile),
    "source_var.*not found"
  )
})

# test happy paths ------------------------------------------------

test_that("dep_percentiles creates a new variable with percentile ranks", {
  df <- test_data()
  result <- dep_percentiles(df, source_var = B06009_001E, new_var = pop_pctile)

  expect_true("pop_pctile" %in% names(result))
  expect_true(is.numeric(result$pop_pctile))
  # Percentiles should range from 0 to 1
  expect_true(min(result$pop_pctile, na.rm = TRUE) >= 0)
  expect_true(max(result$pop_pctile, na.rm = TRUE) <= 1)
})

test_that("dep_percentiles overwrites source when new_var is omitted", {
  df <- test_data()
  original_vals <- df$B06009_001E
  result <- dep_percentiles(df, source_var = B06009_001E)

  # The column should still exist but contain percentile values
  expect_true("B06009_001E" %in% names(result))
  # Values should be between 0 and 1 (percentile ranks)
  expect_true(max(result$B06009_001E, na.rm = TRUE) <= 1)
})

test_that("existing new_var produces a warning", {
  df <- test_data()
  expect_warning(
    dep_percentiles(df, source_var = B06009_001E, new_var = GEOID),
    "new_var.*exists.*overwritten"
  )
})

test_that("dep_percent_rank produces correct values for simple case", {
  # Test the internal helper directly
  x <- c(10, 20, 30, 40, 50)
  result <- deprivateR:::dep_percent_rank(x)

  expect_equal(length(result), 5)
  expect_equal(result[1], 0)    # min should be 0
  expect_equal(result[5], 1)    # max should be 1
  # Values should be monotonically increasing for sorted input
  expect_true(all(diff(result) >= 0))
})

test_that("dep_percent_rank handles NA values", {
  x <- c(10, NA, 30, 40, 50)
  result <- deprivateR:::dep_percent_rank(x)

  expect_equal(length(result), 5)
  expect_true(is.na(result[2]))
  expect_false(any(is.na(result[-2])))
})

test_that("dep_percent_rank handles ties", {
  x <- c(10, 10, 30, 40, 50)
  result <- deprivateR:::dep_percent_rank(x)

  # Tied values should get the same rank
  expect_equal(result[1], result[2])
})
