context("test dep_quantiles function")

# setup ------------------------------------------------

test_data <- function() {
  sample_df <- dep_sample_data(index = "ndi_m")
  dep_calc_index(sample_df, geography = "county", index = "ndi_m", year = 2022,
                 return_percentiles = TRUE)
}

# test errors ------------------------------------------------

test_that("non-data-frame .data triggers error", {
  expect_error(
    dep_quantiles(.data = "not a data frame", source_var = "x", new_var = "y"),
    "The object passed to the '.data' argument must be a data frame."
  )
})

test_that("nonexistent source_var triggers error", {
  df <- test_data()
  expect_error(
    dep_quantiles(df, source_var = "FAKE_VAR", new_var = "q"),
    "The variable name passed to the 'source_var' argument does not exist"
  )
})

test_that("non-integer n triggers error", {
  df <- test_data()
  expect_error(
    dep_quantiles(df, source_var = NDI_M, new_var = q, n = 4),
    "The 'n' argument must be an integer"
  )
})

test_that("n < 2L triggers error", {
  df <- test_data()
  expect_error(
    dep_quantiles(df, source_var = NDI_M, new_var = q, n = 1L),
    "The 'n' argument must be 2L or greater"
  )
})

test_that("invalid return triggers error", {
  df <- test_data()
  expect_error(
    dep_quantiles(df, source_var = NDI_M, new_var = q, return = "ham"),
    "The 'return' argument must be either 'label' or 'factor'"
  )
})

test_that("existing new_var produces a warning", {
  df <- test_data()
  expect_warning(
    dep_quantiles(df, source_var = NDI_M, new_var = NDI_M),
    "already exists in the data"
  )
})

# test happy paths - labels ------------------------------------------------

test_that("default quartiles return correct labels", {
  df <- test_data()
  result <- dep_quantiles(df, source_var = NDI_M, new_var = ndi_quartiles)

  expect_true("ndi_quartiles" %in% names(result))
  labels <- unique(sort(result$ndi_quartiles))
  expect_equal(length(labels), 4)
  expect_true(grepl("Lowest Quartile", labels[1]))
  expect_true(grepl("Highest Quartile", labels[4]))
})

test_that("n = 2L returns median split labels", {
  df <- test_data()
  result <- dep_quantiles(df, source_var = NDI_M, new_var = ndi_median, n = 2L)

  labels <- unique(sort(result$ndi_median))
  expect_equal(length(labels), 2)
  expect_true(grepl("Lower Than Median", labels[1]))
  expect_true(grepl("Greater Than Median", labels[2]))
})

test_that("n = 3L returns tertile labels", {
  df <- test_data()
  result <- dep_quantiles(df, source_var = NDI_M, new_var = ndi_tertiles, n = 3L)

  labels <- unique(sort(result$ndi_tertiles))
  expect_equal(length(labels), 3)
  expect_true(grepl("Lowest Tertile", labels[1]))
  expect_true(grepl("Middle Tertile", labels[2]))
  expect_true(grepl("Highest Tertile", labels[3]))
})

test_that("n = 5L returns quintile labels", {
  df <- test_data()
  result <- dep_quantiles(df, source_var = NDI_M, new_var = ndi_quintiles, n = 5L)

  labels <- unique(sort(result$ndi_quintiles))
  expect_equal(length(labels), 5)
  expect_true(grepl("Lowest Quintile", labels[1]))
  expect_true(grepl("Highest Quintile", labels[5]))
})

test_that("n = 10L returns decile labels", {
  df <- test_data()
  result <- dep_quantiles(df, source_var = NDI_M, new_var = ndi_deciles, n = 10L)

  labels <- unique(result$ndi_deciles)
  expect_equal(length(labels), 10)
  expect_true(any(grepl("Lowest Decile", labels)))
  expect_true(any(grepl("Highest Decile", labels)))
})

# test happy paths - factor ------------------------------------------------

test_that("return = 'factor' returns a factor column", {
  df <- test_data()
  result <- dep_quantiles(df, source_var = NDI_M, new_var = ndi_q_factor,
                          n = 4L, return = "factor")

  expect_true("ndi_q_factor" %in% names(result))
  expect_s3_class(result[["ndi_q_factor"]], "factor")
  expect_equal(nlevels(result[["ndi_q_factor"]]), 4)
})

# test quoted vs unquoted ------------------------------------------------

test_that("quoted variable names work", {
  df <- test_data()
  result <- dep_quantiles(df, source_var = "NDI_M", new_var = "ndi_q_quoted")

  expect_true("ndi_q_quoted" %in% names(result))
})
