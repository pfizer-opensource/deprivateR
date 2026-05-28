
# setup ------------------------------------------------

test_data <- function() {
  sample_df <- dep_sample_data(index = "ndi_m")
  dep_calc_index(sample_df, geography = "county", index = "ndi_m", year = 2022)
}

# test errors ------------------------------------------------

test_that("invalid return argument triggers error", {
  df <- test_data()
  expect_error(
    dep_map_breaks(df, var = "NDI_M", new_var = "breaks", classes = 5,
                   style = "fisher", return = "ham"),
    "return.*only accepts.*col.*breaks"
  )
})

test_that("supplying breaks and return = 'breaks' triggers error", {
  df <- test_data()
  expect_error(
    dep_map_breaks(df, var = "NDI_M", new_var = "breaks",
                   breaks = c(0, 25, 50, 75, 100), return = "breaks"),
    "Returning breaks.*only possible.*breaks"
  )
})

test_that("missing both classes/style and breaks triggers error", {
  df <- test_data()
  expect_error(
    dep_map_breaks(df, var = "NDI_M", new_var = "breaks"),
    "classes.*style.*breaks"
  )
  expect_error(
    dep_map_breaks(df, var = "NDI_M", new_var = "breaks", classes = 5),
    "classes.*style.*breaks"
  )
  expect_error(
    dep_map_breaks(df, var = "NDI_M", new_var = "breaks", style = "fisher"),
    "classes.*style.*breaks"
  )
})

test_that("missing new_var with return = 'col' triggers error", {
  df <- test_data()
  expect_error(
    dep_map_breaks(df, var = "NDI_M", classes = 5, style = "fisher"),
    "new_var.*return.*col"
  )
})

test_that("nonexistent source variable triggers error", {
  df <- test_data()
  expect_error(
    dep_map_breaks(df, var = "FAKE_VAR", new_var = "breaks", classes = 5,
                   style = "fisher"),
    "var.*cannot be found"
  )
})

test_that("non-numeric source variable triggers error", {
  df <- test_data()
  expect_error(
    dep_map_breaks(df, var = "NAME", new_var = "breaks", classes = 5,
                   style = "fisher"),
    "var.*numeric.*integer"
  )
})

test_that("non-numeric classes triggers error", {
  df <- test_data()
  expect_error(
    dep_map_breaks(df, var = "NDI_M", new_var = "breaks", classes = "five",
                   style = "fisher"),
    "classes.*numeric.*integer"
  )
})

test_that("invalid style triggers error", {
  df <- test_data()
  expect_error(
    dep_map_breaks(df, var = "NDI_M", new_var = "breaks", classes = 5,
                   style = "invalid_style"),
    "style.*not supported"
  )
})

test_that("non-numeric breaks triggers error", {
  df <- test_data()
  expect_error(
    dep_map_breaks(df, var = "NDI_M", new_var = "breaks",
                   breaks = c("a", "b", "c")),
    "breaks.*numeric.*integer"
  )
})

test_that("too few breaks triggers error", {
  df <- test_data()
  expect_error(
    dep_map_breaks(df, var = "NDI_M", new_var = "breaks", breaks = c(0, 100)),
    "At least three values.*breaks"
  )
})

test_that("non-numeric sig_digits triggers error", {
  df <- test_data()
  expect_error(
    dep_map_breaks(df, var = "NDI_M", new_var = "breaks", classes = 5,
                   style = "fisher", sig_digits = "two"),
    "sig_digits.*numeric.*integer"
  )
})

test_that("non-logical show_warnings triggers error", {
  df <- test_data()
  expect_error(
    dep_map_breaks(df, var = "NDI_M", new_var = "breaks", classes = 5,
                   style = "fisher", show_warnings = "yes"),
    "show_warnings.*logical"
  )
})

# test happy paths ------------------------------------------------

test_that("return = 'col' adds a factor column after the source variable", {
  df <- test_data()
  result <- dep_map_breaks(df, var = "NDI_M", new_var = "map_breaks",
                           classes = 5, style = "fisher")

  expect_true("map_breaks" %in% names(result))
  expect_s3_class(result[["map_breaks"]], "factor")
  expect_equal(nlevels(result[["map_breaks"]]), 5)

  # new column should be placed after the source variable

  var_pos <- which(names(result) == "NDI_M")
  new_pos <- which(names(result) == "map_breaks")
  expect_equal(new_pos, var_pos + 1)
})

test_that("return = 'breaks' returns a numeric vector", {
  df <- test_data()
  result <- dep_map_breaks(df, var = "NDI_M", new_var = "breaks",
                           classes = 5, style = "fisher", return = "breaks")

  expect_true(is.numeric(result))
  expect_equal(length(result), 6)  # 5 classes = 6 break points
})

test_that("manual breaks work correctly", {
  df <- test_data()
  brks <- c(0, 25, 50, 75, max(df$NDI_M, na.rm = TRUE))
  result <- dep_map_breaks(df, var = "NDI_M", new_var = "map_breaks",
                           breaks = brks)

  expect_true("map_breaks" %in% names(result))
  expect_s3_class(result[["map_breaks"]], "factor")
  # 4 break points create 3 intervals but cut() with include.lowest may differ

  expect_equal(nlevels(result[["map_breaks"]]), length(brks) - 1)
})

test_that("quoted variable names work", {
  df <- test_data()
  result <- dep_map_breaks(df, var = "NDI_M", new_var = "map_breaks",
                           classes = 4, style = "quantile")

  expect_true("map_breaks" %in% names(result))
  expect_equal(nlevels(result[["map_breaks"]]), 4)
})

test_that("unquoted variable names work", {
  df <- test_data()
  result <- dep_map_breaks(df, var = NDI_M, new_var = map_breaks_uq,
                           classes = 4, style = "quantile")

  expect_true("map_breaks_uq" %in% names(result))
})

test_that("show_warnings = FALSE suppresses warnings", {
  df <- test_data()
  # Should not produce warnings
  expect_warning(
    dep_map_breaks(df, var = "NDI_M", new_var = "map_breaks", classes = 5,
                   style = "fisher", show_warnings = FALSE),
    NA
  )
})
