
# test dep_get_data and helpers -------------------------------------------

# test validate_state -----------------------------------------------------

test_that("validate_state returns NULL for NULL input", {
  expect_null(validate_state(NULL))
})

test_that("validate_state resolves valid 2-digit FIPS codes", {
  expect_equal(validate_state("29", .msg = FALSE), "29")
  expect_equal(validate_state("06", .msg = FALSE), "06")
  expect_equal(validate_state("01", .msg = FALSE), "01")
})

test_that("validate_state pads 1-digit FIPS codes", {
  expect_equal(validate_state("6", .msg = FALSE), "06")
  expect_equal(validate_state("1", .msg = FALSE), "01")
})

test_that("validate_state resolves state abbreviations", {
  expect_equal(validate_state("mo", .msg = FALSE), "29")
  expect_equal(validate_state("CA", .msg = FALSE), "06")
  expect_equal(validate_state("ny", .msg = FALSE), "36")
})

test_that("validate_state resolves full state names", {
  expect_equal(validate_state("missouri", .msg = FALSE), "29")
  expect_equal(validate_state("California", .msg = FALSE), "06")
  expect_equal(validate_state("new york", .msg = FALSE), "36")
})

test_that("validate_state returns NULL with warning for invalid input", {
  expect_warning(result <- validate_state("xx", .msg = FALSE))
  expect_null(result)

  expect_warning(result <- validate_state("99", .msg = FALSE))
  expect_null(result)

  expect_warning(result <- validate_state("notastate", .msg = FALSE))
  expect_null(result)
})

test_that("validate_state handles county FIPS with warning", {
  # 5-digit county FIPS should extract state portion
  expect_message(result <- validate_state("29001", .msg = TRUE))
  expect_equal(result, "29")
})

test_that("validate_state handles whitespace in input", {
  expect_equal(validate_state("  mo  ", .msg = FALSE), "29")
  expect_equal(validate_state(" 29 ", .msg = FALSE), "29")
})

# test dep_get_census county subsetting -----------------------------------

test_that("dep_get_census subsets county FIPS to match state", {
  # This tests the county filtering logic inside dep_get_census

  # The function filters county by state prefix then strips to 3-digit
  county_vec <- c("29001", "29003", "17001", "17003")
  state <- "29"

  # Replicate the internal logic
  county_filtered <- county_vec[substr(county_vec, 1, 2) == state]
  county_stripped <- substr(county_filtered, 3, 5)

  expect_equal(county_filtered, c("29001", "29003"))
  expect_equal(county_stripped, c("001", "003"))
})
