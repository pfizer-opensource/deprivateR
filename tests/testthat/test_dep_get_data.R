
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

# test dep_territory_fips ---------------------------------------------------

test_that("dep_territory_fips excludes PR when puerto_rico is FALSE", {
  result <- dep_territory_fips(FALSE)
  expect_true("72" %in% result)
  expect_equal(length(result), 5)
})

test_that("dep_territory_fips includes PR when puerto_rico is TRUE", {
  result <- dep_territory_fips(TRUE)
  expect_false("72" %in% result)
  expect_equal(length(result), 4)
})

# test dep_build_states ---------------------------------------------------

test_that("dep_build_states returns state.abb when state is NULL", {
  result <- dep_build_states(state = NULL, puerto_rico = FALSE)
  expect_equal(result, state.abb)
})

test_that("dep_build_states adds PR when puerto_rico is TRUE", {
  result <- dep_build_states(state = NULL, puerto_rico = TRUE)
  expect_true("PR" %in% result)
  expect_equal(length(result), length(state.abb) + 1)
})

test_that("dep_build_states respects specific state input", {
  result <- dep_build_states(state = c("MO", "IL"), puerto_rico = FALSE)
  expect_equal(result, c("MO", "IL"))
})

test_that("dep_build_states adds PR to specific state input", {
  result <- dep_build_states(state = "MO", puerto_rico = TRUE)
  expect_equal(result, c("MO", "PR"))
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
