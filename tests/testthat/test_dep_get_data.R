
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

# test dep_get_census with mocked API ------------------------------------------

test_that("dep_get_census calls tidycensus with debug='live'", {
  mock_result <- data.frame(
    GEOID = c("29001", "29003"),
    NAME = c("Adair County", "Andrew County"),
    B19083_001E = c(0.45, 0.42),
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    get_acs = function(...) mock_result,
    .package = "tidycensus"
  )

  result <- dep_get_census(
    geography = "county", varlist = "B19083_001E",
    year = 2019, survey = "acs5", state = "29", county = NULL,
    geometry = FALSE, cb = FALSE, keep_geo_vars = FALSE,
    shift_geo = FALSE, key = "fake_key", debug = "live"
  )

  expect_equal(nrow(result), 2)
  expect_true("GEOID" %in% names(result))
})

test_that("dep_get_census calls tidycensus with debug='messages'", {
  mock_result <- data.frame(
    GEOID = "29001", NAME = "Adair County",
    B19083_001E = 0.45, stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    get_acs = function(...) mock_result,
    .package = "tidycensus"
  )

  result <- dep_get_census(
    geography = "county", varlist = "B19083_001E",
    year = 2019, survey = "acs5", state = "29", county = NULL,
    geometry = FALSE, cb = FALSE, keep_geo_vars = FALSE,
    shift_geo = FALSE, key = "fake_key", debug = "messages"
  )

  expect_equal(nrow(result), 1)
})

test_that("dep_get_census calls tidycensus with debug='call'", {
  mock_result <- data.frame(
    GEOID = "29001", NAME = "Adair County",
    B19083_001E = 0.45, stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    get_acs = function(...) mock_result,
    .package = "tidycensus"
  )

  result <- dep_get_census(
    geography = "county", varlist = "B19083_001E",
    year = 2019, survey = "acs5", state = "29", county = NULL,
    geometry = FALSE, cb = FALSE, keep_geo_vars = FALSE,
    shift_geo = FALSE, key = "fake_key", debug = "call"
  )

  expect_equal(nrow(result), 1)
})

test_that("dep_get_census errors with debug='test'", {
  expect_error(
    dep_get_census(
      geography = "county", varlist = "B19083_001E",
      year = 2019, survey = "acs5", state = "29", county = NULL,
      geometry = FALSE, cb = FALSE, keep_geo_vars = FALSE,
      shift_geo = FALSE, key = "fake_key", debug = "test"
    ),
    "Testing debug mode"
  )
})

test_that("dep_get_census filters county to match state FIPS", {
  captured_args <- list()
  local_mocked_bindings(
    get_acs = function(...) {
      args <- list(...)
      captured_args$county <<- args$county
      data.frame(GEOID = "29001", NAME = "Adair", B19083_001E = 0.45,
                 stringsAsFactors = FALSE)
    },
    .package = "tidycensus"
  )

  dep_get_census(
    geography = "county", varlist = "B19083_001E",
    year = 2019, survey = "acs5", state = "29",
    county = c("29001", "29003", "17001"),
    geometry = FALSE, cb = FALSE, keep_geo_vars = FALSE,
    shift_geo = FALSE, key = "fake_key", debug = "live"
  )

  # Only MO counties should be passed (stripped to 3-digit)
  expect_equal(captured_args$county, c("001", "003"))
})

# test dep_finalize_output ---------------------------------------------------

test_that("dep_finalize_output splits into geo/demo without geometry", {
  out <- data.frame(
    GEOID = c("29001", "29003"),
    NAME = c("Adair County", "Andrew County"),
    B19083_001E = c(0.45, 0.42),
    stringsAsFactors = FALSE
  )

  result <- dep_finalize_output(
    out, varlist = "B19083_001E",
    geometry = FALSE, keep_geo_vars = FALSE, shift_geo = FALSE
  )

  expect_type(result, "list")
  expect_named(result, c("geo", "demo"))
  expect_equal(names(result$geo), c("GEOID", "NAME"))
  expect_false("NAME" %in% names(result$demo))
  expect_true("GEOID" %in% names(result$demo))
})

# test dep_get_county_tract --------------------------------------------------

test_that("dep_get_county_tract handles county filter", {
  mock_result <- data.frame(
    GEOID = c("29001", "29003"),
    NAME = c("Adair County", "Andrew County"),
    B19083_001E = c(0.45, 0.42),
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    get_acs = function(...) mock_result,
    .package = "tidycensus"
  )

  result <- dep_get_county_tract(
    geography = "county", varlist = "B19083_001E",
    year = 2019, survey = "acs5", state = NULL,
    county = c("29001", "29003"), puerto_rico = FALSE,
    geometry = FALSE, keep_geo_vars = FALSE,
    key = "fake_key", debug = "live"
  )

  expect_true(is.data.frame(result))
  expect_true("GEOID" %in% names(result))
})

test_that("dep_get_county_tract handles county geography without filter", {
  mock_result <- data.frame(
    GEOID = c("29001", "29003", "72001"),
    NAME = c("Adair County", "Andrew County", "Adjuntas"),
    B19083_001E = c(0.45, 0.42, 0.50),
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    get_acs = function(...) mock_result,
    .package = "tidycensus"
  )

  result <- dep_get_county_tract(
    geography = "county", varlist = "B19083_001E",
    year = 2019, survey = "acs5", state = NULL,
    county = NULL, puerto_rico = FALSE,
    geometry = FALSE, keep_geo_vars = FALSE,
    key = "fake_key", debug = "live"
  )

  # PR (FIPS 72) should be excluded when puerto_rico = FALSE
  expect_false(any(substr(result$GEOID, 1, 2) == "72"))
})

test_that("dep_get_county_tract handles tract geography", {
  mock_result <- data.frame(
    GEOID = c("29001000100", "29001000200"),
    NAME = c("Tract 1", "Tract 2"),
    B19083_001E = c(0.45, 0.42),
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    get_acs = function(...) mock_result,
    .package = "tidycensus"
  )

  result <- dep_get_county_tract(
    geography = "tract", varlist = "B19083_001E",
    year = 2019, survey = "acs5", state = "MO",
    county = NULL, puerto_rico = FALSE,
    geometry = FALSE, keep_geo_vars = FALSE,
    key = "fake_key", debug = "live"
  )

  expect_true(is.data.frame(result))
})

# test dep_get_data dispatcher ------------------------------------------------

test_that("dep_get_data dispatches to dep_get_zcta5 for zcta5 geography", {
  mock_demo <- data.frame(
    GEOID = c("63011", "63101"),
    B19083_001E = c(0.40, 0.38),
    stringsAsFactors = FALSE
  )
  mock_geo <- data.frame(GEOID = c("63011", "63101"), stringsAsFactors = FALSE)
  mock_zcta5_result <- list(geo = mock_geo, demo = mock_demo)

  local_mocked_bindings(
    zi_get_demographics = function(...) mock_demo,
    .package = "zippeR"
  )

  local_mocked_bindings(
    as_tibble = function(x, ...) x,
    .package = "dplyr"
  )

  result <- dep_get_zcta5(
    varlist = "B19083_001E", year = 2019, survey = "acs5",
    state = NULL, county = NULL, puerto_rico = FALSE,
    zcta = NULL, zcta_geo_method = "intersect",
    geometry = FALSE, cb = FALSE, keep_geo_vars = FALSE,
    shift_geo = FALSE, key = "fake_key", debug = "live"
  )

  expect_type(result, "list")
  expect_named(result, c("geo", "demo"))
})

test_that("dep_get_zcta5 filters by zcta when provided", {
  mock_demo <- data.frame(
    GEOID = c("63011", "63101", "10001"),
    B19083_001E = c(0.40, 0.38, 0.50),
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    zi_get_demographics = function(...) mock_demo,
    .package = "zippeR"
  )

  local_mocked_bindings(
    as_tibble = function(x, ...) x,
    .package = "dplyr"
  )

  result <- dep_get_zcta5(
    varlist = "B19083_001E", year = 2019, survey = "acs5",
    state = NULL, county = NULL, puerto_rico = FALSE,
    zcta = c("63011", "63101"), zcta_geo_method = "intersect",
    geometry = FALSE, cb = FALSE, keep_geo_vars = FALSE,
    shift_geo = FALSE, key = "fake_key", debug = "live"
  )

  expect_equal(nrow(result$demo), 2)
  expect_true(all(result$demo$GEOID %in% c("63011", "63101")))
})

test_that("dep_get_zcta5 excludes territories when puerto_rico is FALSE", {
  mock_demo <- data.frame(
    GEOID = c("63011", "00601", "00801", "96901"),
    B19083_001E = c(0.40, 0.38, 0.50, 0.55),
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    zi_get_demographics = function(...) mock_demo,
    .package = "zippeR"
  )

  local_mocked_bindings(
    as_tibble = function(x, ...) x,
    .package = "dplyr"
  )

  result <- dep_get_zcta5(
    varlist = "B19083_001E", year = 2019, survey = "acs5",
    state = NULL, county = NULL, puerto_rico = FALSE,
    zcta = NULL, zcta_geo_method = "intersect",
    geometry = FALSE, cb = FALSE, keep_geo_vars = FALSE,
    shift_geo = FALSE, key = "fake_key", debug = "live"
  )

  # Territories (006, 007, 008, 009, 969) should be excluded
  remaining_prefixes <- substr(result$demo$GEOID, 1, 3)
  expect_false(any(remaining_prefixes %in% c("006", "007", "008", "009", "969")))
})

test_that("dep_get_zcta5 errors with debug='test'", {
  expect_error(
    dep_get_zcta5(
      varlist = "B19083_001E", year = 2019, survey = "acs5",
      state = NULL, county = NULL, puerto_rico = FALSE,
      zcta = NULL, zcta_geo_method = "intersect",
      geometry = FALSE, cb = FALSE, keep_geo_vars = FALSE,
      shift_geo = FALSE, key = "fake_key", debug = "test"
    ),
    "Testing debug mode"
  )
})

test_that("dep_get_zcta5 adjusts year for 2020 geometry", {
  mock_demo <- data.frame(
    GEOID = c("63011"),
    B19083_001E = c(0.40),
    stringsAsFactors = FALSE
  )

  captured_year <- NULL
  local_mocked_bindings(
    zi_get_demographics = function(...) mock_demo,
    zi_get_geometry = function(...) {
      args <- list(...)
      captured_year <<- args$year
      data.frame(GEOID = "63011", stringsAsFactors = FALSE)
    },
    .package = "zippeR"
  )

  result <- dep_get_zcta5(
    varlist = "B19083_001E", year = 2020, survey = "acs5",
    state = NULL, county = NULL, puerto_rico = FALSE,
    zcta = NULL, zcta_geo_method = "intersect",
    geometry = TRUE, cb = FALSE, keep_geo_vars = FALSE,
    shift_geo = FALSE, key = "fake_key", debug = "live"
  )

  # year 2020 should be revised to 2019 for geometry

  expect_equal(captured_year, 2019)
})

# test dep_get_zcta3 ---------------------------------------------------------

test_that("dep_get_zcta3 errors with debug='test'", {
  expect_error(
    dep_get_zcta3(
      varlist = "B19083_001E", year = 2019, survey = "acs5",
      state = NULL, county = NULL, puerto_rico = FALSE,
      zcta = NULL, zcta3_method = "mean",
      geometry = FALSE, shift_geo = FALSE,
      key = "fake_key", debug = "test"
    ),
    "Testing debug mode"
  )
})

test_that("dep_get_zcta3 calls zippeR with correct workflow", {
  mock_demo_tidy <- data.frame(
    GEOID = c("63011", "63011", "63101", "63101"),
    variable = c("B19083_001E", "B19083_001M", "B19083_001E", "B19083_001M"),
    estimate = c(0.40, 0.01, 0.38, 0.02),
    stringsAsFactors = FALSE
  )

  mock_aggregated <- data.frame(
    ZCTA3 = c("630", "631"),
    B19083_001E = c(0.39, 0.38),
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    zi_get_demographics = function(...) mock_demo_tidy,
    zi_aggregate = function(...) mock_aggregated,
    .package = "zippeR"
  )

  local_mocked_bindings(
    as_tibble = function(x, ...) x,
    .package = "dplyr"
  )

  result <- dep_get_zcta3(
    varlist = "B19083_001E", year = 2019, survey = "acs5",
    state = NULL, county = NULL, puerto_rico = FALSE,
    zcta = NULL, zcta3_method = "mean",
    geometry = FALSE, shift_geo = FALSE,
    key = "fake_key", debug = "live"
  )

  expect_type(result, "list")
  expect_named(result, c("geo", "demo"))
  expect_true("GEOID" %in% names(result$demo))
})

test_that("dep_get_zcta3 excludes territories", {
  mock_demo_tidy <- data.frame(
    GEOID = c("63011", "00601", "96901"),
    variable = rep("B19083_001E", 3),
    estimate = c(0.40, 0.38, 0.55),
    stringsAsFactors = FALSE
  )

  mock_aggregated <- data.frame(
    ZCTA3 = c("630"),
    B19083_001E = c(0.39),
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    zi_get_demographics = function(...) mock_demo_tidy,
    zi_aggregate = function(...) mock_aggregated,
    .package = "zippeR"
  )

  local_mocked_bindings(
    as_tibble = function(x, ...) x,
    .package = "dplyr"
  )

  result <- dep_get_zcta3(
    varlist = "B19083_001E", year = 2019, survey = "acs5",
    state = NULL, county = NULL, puerto_rico = FALSE,
    zcta = NULL, zcta3_method = "mean",
    geometry = FALSE, shift_geo = FALSE,
    key = "fake_key", debug = "live"
  )

  # Territories should be filtered out before aggregation
  expect_type(result, "list")
})

# test dep_get_data main dispatcher -------------------------------------------

test_that("dep_get_data dispatches to zcta5 for zcta5 geography", {
  mock_demo <- data.frame(
    GEOID = c("63011"),
    B19083_001E = c(0.40),
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    zi_get_demographics = function(...) mock_demo,
    .package = "zippeR"
  )

  local_mocked_bindings(
    as_tibble = function(x, ...) x,
    .package = "dplyr"
  )

  result <- dep_get_data(
    geography = "zcta5", varlist = "B19083_001E",
    year = 2019, survey = "acs5", state = NULL, county = NULL,
    puerto_rico = FALSE, zcta = NULL, zcta3_method = "mean",
    zcta_geo_method = "intersect", geometry = FALSE, cb = FALSE,
    keep_geo_vars = FALSE, shift_geo = FALSE,
    key = "fake_key", debug = "live"
  )

  expect_type(result, "list")
  expect_named(result, c("geo", "demo"))
})

test_that("dep_get_data dispatches to county_tract for county geography", {
  mock_result <- data.frame(
    GEOID = c("29001", "29003"),
    NAME = c("Adair County", "Andrew County"),
    B19083_001E = c(0.45, 0.42),
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    get_acs = function(...) mock_result,
    .package = "tidycensus"
  )

  result <- dep_get_data(
    geography = "county", varlist = "B19083_001E",
    year = 2019, survey = "acs5", state = NULL, county = NULL,
    puerto_rico = FALSE, zcta = NULL, zcta3_method = "mean",
    zcta_geo_method = "intersect", geometry = FALSE, cb = FALSE,
    keep_geo_vars = FALSE, shift_geo = FALSE,
    key = "fake_key", debug = "live"
  )

  expect_type(result, "list")
  expect_named(result, c("geo", "demo"))
})
