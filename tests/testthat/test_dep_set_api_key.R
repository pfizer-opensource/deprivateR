test_that("dep_set_api_key uses explicit key argument", {
  # Mock tidycensus::census_api_key to capture the call
  local_mocked_bindings(
    census_api_key = function(key, install = FALSE, overwrite = FALSE) {
      invisible(key)
    },
    .package = "tidycensus"
  )

  result <- dep_set_api_key(key = "abcdef1234567890abcdef1234567890abcdef12")
  expect_equal(result, "abcdef1234567890abcdef1234567890abcdef12")
})

test_that("dep_set_api_key falls back to CENSUS_API_KEY env var", {
  local_mocked_bindings(
    census_api_key = function(key, install = FALSE, overwrite = FALSE) {
      invisible(key)
    },
    .package = "tidycensus"
  )

  withr::local_envvar(CENSUS_API_KEY = "abcdef1234567890abcdef1234567890abcdef12")

  expect_message(
    result <- dep_set_api_key(),
    "Using Census API key found in"
  )
  expect_equal(result, "abcdef1234567890abcdef1234567890abcdef12")
})

test_that("dep_set_api_key errors in non-interactive mode with no key", {
  local_mocked_bindings(
    census_api_key = function(key, install = FALSE, overwrite = FALSE) {
      invisible(key)
    },
    .package = "tidycensus"
  )

  withr::local_envvar(CENSUS_API_KEY = "")

  # Ensure non-interactive context
  local_mocked_bindings(
    interactive = function() FALSE,
    .package = "base"
  )

  expect_error(
    dep_set_api_key(),
    "No Census API key found"
  )
})

test_that("dep_set_api_key passes install and overwrite to tidycensus", {
  captured_args <- list()
  local_mocked_bindings(
    census_api_key = function(key, install = FALSE, overwrite = FALSE) {
      captured_args$install <<- install
      captured_args$overwrite <<- overwrite
      invisible(key)
    },
    .package = "tidycensus"
  )

  dep_set_api_key(
    key = "abcdef1234567890abcdef1234567890abcdef12",
    install = TRUE,
    overwrite = TRUE
  )

  expect_true(captured_args$install)
  expect_true(captured_args$overwrite)
})

test_that("dep_set_api_key warns on invalid key format", {
  local_mocked_bindings(
    census_api_key = function(key, install = FALSE, overwrite = FALSE) {
      invisible(key)
    },
    .package = "tidycensus"
  )

  expect_warning(
    dep_set_api_key(key = "not-a-valid-key"),
    "does not match the expected Census API key format"
  )
})
