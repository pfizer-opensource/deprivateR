
# test errors ------------------------------------------------

test_that("incorrectly specified parameters trigger appropriate errors", {
  expect_error(dep_build_varlist(index = "svi20", year = 2020, survey = "acs5", output = "vector"),
               "geography.*must be provided")
  expect_error(dep_build_varlist(geography = "ham", index = "svi20", year = 2020, survey = "acs5", output = "vector"),
               "Invalid.*geography")
  expect_error(dep_build_varlist(geography = "tract", year = 2020, survey = "acs5", output = "vector"),
               "index.*must be provided")
  expect_error(dep_build_varlist(geography = "tract", index = "ham", year = 2020, survey = "acs5", output = "vector"),
               "Invalid.*index")
  expect_error(dep_build_varlist(geography = "tract", index = "svi20", year = 2000, survey = "acs5", output = "vector"),
               "Invalid.*year")
  expect_error(dep_build_varlist(geography = "tract", index = "svi20", year = "ham", survey = "acs5", output = "vector"),
               "Invalid.*year")
  expect_error(dep_build_varlist(geography = "tract", index = "svi20", year = 2020, survey = "acs3", output = "vector"),
               "acs3.*after.*2013")
  expect_error(dep_build_varlist(geography = "tract", index = "svi20", year = 2020, survey = "acs3", output = "vector"),
               "acs3.*after.*2013")
  expect_error(dep_build_varlist(geography = "tract", index = "svi20", year = 2020, survey = "ham", output = "vector"),
               "Invalid.*survey")
  expect_error(dep_build_varlist(geography = "tract", index = "svi20", year = 2020, survey = "acs5", output = "ham"),
               "Invalid.*output")
})

# test adi output ------------------------------------------------

test_that("adi creation executes correctly", {
  out_adi <- expect_error(
    dep_build_varlist(geography = "county", index = "adi", year = 2019, survey = "acs5"), NA
  )

  expect_equal(class(out_adi), "character")
  expect_equal(length(out_adi), 60)
})

# test gini output ------------------------------------------------

test_that("gini creation executes correctly", {
  out_gini <- expect_error(
    dep_build_varlist(geography = "county", index = "gini", year = 2019, survey = "acs5"), NA
  )

  expect_equal(class(out_gini), "character")
  expect_equal(length(out_gini), 1)

  skip_if_not(nzchar(Sys.getenv("CENSUS_API_KEY")),
              message = "Census API key not available")
  out_gini_tbl <- expect_error(
    dep_build_varlist(geography = "county", index = "gini", year = 2019,
                      survey = "acs5", output = "tibble"), NA
  )

  expect_equal(class(out_gini_tbl), c("tbl_df", "tbl", "data.frame"))
  expect_true(all(c("name", "label", "concept") %in% names(out_gini_tbl)))
  expect_equal(nrow(out_gini_tbl), 1)
})

# test ndi output ------------------------------------------------

test_that("ndi_m creation executes correctly", {
  out_ndi_m <- expect_error(
    dep_build_varlist(geography = "county", index = "ndi_m", year = 2019, survey = "acs5"), NA
  )

  expect_equal(class(out_ndi_m), "character")
  expect_equal(length(out_ndi_m), 19)

  skip_if_not(nzchar(Sys.getenv("CENSUS_API_KEY")),
              message = "Census API key not available")
  out_ndi_m_tbl <- expect_error(
    dep_build_varlist(geography = "county", index = "ndi_m", year = 2019,
                      survey = "acs5", output = "tibble"), NA
  )

  expect_equal(class(out_ndi_m_tbl), c("tbl_df", "tbl", "data.frame"))
  expect_true(all(c("name", "label", "concept") %in% names(out_ndi_m_tbl)))
  expect_equal(nrow(out_ndi_m_tbl), 19)
})

test_that("ndi_pw creation executes correctly", {
  out_ndi_m <- expect_error(
    dep_build_varlist(geography = "county", index = "ndi_pw", year = 2019, survey = "acs5"), NA
  )

  expect_equal(class(out_ndi_m), "character")
  expect_equal(length(out_ndi_m), 19)

  skip_if_not(nzchar(Sys.getenv("CENSUS_API_KEY")),
              message = "Census API key not available")
  out_ndi_m_tbl <- expect_error(
    dep_build_varlist(geography = "county", index = "ndi_pw", year = 2019,
                      survey = "acs5", output = "tibble"), NA
  )

  expect_equal(class(out_ndi_m_tbl), c("tbl_df", "tbl", "data.frame"))
  expect_true(all(c("name", "label", "concept") %in% names(out_ndi_m_tbl)))
  expect_equal(nrow(out_ndi_m_tbl), 19)
})

# test svi output ------------------------------------------------

test_that("svi creation executes correctly", {
  out_svi10 <- expect_error(
    dep_build_varlist(geography = "county", index = "svi10", year = 2019, survey = "acs5"), NA
  )

  expect_equal(class(out_svi10), "character")
  expect_equal(length(out_svi10), 53)

  out_svi14 <- expect_error(
    dep_build_varlist(geography = "county", index = "svi14", year = 2019, survey = "acs5"), NA
  )

  expect_equal(class(out_svi14), "character")
  expect_equal(length(out_svi14), 66)

  out_svi20 <- expect_error(
    dep_build_varlist(geography = "county", index = "svi20", year = 2019, survey = "acs5"), NA
  )

  expect_equal(class(out_svi20), "character")
  expect_equal(length(out_svi20), 71)

  out_svi20s <- expect_error(
    dep_build_varlist(geography = "county", index = "svi20s", year = 2019, survey = "acs5"), NA
  )

  expect_equal(class(out_svi20s), "character")
  expect_equal(length(out_svi20s), 69)
})
