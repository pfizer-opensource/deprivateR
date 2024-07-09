context("test dep_build_varlist function")

# test errors ------------------------------------------------

test_that("incorrectly specified parameters trigger appropriate errors", {
  expect_error(dep_build_varlist(index = "svi20", year = 2020, survey = "acs5", output = "vector"),
               "A level of geography must be provided. Please choose one of: 'county', 'zcta3', 'zcta5', or 'tract'.")
  expect_error(dep_build_varlist(geography = "ham", index = "svi20", year = 2020, survey = "acs5", output = "vector"),
               "Invalid level of geography provided. Please choose one of: 'county', 'zcta3', 'zcta5', or 'tract'.")
  expect_error(dep_build_varlist(geography = "tract", year = 2020, survey = "acs5", output = "vector"),
               "A 'index' value must be provided. Please choose one of: 'adi', 'gini', 'ndi_m', 'ndi_pw', 'svi10', 'svi14', 'svi20', or 'svi20s'.")
  expect_error(dep_build_varlist(geography = "tract", index = "ham", year = 2020, survey = "acs5", output = "vector"),
               "Invalid index provided. Please choose one of: 'adi', 'gini', 'ndi_m', 'ndi_pw', 'svi10', 'svi14', 'svi20', or 'svi20s'.")
  expect_error(dep_build_varlist(geography = "tract", index = "svi20", year = 2000, survey = "acs5", output = "vector"),
               "The 'year' value provided is invalid. Please provide a numeric value between 2010 and 2022.")
  expect_error(dep_build_varlist(geography = "tract", index = "svi20", year = "ham", survey = "acs5", output = "vector"),
               "The 'year' value provided is invalid. Please provide a numeric value between 2010 and 2022.")
  expect_error(dep_build_varlist(geography = "tract", index = "svi20", year = 2020, survey = "acs3", output = "vector"),
               "The 'acs3' survey was discontinued after 2013. Please select one of 'acs1' or 'acs5'.")
  expect_error(dep_build_varlist(geography = "tract", index = "svi20", year = 2020, survey = "acs3", output = "vector"),
               "The 'acs3' survey was discontinued after 2013. Please select one of 'acs1' or 'acs5'.")
  expect_error(dep_build_varlist(geography = "tract", index = "svi20", year = 2020, survey = "ham", output = "vector"),
               "The 'survey' value provided is not valid. Please choose one of 'acs1', 'acs3', or 'acs5'.")
  expect_error(dep_build_varlist(geography = "tract", index = "svi20", year = 2020, survey = "acs5", output = "ham"),
               "The 'output' value provided is not valid. Please choose one of 'vector' or 'tibble'.")
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

  skip_if_offline()
  out_gini_tbl <- expect_error(
    dep_build_varlist(geography = "county", index = "gini", year = 2019,
                      survey = "acs5", output = "tibble"), NA
  )

  expect_equal(class(out_gini_tbl), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(out_gini_tbl), c("name", "label", "concept"))
  expect_equal(nrow(out_gini_tbl), 1)
})

# test ndi output ------------------------------------------------

test_that("ndi_m creation executes correctly", {
  out_ndi_m <- expect_error(
    dep_build_varlist(geography = "county", index = "ndi_m", year = 2019, survey = "acs5"), NA
  )

  expect_equal(class(out_ndi_m), "character")
  expect_equal(length(out_ndi_m), 19)

  skip_if_offline()
  out_ndi_m_tbl <- expect_error(
    dep_build_varlist(geography = "county", index = "ndi_m", year = 2019,
                      survey = "acs5", output = "tibble"), NA
  )

  expect_equal(class(out_ndi_m_tbl), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(out_ndi_m_tbl), c("name", "label", "concept"))
  expect_equal(nrow(out_ndi_m_tbl), 19)
})

test_that("ndi_pw creation executes correctly", {
  out_ndi_m <- expect_error(
    dep_build_varlist(geography = "county", index = "ndi_pw", year = 2019, survey = "acs5"), NA
  )

  expect_equal(class(out_ndi_m), "character")
  expect_equal(length(out_ndi_m), 19)

  skip_if_offline()
  out_ndi_m_tbl <- expect_error(
    dep_build_varlist(geography = "county", index = "ndi_pw", year = 2019,
                      survey = "acs5", output = "tibble"), NA
  )

  expect_equal(class(out_ndi_m_tbl), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(out_ndi_m_tbl), c("name", "label", "concept"))
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
