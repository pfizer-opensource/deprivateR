context("test dep_sample_data function")

# test errors ------------------------------------------------

test_that("incorrectly specified parameters trigger appropriate errors", {
  expect_error(dep_sample_data(),
               "A 'index' value must be provided. Please choose one of: 'adi', 'ndi_m', 'ndi_pw', 'svi10', 'svi14', 'svi20', or 'svi20s'.")
  expect_error(dep_sample_data(index = "ham"),
               "Invalid index provided. Please choose one of: 'adi', 'ndi_m', 'ndi_pw', 'svi10', 'svi14', 'svi20', or 'svi20s'.")
})

# test adi output ------------------------------------------------

test_that("adi creation executes correctly", {
  out_adi <- expect_error(
    dep_sample_data(index = "adi"), NA
  )

  expect_equal(class(out_adi), c("tbl_df", "tbl", "data.frame"))
  expect_equal(length(names(out_adi)), 281)
})
