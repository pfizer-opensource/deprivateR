test_that(".dep_str_word extracts the correct word", {
  expect_equal(.dep_str_word("svi10, hhd", 1), "svi10,")
  expect_equal(.dep_str_word("svi10, hhd", 2), "hhd")
  expect_equal(.dep_str_word("one two three", 3), "three")
})

test_that(".dep_str_word returns NA for edge cases", {
  expect_equal(.dep_str_word(NA, 1), NA_character_)
  expect_equal(.dep_str_word("", 1), "")
  expect_equal(.dep_str_word("one", 5), NA_character_)
  expect_equal(.dep_str_word("word", 0), NA_character_)
})

test_that(".dep_str_word handles multiple spaces", {
  expect_equal(.dep_str_word("  svi20,   eng  ", 1), "svi20,")
  expect_equal(.dep_str_word("  svi20,   eng  ", 2), "eng")
})
