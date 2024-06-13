
test_that("i_as_character: i_labelelled to character", {
  x <- i_labelled(c(1:3,NA), labels = c("A" = 1, "B" = 2, "C" = 3), na_values = 3)
  expect_no_error(i_as_character(x))
  expect_equal(i_as_character(x), c("A", "B", "C", NA))
})


test_that("i_as_character: base class to character", {
  # character to character
  expect_equal(i_as_character(c("A","B",NA)), c("A","B",NA))
  # numeric to character
  expect_equal(i_as_character(c(1.5,2.5,NA)), c("1.5","2.5",NA))
  # logical to character
  expect_equal(i_as_character(c(TRUE,FALSE,NA)), c("TRUE","FALSE",NA))
  # date to character
  expect_equal(i_as_character(Sys.Date()), as.character(Sys.Date()))
})


test_that("missing values become NA", {
  x <- i_labelled(c(1:3,NA), labels = c("A" = 1, "B" = 2, "C" = 3), na_values = 3)
  expect_equal(i_as_character(x, missing_to_na = T), c("A", "B", NA, NA))
})


test_that("Error when require_all_labels and label is missing", {
  x <- i_labelled(c(1:3,NA), labels = c("A" = 1, "B" = 2), na_values = 3)
  expect_error(i_as_character(x, require_all_labels = T))
})



