
test_that("i_labelelled becomes character", {
  x <- i_labelled(c(1:3,NA), labels = c("A" = 1, "B" = 2, "C" = 3), na_values = 3)
  expect_no_error(i_as_character(x))
  expect_equal(i_as_character(x), c("A", "B", "C", NA))
})


test_that("missing values become NA", {
  x <- i_labelled(c(1:3,NA), labels = c("A" = 1, "B" = 2, "C" = 3), na_values = 3)
  expect_equal(i_as_character(x, missing_to_na = T), c("A", "B", NA, NA))
})


test_that("Error when require_all_labels and label is missing", {
  x <- i_labelled(c(1:3,NA), labels = c("A" = 1, "B" = 2), na_values = 3)
  expect_error(i_as_character(x, require_all_labels = T))
})
