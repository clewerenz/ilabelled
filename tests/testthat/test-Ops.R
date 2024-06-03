x <- i_labelled(c(1:3,NA,5,9), labels = c("A" = 1, "B" = 2, "C" = 3, "D" =5, "E" = 9), na_values = c(3,9))

test_that("i_labelled with labels is alway label not numeric value and behave like character values", {
  expect_true(all(x != 2, na.rm = TRUE))
  expect_true(!any(x == 2, na.rm = TRUE))
  expect_true(!any(x < 2, na.rm = TRUE))
  expect_true(all(x >= 2, na.rm = TRUE))
})


test_that("i_labelled are matched with their value labels", {
  expect_true(identical(x == "A", c(TRUE, FALSE, FALSE, NA, FALSE, FALSE)))
  expect_true(identical(x != "B", c(TRUE, FALSE, TRUE, NA, TRUE, TRUE)))
})
