x <- i_labelled(c(1:3,NA,5,9), labels = c("A" = 1, "B" = 2, "C" = 3, "D" =5, "E" = 9), na_values = c(3,9))

test_that("i_labelled are compared by value labels, when character is compare value", {
  expect_true(identical(x == "A", c(TRUE, FALSE, FALSE, NA, FALSE, FALSE)))
  expect_true(identical(x != "B", c(TRUE, FALSE, TRUE, NA, TRUE, TRUE)))
  expect_true(identical(x < "E", c(TRUE, TRUE, TRUE, NA, TRUE, FALSE)))
})

test_that("i_labelled are compared by values, when numeric or logical compare value", {
  expect_true(identical(x == 1, c(TRUE, FALSE, FALSE, NA, FALSE, FALSE)))
  expect_true(identical(x != 2, c(TRUE, FALSE, TRUE, NA, TRUE, TRUE)))
  expect_true(identical(x < 2, c(TRUE, FALSE, FALSE, NA, FALSE, FALSE)))
  expect_true(identical(x == TRUE, c(TRUE, FALSE, FALSE, NA, FALSE, FALSE)))
})
