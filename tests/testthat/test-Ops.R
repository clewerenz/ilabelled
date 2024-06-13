x <- i_labelled(c(1:3,NA,5,9), labels = c("A" = 1, "B" = 2, "C" = 3, "D" =5, "E" = 9), na_values = c(3,9))

test_that("Ops: i_labelled are compared by value labels, when character is compare value", {
  expect_true(identical(x == "A", c(TRUE, FALSE, FALSE, NA, FALSE, FALSE)))
  expect_true(identical(x != "B", c(TRUE, FALSE, TRUE, NA, TRUE, TRUE)))
  expect_true(identical(x < "E", c(TRUE, TRUE, TRUE, NA, TRUE, FALSE)))
})

test_that("Ops: i_labelled are compared by values, when numeric or logical compare value", {
  expect_true(identical(x == 1, c(TRUE, FALSE, FALSE, NA, FALSE, FALSE)))
  expect_true(identical(x != 2, c(TRUE, FALSE, TRUE, NA, TRUE, TRUE)))
  expect_true(identical(x < 2, c(TRUE, FALSE, FALSE, NA, FALSE, FALSE)))
  expect_true(identical(x == TRUE, c(TRUE, FALSE, FALSE, NA, FALSE, FALSE)))
})

test_that("Ops: compare i_labelled character to numeric", {
  xx <- i_labelled(c("A", "1"))
  expect_equal(xx == 1, c(FALSE, TRUE))
})

test_that("Ops: compare i_labelled character to character", {
  xx <- i_labelled(c("A", "1"))
  expect_equal(xx == "1", c(FALSE, TRUE))
})

test_that("Ops: compare i_labelled character to i_labelled character", {
  ww <- i_labelled(c("A", "1"), labels = c("A" = "1"))
  xx <- i_labelled(c("A", "1"))
  yy <- i_labelled(c("A"))
  zz <- i_labelled(c("1"), labels = c("A" = "1"))
  expect_equal(xx == yy, c(TRUE, FALSE))
  expect_equal(xx == zz, c(TRUE, FALSE))
  expect_equal(ww == zz, c(TRUE, TRUE))
})
