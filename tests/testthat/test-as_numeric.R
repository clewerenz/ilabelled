test_that("i_as_numeric: default, no error", {
  x <- i_labelled(c(1,2,3,NA), labels = c("AA" = 1, "BB" = 2, "CC" = 3), na_values = 3)
  y <- c(1:3,NA)
  z <- c(T,F,NA)
  w <- factor(c(1:3,NA), levels = 1:3, labels = c("A","B","C"))
  expect_no_error(i_as_numeric(x))
  expect_no_error(i_as_numeric(y))
  expect_no_error(i_as_numeric(z))
  expect_no_error(i_as_numeric(w))
})


test_that("i_as_numeric: verify return values", {
  x <- i_labelled(c(1,2,3,NA), labels = c("AA" = 1, "BB" = 2, "CC" = 3), na_values = 3)
  y <- c(1:3,NA)
  z <- c(T,F,NA)
  w <- factor(c(1:3,NA), levels = 1:3, labels = c("A","B","C"))
  expect_equal(i_as_numeric(x), c(1:3,NA))
  expect_equal(i_as_numeric(y), c(1:3,NA))
  expect_equal(i_as_numeric(z), c(1,0,NA))
  expect_equal(i_as_numeric(w), c(1:3,NA))
})


test_that("i_as_numeric: missing_to_na param", {
  x <- i_labelled(c(1,2,3,NA), labels = c("AA" = 1, "BB" = 2, "CC" = 3), na_values = 3)
  expect_equal(i_as_numeric(x, missing_to_na = TRUE), c(1,2,NA,NA))
})


test_that("i_as_numeric: keep_attributes param", {
  x <- i_labelled(c(1,2,3,NA), labels = c("AA" = 1, "BB" = 2, "CC" = 3), na_values = 3)
  y <- structure(.Data = c(1,2,3,NA), labels = c("AA" = 1, "BB" = 2, "CC" = 3), na_values = 3)
  expect_equal(i_as_numeric(x, keep_attributes = TRUE), y)
})


