test_that("eval_dots_arg: default behaviour", {
  func <- function(...) .eval_dots_arg(...)

  expect_equal(func(list(A = c(1,2), B = c("DREI"))), list(A = c(1,2), B = c("DREI")))
  expect_equal(func(NULL), NULL)
  expect_equal(func(list(A = 1, B = 2, C = 3)), list(A = 1, B = 2, C = 3))
  expect_equal(func(A = 1, B = 2, C = 3), c(A = 1, B = 2, C = 3))
  expect_equal(func(c(A = 1, B = 2, C = 3)), c(A = 1, B = 2, C = 3))
  expect_equal(func(setNames(1:3, c("A","B","C"))), c(A = 1, B = 2, C = 3))
})


test_that("eval_dots_arg: param flatten = TRUE", {
  func <- function(...) .eval_dots_arg(..., flatten = TRUE)

  expect_equal(func(list(A = c(1,2), B = c("DREI"))), c(A = "1", A = "2", B = "DREI"))
  expect_equal(func(NULL), NULL)
  expect_equal(func(list(A = 1, B = 2, C = 3)), c(A = 1, B = 2, C = 3))
  expect_equal(func(A = 1, B = 2, C = 3), c(A = 1, B = 2, C = 3))
  expect_equal(func(c(A = 1, B = 2, C = 3)), c(A = 1, B = 2, C = 3))
  expect_equal(func(setNames(1:3, c("A","B","C"))), c(A = 1, B = 2, C = 3))
})
