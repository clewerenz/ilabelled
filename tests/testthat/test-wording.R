test_that("i_wording: default, no error", {
  x <- i_labelled(1:3)
  expect_no_error(i_wording(x, c("my wording 1")))
})


test_that("i_wording: wording is added correctly", {
  x <- i_labelled(1:3)
  x <- i_wording(x, c("my wording 1"))
  expect_equal(attr(x, "wording"), c("my wording 1"))
})


test_that("i_wording: throw error when invalid new wording value", {
  x <- i_labelled(1:3)
  expect_error(i_wording(x, c("wording 1", "wording 2")))
  expect_error(i_wording(x, c(1)))
  expect_error(i_wording(x, c(1)))
})


test_that("i_wording: remove wording when NULL value", {
  x <- i_labelled(1:3)
  x <- i_wording(x, c("my wording 1"))
  x <- i_wording(x, NULL)
  expect_null(attr(x, "wording"))
})


test_that("i_wording: valid wording on vector", {
  x <- i_labelled(1:3)

  expect_false(i_valid_wording(x))

  y <- i_wording(x, c("my wording 1"))
  expect_true(i_valid_wording(y))

  y <- x
  attr(y, "wording") <- 1
  expect_false(i_valid_wording(y))
})


test_that("i_wording: valid wording on data.frame", {
  x <- data.frame(
    V1 = i_labelled(c(1:4,-9), labels = c("A" = 1, "B" = 2, "C" = 3, "D" = 4), na_values = -9),
    V2 = factor(c(LETTERS[1:4], "X")),
    V3 = c(LETTERS[1:4], "X")
  )
  x$V1 <- i_wording(x$V1, "my wording 1")
  r <- list(V1 = TRUE, V2 = FALSE, V3 = FALSE)
  expect_equal(i_valid_wording(x), r)
})
