test_that("i_annotation: default, no error", {
  x <- i_labelled(1:3)
  expect_no_error(i_annotation(x, c("my annotation 1", "my annotation 2")))
})


test_that("i_annotation: annotation is added correctly", {
  x <- i_labelled(1:3)
  x <- i_annotation(x, c("my annotation 1", "my annotation 2"))
  expect_equal(attr(x, "annotation"), c("my annotation 1", "my annotation 2"))
})


test_that("i_annotation: add annotation to existing annotation; overwrite = FALSE", {
  x <- i_labelled(1:3)
  x <- i_annotation(x, c("my annotation 1"), overwrite = FALSE)
  x <- i_annotation(x, c("my annotation 2"), overwrite = FALSE)
  expect_equal(attr(x, "annotation"), c("my annotation 1", "my annotation 2"))
})


test_that("i_annotation: overwrite existing annotation; overwrite = FALSE", {
  x <- i_labelled(1:3)
  x <- i_annotation(x, c("my annotation 1"), overwrite = FALSE)
  x <- i_annotation(x, c("my annotation 2"), overwrite = TRUE)
  expect_equal(attr(x, "annotation"), c("my annotation 2"))
})


test_that("i_annotation: throw error when invalid new annotation value", {
  x <- i_labelled(1:3)
  expect_error(i_annotation(x, c(F)))
  expect_error(i_annotation(x, c(1)))
})


test_that("i_annotation: throw error when invalid old annotation value", {
  x <- i_labelled(1:3)
  attr(x, "annotation") <- 1
  expect_error(i_annotation(x, c("my annotation")))
})


test_that("i_annotation: remove annotation when NULL value", {
  x <- i_labelled(1:3)
  x <- i_annotation(x, c("my annotation 1"))
  x <- i_annotation(x, NULL)
  expect_null(attr(x, "annotation"))
})


test_that("i_annotation: valid annotation on vector", {
  x <- i_labelled(1:3)

  expect_false(i_valid_annotation(x))

  y <- i_annotation(x, c("my annotation 1"))
  expect_true(i_valid_annotation(y))

  y <- x
  attr(y, "annotation") <- 1
  expect_false(i_valid_annotation(y))
})


test_that("i_annotation: valid annotation on data.frame", {
  x <- data.frame(
    V1 = i_labelled(c(1:4,-9), labels = c("A" = 1, "B" = 2, "C" = 3, "D" = 4), na_values = -9),
    V2 = factor(c(LETTERS[1:4], "X")),
    V3 = c(LETTERS[1:4], "X")
  )
  x$V1 <- i_annotation(x$V1, "my annotation 1")
  r <- list(V1 = TRUE, V2 = FALSE, V3 = FALSE)
  expect_equal(i_valid_annotation(x), r)
})
