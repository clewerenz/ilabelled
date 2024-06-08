test_that("i_subject: default, no error", {
  x <- i_labelled(1:3)
  expect_no_error(i_subject(x, c("my subject 1")))
})


test_that("i_subject: subject is added correctly", {
  x <- i_labelled(1:3)
  x <- i_subject(x, c("my subject 1"))
  expect_equal(attr(x, "subject"), c("my subject 1"))
})


test_that("i_subject: throw error when invalid new subject value", {
  x <- i_labelled(1:3)
  expect_error(i_subject(x, c("subject 1", "subject 2")))
  expect_error(i_subject(x, c(1)))
  expect_error(i_subject(x, c(1)))
})


test_that("i_subject: remove subject when NULL value", {
  x <- i_labelled(1:3)
  x <- i_subject(x, c("my subject 1"))
  x <- i_subject(x, NULL)
  expect_null(attr(x, "subject"))
})


test_that("i_subject: valid subject on vector", {
  x <- i_labelled(1:3)

  expect_false(i_valid_subject(x))

  y <- i_subject(x, c("my subject 1"))
  expect_true(i_valid_subject(y))

  y <- x
  attr(y, "subject") <- 1
  expect_false(i_valid_subject(y))
})


test_that("i_subject: valid subject on data.frame", {
  x <- data.frame(
    V1 = i_labelled(c(1:4,-9), labels = c("A" = 1, "B" = 2, "C" = 3, "D" = 4), na_values = -9),
    V2 = factor(c(LETTERS[1:4], "X")),
    V3 = c(LETTERS[1:4], "X")
  )
  x$V1 <- i_subject(x$V1, "my subject 1")
  r <- list(V1 = TRUE, V2 = FALSE, V3 = FALSE)
  expect_equal(i_valid_subject(x), r)
})
