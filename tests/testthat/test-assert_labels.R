x <- data.frame(
  V1 = i_labelled(1:3, labels = c("A" = 1, "B" = 2, "C" = 3)),
  V2 = i_labelled(1:3, labels = c("A" = 1, "B" = 2, "X" = 3)),
  V3 = factor(1:3, c("A", "B", "C")),
  V4 = factor(1:3, c("A", "B", "X")),
  V5 = LETTERS[1:3],
  V6 = 1:3
)

## success handling

test_that("assert labels with i_labelled - single vector - no error", {
  expect_no_error(i_assert_labels(x$V1, labels = c("A", "B", "C"), info = "MyVars recoding"))
})

test_that("assert labels factor - single vector - no error", {
  expect_no_error(i_assert_labels(x$V3, labels = c("A", "B", "C"), info = "MyVars recoding"))
})

test_that("assert labels factor and i_labelled - data.frame - no error", {
  expect_no_error(i_assert_labels(x[c("V1", "V3")], labels = c("A", "B", "C"), info = "MyVars recoding"))
})


## error handling

test_that("assert labels i_labelled - single vector - error", {
  expect_error(i_assert_labels(x$V2, labels = c("A", "B", "C"), info = "MyVars recoding"))
})

test_that("assert labels factor - single vector - error", {
  expect_error(i_assert_labels(x$V4, labels = c("A", "B", "C"), info = "MyVars recoding"))
})

test_that("assert labels wrong class character - single vector - error", {
  expect_error(i_assert_labels(x$V5, labels = c("A", "B", "C"), info = "MyVars recoding"))
})

test_that("assert labels wrong class numeric - single vector - error", {
  expect_error(i_assert_labels(x$V6, labels = c("A", "B", "C"), info = "MyVars recoding"))
})

test_that("assert labels missing labels - data.frame - error", {
  expect_error(suppressMessages(i_assert_labels(x[c("V1", "V2", "V4")], labels = c("A", "B", "C"), info = "MyVars recoding")))
})

test_that("assert labels missing labels and wrong class - data.frame - error", {
  expect_error(suppressMessages(i_assert_labels(x[c("V1", "V2", "V5")], labels = c("A", "B", "C"), info = "MyVars recoding")))
})




