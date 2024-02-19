
x <- data.frame(
  V1 = i_labelled(c(1:4,-9), labels = c("A" = 1, "B" = 2, "C" = 3, "D" = 4), na_values = -9),
  V2 = factor(c(LETTERS[1:4], "X")),
  V3 = c(LETTERS[1:4], "X"),
  V4 = c(1:4,-9)
)

## success handling

### atomic vector

test_that("i_recode - recode i_labelled var - no error", {
  expect_no_error(i_recode(x$V1, "AB" = 1 ~ x %in% c("A", "B"), "CD" = 2 ~ x %in% c("C", "D")))
})
test_that("i_recode - recode i_labelled var - no error", {
  expect_no_error(i_recode(x$V1, "AB" = 1 ~ x %in% c("A", "B"), "CD" = 2 ~ x %in% c("C", "D")))
})
test_that("i_recode - recode factor var - no error", {
  expect_no_error(i_recode(x$V2, "AB" = 1 ~ x %in% c("A", "B"), "CD" = 2 ~ x %in% c("C", "D")))
})
test_that("i_recode - recode character var - no error", {
  expect_no_error(i_recode(x$V3, "AB" = 1 ~ x %in% c("A", "B"), "CD" = 2 ~ x %in% c("C", "D")))
})
test_that("i_recode - recode numeric var - no error", {
  expect_no_error(i_recode(x$V4, "AB" = 1 ~ x %in% c(1, 2), "CD" = 2 ~ x %in% c(3, 4)))
})

test_that("i_recode - apply variable label - no error", {
  expect_no_error(
    res <- i_recode(x$V1, "AB" = 1 ~ x %in% c("A", "B"), "CD" = 2 ~ x %in% c("C", "D"), label = "myRecoding")
  )
  expect_equal(attr(res, "label"), "myRecoding")
})
test_that("i_recode - apply na_values - no error", {
  expect_no_error(
    res <- i_recode(x$V4, "AB" = 1 ~ x %in% c(1, 2), "CD" = 2 ~ x %in% c(3, 4), "X" = -9 ~ x == -9, na_values = -9)
  )
  expect_equal(attr(res, "na_values"), -9)
})

test_that("recode labels and negative values at the same time - no error", {
  myVar <- i_labelled(c(1,2,-9,-8,-7), labels = c("A" = 1, "B" = 2, "X" = -9, "Y" = -8), na_range = -9:-8, label = "myVar")
  expect_no_error(
    x <- i_recode(
      myVar,
      "AB" = 1 ~ x %in% c("A", "B"),
      "C2" = 2 ~ x %in% "C",
      "D2" = 9 ~ x %in% "D",
      "X" = -9 ~ x %in% c(-9,-8),
      "Y" = 888 ~ x %in% c(-7),
      na_values = -9
    )
  )
  expect_equal(as.numeric(x), c(1,1,-9,-9,888))
})

### data.frame

test_that("recode on data.frame", {
  expect_no_error(i_recode(x = x, "AB" = 1 ~ V1 %in% c("A", "B"), "CD" = 2 ~ V1 %in% c("C", "D")))
})

## error handling

test_that("... must be formula", {
  expect_error(i_recode(iris, "AB" = c(1, 2), "CD" = 2 ~ c(3, 4)))
})


