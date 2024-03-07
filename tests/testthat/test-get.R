test_that("get value labels from vector", {
  x <- i_labelled(c(1:3,NA), labels = c("A" = 1, "B" = 2, "C" = 3))

  expect_no_error(i_get_labels(x))
  expect_true(is.data.frame(i_get_labels(x)))
  expect_identical(i_get_labels(x)[["value"]], c(1,2,3))
  expect_identical(i_get_labels(x)[["label"]], c("A","B","C"))
})

test_that("get value labels from data.frame", {
  x <- data.frame(
    V1 = i_labelled(c(1:3,NA), labels = c("A" = 1, "B" = 2, "C" = 3)),
    V2 = i_labelled(c("A","B","C",NA))
  )

  expect_no_error(i_get_labels(x))
  expect_true(is.list(i_get_labels(x)))
  expect_identical(i_get_labels(x)[[1]][["value"]], c(1,2,3))
  expect_identical(i_get_labels(x)[[1]][["label"]], c("A","B","C"))
  expect_true(is.na(i_get_labels(x)[[2]]))
})

test_that("get variable label from vector", {
  x <- i_labelled(c(1:3,NA), labels = c("A" = 1, "B" = 2, "C" = 3), label = "myVar")

  expect_no_error(i_get_label(x))
  expect_identical(i_get_label(x), "myVar")
})

test_that("get variable labels from data.frame", {
  x <- data.frame(
    V1 = i_labelled(c(1:3,NA), labels = c("A" = 1, "B" = 2, "C" = 3), label = "var1"),
    V2 = i_labelled(c("A","B","C",NA))
  )

  expect_no_error(i_get_label(x))
  expect_true(is.list(i_get_label(x)))
  expect_identical(i_get_label(x)[[1]], "var1")
  expect_true(is.na(i_get_label(x)[[2]]))
})

test_that("get missing values from vector", {
  x <- i_labelled(c(1:3,NA), labels = c("A" = 1, "B" = 2, "C" = 3), label = "myVar", na_values = 1)

  expect_no_error(i_get_na_values(x))
  expect_equal(i_get_na_values(x), 1)
})

test_that("get missing values from data.frame", {
  x <- data.frame(
    V1 = i_labelled(c(1:3,NA), labels = c("A" = 1, "B" = 2, "C" = 3), label = "var1", na_values = 1),
    V2 = i_labelled(c("A","B","C",NA))
  )

  expect_no_error(i_get_na_values(x))
  expect_true(is.list(i_get_na_values(x)))
  expect_equal(i_get_na_values(x)[[1]], 1)
  expect_true(is.na(i_get_na_values(x)[[2]]))
})

test_that("get missing range from vector", {
  x <- i_labelled(c(1:3,NA), labels = c("A" = 1, "B" = 2, "C" = 3), label = "myVar", na_range = 1:2)

  expect_no_error(i_get_na_range(x))
  expect_equal(i_get_na_range(x), c(1,2))
})

test_that("get missing range from data.frame", {
  x <- data.frame(
    V1 = i_labelled(c(1:3,NA), labels = c("A" = 1, "B" = 2, "C" = 3), label = "var1", na_range = 1:2),
    V2 = i_labelled(c("A","B","C",NA))
  )

  expect_no_error(i_get_na_range(x))
  expect_true(is.list(i_get_na_range(x)))
  expect_equal(i_get_na_range(x)[[1]], c(1,2))
  expect_true(is.na(i_get_na_range(x)[[2]]))
})

test_that("get scale level from vector", {
  x <- i_labelled(c(1:3,NA), labels = c("A" = 1, "B" = 2, "C" = 3), label = "myVar", na_range = 1:2, scale = "Nominal")

  expect_no_error(i_get_scale(x))
  expect_equal(i_get_scale(x), "nominal")
})

test_that("get scale level from data.frame", {
  x <- data.frame(
    V1 = i_labelled(c(1:3,NA), labels = c("A" = 1, "B" = 2, "C" = 3), label = "var1", na_range = 1:2, scale = "Nominal"),
    V2 = i_labelled(c("A","B","C",NA))
  )

  expect_no_error(i_get_scale(x))
  expect_true(is.list(i_get_scale(x)))
  expect_equal(i_get_scale(x)[[1]], "nominal")
  expect_true(is.na(i_get_scale(x)[[2]]))
})
