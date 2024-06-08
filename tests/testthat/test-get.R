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

test_that("get annotation from vector", {
  x <- i_labelled(1:3)
  x <- i_annotation(x, "my annotation")
  expect_equal(i_get_annotation(x), "my annotation")
})

test_that("get annotation from data.frame", {
  x <- data.frame(
    V1 = i_labelled(1:3),
    V2 = i_labelled(1:3)
  )
  x$V1 <- i_annotation(x$V1, "my annotation")
  r <- list(V1 = "my annotation", V2 = NA)
  expect_equal(i_get_annotation(x), r)
})


# ----------------------------------------------------------------------------

# i_get_equal_subject()

dataWithSubjects <- data.frame(
  V1 = i_labelled(1:3, subject = "my subject 1"),
  V2 = i_labelled(1:3, subject = "my subject 1"),
  V3 = i_labelled(1:3, subject = "my subject 2"),
  V4 = i_labelled(1:3)
)

dataWithoutSubjects <- data.frame(
  V1 = i_labelled(1:3),
  V2 = i_labelled(1:3),
  V3 = i_labelled(1:3),
  V4 = i_labelled(1:3)
)

test_that("i_get_equal_subject - return list with variables for each subject in data. param subject = NULL", {
  ret <- list("my subject 1" = c("V1", "V2"), "my subject 2" = c("V3"))
  expect_equal(i_get_equal_subject(dataWithSubjects), ret)
})

test_that("i_get_equal_subject - return NA for data with no subjects. param subject = NULL", {
  expect_equal(i_get_equal_subject(dataWithoutSubjects), NA)
})

test_that("i_get_equal_subject - return list with variables for each subject in data. specified subjects", {
  ret1 <- list("my subject 1" = c("V1", "V2"))
  ret2 <- list("my subject 1" = c("V1", "V2"), "my subject 2" = c("V3"))
  expect_equal(i_get_equal_subject(dataWithSubjects, subject = c("my subject 1")), ret1)
  expect_equal(i_get_equal_subject(dataWithSubjects, subject = c("my subject 1", "my subject 2")), ret2)
  expect_equal(
    suppressWarnings(i_get_equal_subject(dataWithSubjects, subject = c("my subject 1", "my subject 2", "my subject 3"))),
    ret2
  )
  expect_equal(
    suppressWarnings(i_get_equal_subject(dataWithSubjects, subject = c("my subject 3"))),
    NA
  )
})

test_that("i_get_equal_subject - throw warning when subject is not in data", {
  expect_warning(i_get_equal_subject(dataWithSubjects, subject = c("my subject 3")))
  expect_warning(i_get_equal_subject(dataWithSubjects, subject = c("my subject 1", "my subject 3")))
})

test_that("i_get_equal_subject - throw error when x is not data.frame", {
  expect_error(i_get_equal_subject(1:3))
})

test_that("i_get_equal_subject - throw error when subject is not character vector", {
  expect_error(i_get_equal_subject(dataWithSubjects, subject = 1:3))
  expect_error(i_get_equal_subject(dataWithSubjects, subject = list("A" = 1)))
  expect_error(i_get_equal_subject(dataWithSubjects, subject = data.frame("A" = 1)))
  expect_error(i_get_equal_subject(dataWithSubjects, subject = c(TRUE, FALSE)))
})




# ----------------------------------------------------------------------------

# i_get_equal_wording()

dataWithwordings <- data.frame(
  V1 = i_labelled(1:3, wording = "my wording 1"),
  V2 = i_labelled(1:3, wording = "my wording 1"),
  V3 = i_labelled(1:3, wording = "my wording 2"),
  V4 = i_labelled(1:3)
)

dataWithoutwordings <- data.frame(
  V1 = i_labelled(1:3),
  V2 = i_labelled(1:3),
  V3 = i_labelled(1:3),
  V4 = i_labelled(1:3)
)

test_that("i_get_equal_wording - return list with variables for each wording in data. param wording = NULL", {
  ret <- list("my wording 1" = c("V1", "V2"), "my wording 2" = c("V3"))
  expect_equal(i_get_equal_wording(dataWithwordings), ret)
})

test_that("i_get_equal_wording - return NA for data with no wordings. param wording = NULL", {
  expect_equal(i_get_equal_wording(dataWithoutwordings), NA)
})

test_that("i_get_equal_wording - return list with variables for each wording in data. specified wordings", {
  ret1 <- list("my wording 1" = c("V1", "V2"))
  ret2 <- list("my wording 1" = c("V1", "V2"), "my wording 2" = c("V3"))
  expect_equal(i_get_equal_wording(dataWithwordings, wording = c("my wording 1")), ret1)
  expect_equal(i_get_equal_wording(dataWithwordings, wording = c("my wording 1", "my wording 2")), ret2)
  expect_equal(
    suppressWarnings(i_get_equal_wording(dataWithwordings, wording = c("my wording 1", "my wording 2", "my wording 3"))),
    ret2
  )
  expect_equal(
    suppressWarnings(i_get_equal_wording(dataWithwordings, wording = c("my wording 3"))),
    NA
  )
})

test_that("i_get_equal_wording - throw warning when wording is not in data", {
  expect_warning(i_get_equal_wording(dataWithwordings, wording = c("my wording 3")))
  expect_warning(i_get_equal_wording(dataWithwordings, wording = c("my wording 1", "my wording 3")))
})

test_that("i_get_equal_wording - throw error when x is not data.frame", {
  expect_error(i_get_equal_wording(1:3))
})

test_that("i_get_equal_wording - throw error when wording is not character vector", {
  expect_error(i_get_equal_wording(dataWithwordings, wording = 1:3))
  expect_error(i_get_equal_wording(dataWithwordings, wording = list("A" = 1)))
  expect_error(i_get_equal_wording(dataWithwordings, wording = data.frame("A" = 1)))
  expect_error(i_get_equal_wording(dataWithwordings, wording = c(TRUE, FALSE)))
})
