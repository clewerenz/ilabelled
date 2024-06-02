set.seed(1234)

a <- sample(c("X","Y","Z", NA), 10, replace = T)
b <- sample(c(1:3, NA), 10, replace = T)
x <- i_labelled(sample(c(1:3,NA), 10, replace = T), labels = c("A" = 1, "B" = 2, "C" = 3))
y <- factor(sample(c("X","Y","Z", NA), 10, replace = T))
z <- i_labelled(sample(c(1,2), 10, replace = T), labels = c("male" = 1, "female" = 2))
df <- data.frame(a,b,x,y,z)

test_that("i_table does not throw error", {
  expect_no_error(i_table(a))
  expect_no_error(i_table(b))
  expect_no_error(i_table(x))
  expect_no_error(i_table(a,b,x))
  expect_no_error(i_table(x, y, z))
  expect_no_error(i_table(df[c(1,2,3)]))
})


test_that("return value is 'table'", {
  expect_true(is.table(i_table(x)))
  expect_true(is.table(i_table(df[c(1,2,3)])))
})


test_that("error handling 'table_args'", {
  # no arguments in list
  expect_error(i_table(x, table_args = list()))
  # missing argument
  expect_error(i_table(x, table_args = list(exclude)))
  # missing argument name
  expect_error(i_table(x, table_args = list("ifany", exclude = "B")))
  # argument not from base::table
  expect_error(i_table(x, table_args = list(bla = 1)))
  # either data.frame or atomic vectors
  expect_error(i_table(df[c(1,2,3)], a))
  # either data.frame or atomic vectors
  expect_error(i_table(df[c(1,2,3)], NA))
})


test_that("arguments are passed to base::table", {
  expect_true("A" %in% names(i_table(x, table_args = list(useNA = "ifany", exclude = "B"))))
  expect_false("B" %in% names(i_table(x, table_args = list(useNA = "ifany", exclude = "B"))))
  expect_true(NA %in% names(i_table(x, table_args = list(useNA = "ifany", exclude = "B"))))
})
