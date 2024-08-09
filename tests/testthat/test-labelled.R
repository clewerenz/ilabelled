test_that("initialize i_labelled object", {

  expect_no_error(i_labelled(c(1:3,-9)))
  expect_s3_class(i_labelled(c(1:3,-9)), class = c("i_labelled", "double"))

  expect_no_error(i_labelled(1:5))
  expect_no_error(i_labelled(LETTERS))
  expect_no_error(i_labelled(factor(LETTERS)))
  expect_no_error(i_labelled(i_labelled(LETTERS)))

  expect_vector(i_labelled(iris$Species))

  # apply class to vector which already has attr labels
  x <- c(1,2,3,1,2,3)
  attr(x, "labels") <- c(A=1,B=2,C=3)
  y <- i_labelled(x)
  expect_equal(unname(attr(y, "labels", T)), 1:3)
  expect_equal(names(attr(y, "labels", T)), c("A","B","C"))
  y <- i_labelled(x, labels = c(Bla = 3, Blubb = 4))
  expect_equal(unname(attr(y, "labels", T)), c(1,2,3,4))
  expect_equal(names(attr(y, "labels", T)), c("A","B","Bla","Blubb"))
  y <- i_labelled(x, labels = c(NULL = 3, Blubb = 4))
  expect_equal(unname(attr(y, "labels", T)), c(1:2,4))
  expect_equal(names(attr(y, "labels", T)), c("A","B","Blubb"))
})


test_that("initialize class - errors", {
  expect_error(i_labelled(list(LETTERS, letters)))
  expect_error(i_labelled(list(factor(LETTERS), factor(letters))))
  ## value labels
  ### no duplicate values in value labels
  expect_error(i_labelled(1:3, labels = c(A = 1, B = 1)))
})


test_that("class attributes are correct", {
  expect_equal(class(i_labelled(LETTERS)), c("i_labelled", "character"))
  expect_equal(class(i_labelled(1:5)), c("i_labelled", "double"))
  expect_equal(class(i_labelled(as.double(seq(1.10,5)))), c("i_labelled", "double"))
  expect_equal(class(i_labelled(factor(LETTERS))), c("i_labelled", "double"))
  expect_equal(class(i_labelled(sample(c(T,F),20,replace = T))), c("i_labelled", "double"))
  expect_equal(class(i_labelled(c(Sys.Date(), Sys.Date()-1, Sys.Date()-2, Sys.Date()-3))), c("i_labelled", "character"))
})


test_that("initialize class - attributes are available", {
  x <- i_labelled(LETTERS)
  attr(x, "test1") <- "Test 1"
  attr(x, "test2") <- list("Test 2" = LETTERS)

  y <- iris
  y$Species <- i_labelled(y$Species)
  expect_contains(names(attributes(x)), c("test1", "test2", "class"))
  expect_contains(names(attributes(x[1:5])), c("test1", "test2", "class"))
  expect_contains(names(attributes(y$Species)), c("class", "labels"))
  expect_contains(names(attributes(y$Species[1:5])), c("class", "labels"))

  x <- i_labelled(1:5, test_attr = "Test attribute")
  expect_contains(names(attributes(x)), "test_attr")

  x <- i_labelled(c(1,2,3,1,2,3), labels = c(A = 1, NULL = 2, C = 3))
  expect_equal(names(attr(x, "labels", T)), c("A", "C"))
  expect_equal(unname(attr(x, "labels", T)), c(1, 3))
})


test_that("Error handling: value labels must be in correct format numeric/character", {
  expect_error(i_labelled(1:3, labels = c("A" = "1", "B" = "2", "C" = "3")))
  expect_error(i_labelled(c("1","2","3"), labels = c("A" = 1, "B" = 2, "C" = 3)))
})


test_that("i_labelled - make all vars in data.frame i_labelled", {
  expect_true(all(unlist(lapply(i_labelled(iris), is.i_labelled))))
})


test_that("function 'is.i_labelled", {
  expect_true(is.i_labelled(i_labelled(c(1:3,-9))))
})


test_that("classic subsetting", {
  x <- i_labelled(LETTERS)
  y <- i_labelled(factor(LETTERS))
  z <- i_labelled(i_labelled(LETTERS))
  k <- i_labelled(i_labelled(seq(0,10,.5)))
  expect_equal(class(x[1:5]), c("i_labelled", "character"))
  expect_equal(class(y[1:5]), c("i_labelled", "double"))
  expect_equal(class(z[1:5]), c("i_labelled", "character"))
  expect_equal(class(k[1:5]), c("i_labelled", "double"))

  x <- subset(i_labelled(iris), subset = Species %in% 1 & Sepal.Length < 5, select = c(Species, Sepal.Length))
  expect_true(all(unlist(lapply(x, is.i_labelled))))
  y <- x[1:5,1]
  expect_true(is.i_labelled(y))
  expect_equal(unname(attr(y, "labels", T)), 1:3)
  expect_equal(names(attr(y, "labels", T)), c("setosa","versicolor","virginica"))
})

