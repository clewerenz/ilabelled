test_that(
  "Test 'i_labelled", {

    # Test class
    expect_s3_class(i_labelled(iris$Species), "i_labelled")

    # Test is.i_labelled
    expect_true(is.i_labelled(i_labelled(iris$Species)))

    # Test data input
    expect_error(i_labelled(list(LETTERS, letters)))
    expect_error(i_labelled(iris))
    expect_error(i_labelled(list(factor(LETTERS), factor(letters))))
    expect_no_error(i_labelled(1:5))
    expect_no_error(i_labelled(LETTERS))
    expect_no_error(i_labelled(factor(LETTERS)))
    expect_no_error(i_labelled(i_labelled(LETTERS)))

    # Test output
    expect_vector(i_labelled(iris$Species))

    # Test class
    expect_equal(class(i_labelled(LETTERS)), c("i_labelled", "character"))
    expect_equal(class(i_labelled(1:5)), c("i_labelled", "integer"))
    expect_equal(class(i_labelled(as.double(seq(1.10,5)))), c("i_labelled", "numeric"))
    expect_equal(class(i_labelled(factor(LETTERS))), c("i_labelled", "factor"))
    expect_equal(class(i_labelled(sample(c(T,F),20,replace = T))), c("i_labelled", "logical"))
    expect_equal(class(i_labelled(c(Sys.Date(), Sys.Date()-1, Sys.Date()-2, Sys.Date()-3))), c("i_labelled", "Date"))

    # Test subset
    x <- i_labelled(LETTERS)
    y <- i_labelled(factor(LETTERS))
    z <- i_labelled(i_labelled(LETTERS))
    k <- i_labelled(i_labelled(seq(0,10,.5)))
    expect_equal(class(x[1:5]), c("i_labelled", "character"))
    expect_equal(class(y[1:5]), c("i_labelled", "factor"))
    expect_equal(class(z[1:5]), c("i_labelled", "character"))
    expect_equal(class(k[1:5]), c("i_labelled", "numeric"))

    # Test attributes
    x <- i_labelled(LETTERS)
    attr(x, "test1") <- "Test 1"
    attr(x, "test2") <- list("Test 2" = LETTERS)
    y <- iris
    y$Species <- i_labelled(y$Species)
    expect_contains(names(attributes(x)), c("test1", "test2", "class"))
    expect_contains(names(attributes(x[1:5])), c("test1", "test2", "class"))
    expect_contains(names(attributes(y$Species)), c("class", "levels"))
    expect_contains(names(attributes(y$Species[1:5])), c("class", "levels"))

  }
)




