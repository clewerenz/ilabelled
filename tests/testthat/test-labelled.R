test_that(
  "Test i_labelled", {

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

    x <- i_labelled(c(1,2,3,1,2,3), labels = c(A = 1, NULL = 2, C = 3))
    expect_equal(names(attr(x, "labels", T)), c("A", "C"))
    expect_equal(unname(attr(x, "labels", T)), c(1, 3))

    # make all vars in data.frame i_labelled
    expect_true(all(unlist(lapply(i_labelled_df(iris), is.i_labelled))))

    # Test output
    expect_vector(i_labelled(iris$Species))

    # Test class
    expect_equal(class(i_labelled(LETTERS)), c("i_labelled"))
    expect_equal(class(i_labelled(1:5)), c("i_labelled"))
    expect_equal(class(i_labelled(as.double(seq(1.10,5)))), c("i_labelled"))
    expect_equal(class(i_labelled(factor(LETTERS))), c("i_labelled"))
    expect_equal(class(i_labelled(sample(c(T,F),20,replace = T))), c("i_labelled"))
    expect_equal(class(i_labelled(c(Sys.Date(), Sys.Date()-1, Sys.Date()-2, Sys.Date()-3))), c("i_labelled"))

    # Test subset
    x <- i_labelled(LETTERS)
    y <- i_labelled(factor(LETTERS))
    z <- i_labelled(i_labelled(LETTERS))
    k <- i_labelled(i_labelled(seq(0,10,.5)))
    expect_equal(class(x[1:5]), c("i_labelled"))
    expect_equal(class(y[1:5]), c("i_labelled"))
    expect_equal(class(z[1:5]), c("i_labelled"))
    expect_equal(class(k[1:5]), c("i_labelled"))

    # Test attributes
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

    # test recode values
    x <- i_labelled(1:5, label = "Test label", labels = setNames(1:5, LETTERS[1:5]))
    x[x %in% 2] <- 999
    expect_equal(x[[2]], 999)
    expect_equal(attr(x, "label", T), "Test label")
    expect_equal(unname(attr(x, "labels", T)), 1:5)
    expect_equal(names(attr(x, "labels", T)), LETTERS[1:5])

    # test subsetting
    x <- subset(i_labelled_df(iris), subset = Species %in% 1 & Sepal.Length < 5, select = c(Species, Sepal.Length))
    expect_true(all(unlist(lapply(x, is.i_labelled))))
    y <- x[1:5,1]
    expect_true(is.i_labelled(y))
    expect_equal(unname(attr(y, "labels", T)), 1:3)
    expect_equal(names(attr(y, "labels", T)), c("setosa","versicolor","virginica"))

    # apply class to vector which already has attr labels
    x <- c(1,2,3,1,2,3)
    attr(x, "labels") <- c(A=1,B=2,C=3)
    y <- i_labelled(x)
    expect_equal(unname(attr(y, "labels", T)), 1:3)
    expect_equal(names(attr(y, "labels", T)), c("A","B","C"))
    y <- i_labelled(x, labels = c(Bla = 3, Blubb = 4))
    expect_equal(unname(attr(y, "labels", T)), 1:4)
    expect_equal(names(attr(y, "labels", T)), c("A","B","Bla","Blubb"))
    y <- i_labelled(x, labels = c(NULL = 3, Blubb = 4))
    expect_equal(unname(attr(y, "labels", T)), c(1:2,4))
    expect_equal(names(attr(y, "labels", T)), c("A","B","Blubb"))

  }
)

