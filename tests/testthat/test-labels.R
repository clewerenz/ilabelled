test_that(
  "Set value labels", {

    x <- i_labelled(iris$Species, labels = c(A = 1, B = 2, C = 3))

    # return labels correctly when no arguments given
    expect_equal(names(attr(i_labels(x), "labels")), c("A","B","C"))
    expect_equal(unname(attr(i_labels(x), "labels")), c(1,2,3))

    # set labels with list or vector
    y <- i_labels(i_labelled(1:3), A = 1, NULL = 2, C = 3)
    expect_equal(names(attr(i_labels(y), "labels")), c("A","C"))
    expect_equal(unname(attr(i_labels(y), "labels")), c(1,3))

    # remove all labels
    expect_null(attr(i_labels(x, NULL), "labels"))
    # remove one label
    y <- i_labels(x, NULL = 1)
    expect_equal(names(attr(y, "labels")), c("B","C"))
    expect_equal(unname(attr(y, "labels")), c(2,3))
    # remove two labels
    y <- i_labels(x, NULL = 1, NULL = 2)
    expect_equal(names(attr(y, "labels")), c("C"))
    expect_equal(unname(attr(y, "labels")), 3)
    # remove all labels but individually
    y <- i_labels(x, NULL = 1, NULL = 2, NULL = 3)
    expect_null(attr(y, "labels"))

    # replace one value
    y <- i_labels(x, "test" = 1)
    expect_equal(names(attr(y, "labels")), c("test","B","C"))
    y <- i_labels(x, "test" = 2)
    expect_equal(names(attr(y, "labels")), c("A","test","C"))
    y <- i_labels(x, "test" = 3)
    expect_equal(names(attr(y, "labels")), c("A","B","test"))

    # add values
    y <- i_labels(x, "test" = 4)
    expect_equal(names(attr(y, "labels")), c("A","B","C","test"))
    expect_equal(unname(attr(y, "labels")), 1:4)
    y <- i_labels(x, "test" = 999)
    expect_equal(names(attr(y, "labels")), c("A","B","C","test"))
    expect_equal(unname(attr(y, "labels")), c(1:3,999))

    # sort values
    y <- i_labels(x, "test" = 999, sort_desc = T)
    expect_equal(names(attr(y, "labels")), c("test","C","B","A"))
    expect_equal(unname(attr(y, "labels")), c(999,3:1))

    # add value, replace value, remove, and sort value labels at the same time
    y <- i_labels(x, add_val = 4, NULL = 1, repl_val = 2, sort_desc = T)
    expect_equal(names(attr(y, "labels")), c("add_val","C","repl_val"))
    expect_equal(unname(attr(y, "labels")), c(4:2))

    # errors
    expect_error(i_labels(x, 1234))
    expect_error(i_labels(x, 1:5))
    expect_error(i_labels(x, LETTERS[1:5]))
    expect_error(i_labels(x, list(LETTERS[1:5])))
    expect_error(i_labels(x, list(LETTERS, letters)))
    expect_error(i_labels(x, list("A" = LETTERS, letters)))
    expect_error(i_labels(x, list("A" = LETTERS[1:5], "B" = 1:5)))
    expect_error(i_labels(x, list("A" = LETTERS[1:5], "B" = 1:5)))
    expect_error(i_labels(x, A = 1, B = 2, C))
    expect_error(i_labels(x, A = 1, B = 2, C = "bla"))
    expect_error(i_labels(i_labelled(1:3), list(A = 1, NULL = 2, C = 3)))

    # valudate value labels
    x <- i_labelled(iris$Species)
    expect_true(i_valid_labels(i_labelled(iris$Species)))
    attr(x, "labels") <- NA
    expect_false(i_valid_labels(x))
    attr(x, "labels") <- 1:5
    expect_false(i_valid_labels(x))
    attr(x, "labels") <- LETTERS[1:5]
    expect_false(i_valid_labels(x))
    attr(x, "labels") <- NULL
    expect_false(i_valid_labels(x))
    attr(x, "labels") <- character(0)
    expect_false(i_valid_labels(x))
    attr(x, "labels") <- TRUE
    expect_false(i_valid_labels(x))
    attr(x, "labels") <- FALSE
    expect_false(i_valid_labels(x))
    expect_true(i_valid_labels(i_labelled(iris$Species, labels = c(Bla = "1", Bli = 2, Blubb = "3"))))
    expect_true(i_valid_labels(i_labelled(iris$Species, labels = c(Bla = "1", Bli = 2, Blubb = "3", What = 4))))
    expect_false(i_valid_labels(i_labelled(iris$Species, labels = c(Bla = "1", Bli = NULL, Blubb = "3"))))
    expect_false(i_valid_labels(i_labelled(iris$Species, labels = c(Bla = "1", NULL = 2, Blubb = "3"))))
    expect_false(i_valid_labels(i_labelled(iris$Species, labels = c(Bla = "1", "AAA" = "BBB", Blubb = "3"))))


  }
)
