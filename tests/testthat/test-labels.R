# test_that(".valid_labels", {
#
#   # valid input: named vector
#   expect_no_error(.valid_labels(x = c(A = 1, B = 2, C = 3)))
#
#   # return valus is NULL, wenn x is NULL
#   expect_equal(.valid_labels(x = NULL), NULL)
#
#   # Errors: invalid inputs
#   expect_error(.valid_labels(x = c(A = 1, B  = NA)), regexp = "labels cannot contain NA values")
#   expect_error(.valid_labels(x = list(A = 1, B = 2, C = 3)), regexp = "labels cannot be list or data.frame")
#   expect_error(.valid_labels(x = data.frame(A = 1, B = 2, C = 3)), regexp = "labels cannot be list or data.frame")
#   expect_error(.valid_labels(x = c(A = 1, B = 2, 3)), regexp = "all values in value labels should be labelled")
#   expect_error(.valid_labels(x = c(A = 1, B = 1, C = 3)), regexp = "duplicated values in value labels")
# })


test_that("i_labels: default behaviour", {

  # Apply labels
  # x <- i_labelled(x = c(1:3,NA))
  # expect_equal(attr(i_labels(x = x, list(A = 1:2, B = 3)), "labels"), c(A = 1, A = 2, B = 3))
  # expect_equal(attr(i_labels(x = x, setNames(1:3, c("A","B","C"))), "labels"), c(A = 1, B = 2, C = 3))
  # expect_equal(attr(i_labels(x = x, A = 1, B = 2, C = 3), "labels"), c(A = 1, B = 2, C = 3))
  # expect_equal(attr(i_labels(x = x, c(A = 1, B = 2, C = 3)), "labels"), c(A = 1, B = 2, C = 3))

  # Drop alle labels
  expect_null(attr(i_labels(i_labelled(iris$Species), NULL), "labels", exact = TRUE))
})


test_that(
  "Set value labels", {

    x <- i_labelled(iris$Species, labels = c(A = 1, B = 2, C = 3))

    # set labels with list or vector or setNames
    y <- i_labels(i_labelled(1:3), A = 1, NULL = 2, C = 3)
    expect_equal(names(attr(y, "labels")), c("A","C"))
    expect_equal(unname(attr(y, "labels")), c(1,3))
    expect_equal(attr(i_labels(y, setNames(1, "AA")), "labels", TRUE), setNames(c(1,3), c("AA", "C")))

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
    expect_equal(unname(attr(y, "labels")), c(1:4))
    y <- i_labels(x, "test" = 999)
    expect_equal(names(attr(y, "labels")), c("A","B","C","test"))
    expect_equal(unname(attr(y, "labels")), c(1:3,999))

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

    ## no duplicate values in value labels
    expect_error(i_labels(x, c(A = 1, B = 1)))


    # validate value labels
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
    expect_true(i_valid_labels(i_labelled(iris$Species, labels = c(Bla = 1, Bli = 2, Blubb = 3))))
    expect_true(i_valid_labels(i_labelled(iris$Species, labels = c(Bla = 1, Bli = 2, Blubb = 3, What = 4))))
    expect_false(i_valid_labels(i_labelled(iris$Species, labels = c(Bla = 1, Bli = NULL, Blubb = 3))))
    expect_false(i_valid_labels(i_labelled(iris$Species, labels = c(Bla = 1, NULL = 2, Blubb = 3))))

    # overwrite parameter - overwrite existing labels
    x <- i_labelled(iris$Species)
    x <- i_labels(x, test = 999, overwrite = F)
    expect_equal(unname(attr(x, "labels", T)), c(1,2,3,999))
    expect_equal(names(attr(x, "labels", T)), c("setosa","versicolor","virginica","test"))
    x <- i_labels(x, test = 999, overwrite = T)
    expect_equal(unname(attr(x, "labels", T)), c(999))
    expect_equal(names(attr(x, "labels", T)), "test")

    # sort value labels on vector
    x <- i_labelled(c(1,2,3,1,2,3,-9), labels = c(A = 1, B = 2, C = 3, X = -9))
    ## sort increasing by values (default)
    y <- i_sort_labels(x, by = "values", decreasing = F)
    expect_equal(unname(attr(y, "labels", T)), c(-9,1,2,3))
    expect_equal(names(attr(y, "labels", T)), c("X","A","B","C"))
    ## sort decreasing by values
    y <- i_sort_labels(x, by = "values", decreasing = T)
    expect_equal(unname(attr(y, "labels", T)), c(3,2,1,-9))
    expect_equal(names(attr(y, "labels", T)), c("C","B","A","X"))
    ## sort increasing by labels
    y <- i_sort_labels(x, by = "labels", decreasing = F)
    expect_equal(unname(attr(y, "labels", T)), c(1,2,3,-9))
    expect_equal(names(attr(y, "labels", T)), c("A","B","C","X"))
    ## sort decreasing by labels
    y <- i_sort_labels(x, by = "labels", decreasing = T)
    expect_equal(unname(attr(y, "labels", T)), c(-9,3,2,1))
    expect_equal(names(attr(y, "labels", T)), c("X","C","B","A"))

    # sort value labels on data.frame
    x <- data.frame(
      V1 = i_labelled(c(1,2,3,1,2,3,-9), labels = c(A = 1, B = 2, C = 3, X = -9)),
      V2 = i_labelled(c("A","B","C","A","B","C","X"), labels = c("Eins" = "A", "Zwei" = "B", "Drei" = "C", "Null" = "X"))
    )
    ## sort decreasing by values - all variables
    y <- i_sort_labels(x, by = "values", decreasing = T)
    expect_equal(unname(attr(y$V1, "labels", T)), c(3,2,1,-9))
    expect_equal(names(attr(y$V1, "labels", T)), c("C","B","A","X"))
    expect_equal(unname(attr(y$V2, "labels", T)), c("X","C","B","A"))
    expect_equal(names(attr(y$V2, "labels", T)), c("Null","Drei","Zwei","Eins"))
    ## sort decreasing by labels - all variables
    y <- i_sort_labels(x, by = "labels", decreasing = T)
    expect_equal(unname(attr(y$V1, "labels", T)), c(-9,3,2,1))
    expect_equal(names(attr(y$V1, "labels", T)), c("X","C","B","A"))
    expect_equal(unname(attr(y$V2, "labels", T)), c("B","X","A","C"))
    expect_equal(names(attr(y$V2, "labels", T)), c("Zwei","Null","Eins","Drei"))
    ## sort increasing by labels - all variables
    y <- i_sort_labels(x, by = "labels", decreasing = F)
    expect_equal(unname(attr(y$V1, "labels", T)), c(1,2,3,-9))
    expect_equal(names(attr(y$V1, "labels", T)), c("A","B","C","X"))
    expect_equal(unname(attr(y$V2, "labels", T)), c("C","A","X","B"))
    expect_equal(names(attr(y$V2, "labels", T)), c("Drei","Eins","Null","Zwei"))
  }
)


test_that("allow duplicated value labels", {
  ## allow duplicated value labels in class constructor
  expect_no_error(i_labelled(1:2, labels = c("A" = 1, "A" = 2)))

  ## allow duplicated value labels in as factor
  expect_no_error(i_as_factor(i_labelled(1:2, labels = c("A" = 1, "A" = 2))))
  expect_equal(i_as_factor(i_labelled(1:2, labels = c("A" = 1, "A" = 2))), factor(c(1,1), levels = 1, labels = "A"))

  ## allow duplicated value labels in as character
  expect_no_error(i_as_character(i_labelled(1:2, labels = c("A" = 1, "A" = 2))))
  expect_equal(i_as_character(i_labelled(1:2, labels = c("A" = 1, "A" = 2))), c("A", "A"))
})


test_that("set i_labels via named vector", {
  myData <- i_labelled(iris$Species)
  expect_equal(attr(i_labels(myData, "A" = 1, "B" = 2), "labels", exact = TRUE), c("A" = 1, "B" = 2, "virginica" = 3))
})

test_that("set i_labels via setNames", {
  myData <- i_labelled(iris$Species)
  expect_equal(attr(i_labels(myData, setNames(c(1,2), c("A", "B"))), "labels", exact = TRUE), c("A" = 1, "B" = 2, "virginica" = 3))
})

test_that("set i_labels via named list", {
  myData <- i_labelled(iris$Species)
  expect_equal(attr(i_labels(myData, list("A" = 1:2, "B" = 3)), "labels", exact = TRUE), c("A" = 1, "A" = 2, "B" = 3))
})
