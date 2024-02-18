test_that(
  "Handling of missing values", {

    # set NA values via class constructor
    x <- i_labelled(iris$Species, na_values = c(1))
    expect_true(!is.null(attr(x, "na_values", T)))
    expect_true(length(attr(x, "na_values", T)) == 1)
    expect_true(attr(x, "na_values", T) == 1)

    x <- i_labelled(iris$Species, na_values = c(1,3))
    expect_true(length(attr(x, "na_values", T)) == 2)
    expect_true(all(attr(x, "na_values", T) == c(1,3)))

    x <- i_labelled(iris$Species, na_values = c("bla","blubb"))
    expect_true(length(attr(x, "na_values", T)) == 2)
    expect_true(all(attr(x, "na_values", T) == c("bla","blubb")))
    x <- i_labelled(iris$Species, na_values = c("Z","W","A"))
    expect_true(length(attr(x, "na_values", T)) == 3)
    expect_true(all(attr(x, "na_values", T) == c("Z","W","A")))

    expect_error(i_labelled(1:5, na_values = c(NA,1)))


    # set NA values via method
    x <- i_labelled(iris$Species)
    x <- i_na_values(x, c(1))
    expect_true(!is.null(attr(x, "na_values", T)))
    expect_true(length(attr(x, "na_values", T)) == 1)
    expect_true(attr(x, "na_values", T) == 1)

    x <- i_labelled(iris$Species)
    x <- i_na_values(x, c(1,3))
    expect_true(length(attr(x, "na_values", T)) == 2)
    expect_true(all(attr(x, "na_values", T) == c(1,3)))
    x <- i_na_values(x, c("Z","W","A"))
    expect_true(length(attr(x, "na_values", T)) == 3)
    expect_true(all(attr(x, "na_values", T) == c("A","W","Z")))
    x <- i_na_values(x, c("Z","W","A"),sort = T, desc = T)
    expect_true(length(attr(x, "na_values", T)) == 3)
    expect_true(all(attr(x, "na_values", T) == c("Z","W","A")))
    x <- i_na_values(x, NULL)
    expect_true(is.null(attr(x, "na_values", T)))

    # set NA values to all variables in data.frame
    x <- i_labelled_df(iris)
    x <- i_na_values(x, c(1,2,3))
    res <- unlist(lapply(x, function(x) !is.null(attr(x, "na_values", T))))
    expect_true(all(res))
    res <- unlist(lapply(x, function(x) all(attr(x, "na_values", T) == c(1,2,3))))
    expect_true(all(res))

    x <- i_na_values(x, NULL)
    res <- unlist(lapply(x, function(x) is.null(attr(x, "na_values", T))))
    expect_true(all(res))

    # set NA range via class contructor
    x <- i_labelled(iris$Species, na_range = c(-9,-1))
    expect_true(length(attr(x, "na_range", T)) == 2)
    expect_true(all(attr(x, "na_range", T) == c(-9,-1)))
    x <- i_labelled(iris$Species, na_range = c(3,-2))
    expect_true(length(attr(x, "na_range", T)) == 2)
    expect_true(all(attr(x, "na_range", T) == c(3,-2)))

    expect_error(i_labelled(1:5, na_range = c(NA,1)))
    expect_error(i_labelled(iris$Species, na_range = c(-2,-1,0,1,2,3)))
    expect_error(i_labelled(iris$Species, na_range = c(-9:-4, -2:-1)))
    expect_error(i_labelled(iris$Species, na_range = c(1,2,3,5)))
    expect_error(i_labelled(iris$Species, na_range = LETTERS[1:3]))
    expect_error(i_labelled_df(iris$Species, na_range = LETTERS[1:3]))

    # set NA range via method
    x <- i_labelled(iris$Species)
    expect_no_error(i_na_range(x, c(3,-2)))
    expect_error(i_na_range(x, 1,4,2,3))
    expect_error(i_na_range(LETTERS[1:3]))
    expect_error(i_na_range(c(1,2,4)))
    expect_error(i_na_range(c(NA,2,4)))

    # remove missing values
    x <- i_labelled(c(1,2,3,-9,1,2,3,-8), na_range = -9:-8, labels = c(X = -9, Y = -8, A = 1, B = 2, C = 3))
    expect_true(is.na(i_missing_to_na(x)[[4]]))
    expect_true(is.na(i_missing_to_na(x)[[8]]))

    x <- i_labelled(c(1,2,3,888,1,2,3,999), na_values = c(888,999), labels = c(X = 888, Y = 999, A = 1, B = 2, C = 3))
    expect_true(is.na(i_missing_to_na(x)[[4]]))
    expect_true(is.na(i_missing_to_na(x)[[8]]))

    x <- i_labelled(c(LETTERS[1:3],"X",LETTERS[1:3], "Y"), na_values = c("X","Y"))
    expect_true(is.na(i_missing_to_na(x)[[4]]))
    expect_true(is.na(i_missing_to_na(x)[[8]]))

    x <- factor(c(1,2,3), labels = c("A","B","C"))
    attr(x, "na_values") <- "A"
    expect_true(is.na(i_missing_to_na(x)[[1]]))

    x <- data.frame(V1 = c(286,127,478,-9), V2 = c(-8,1,2,3), V3 = c("A","B","-9","D"))
    x <- i_na_values(x, values = c(-9,-8))
    x <- i_missing_to_na(x)
    expect_true(is.na(x$V1[4]))
    expect_true(is.na(x$V2[1]))
    expect_true(is.na(x$V3[3]))

    # missings to NA - method

    ## no missings declared: do nothing
    x <- i_labelled(c(1,2,3,-9), labels = c(X = -9))
    expect_no_error(i_missing_to_na(x))
    y <- i_missing_to_na(x)
    expect_equal(y, x)

    ## numeric vectors

    x <- i_labelled(c(-9,1,2,3,1,2,3), na_values = -9, labels = c(setosa = 1, versicolor = 2, missing = -9, virginica = 3))

    ### keep missing labels
    y <- i_missing_to_na(x, remove_missing_labels = F)
    expect_true(is.na(y[1]))
    expect_false(is.na(y[2]))
    expect_true(y[2] == 1)
    expect_true(y[3] == 2)
    expect_equal(names(attr(y, "labels", T)), c("setosa","versicolor","missing","virginica"))
    ### remove missing labels
    y <- i_missing_to_na(x, remove_missing_labels = T)
    expect_equal(names(attr(y, "labels", T)), c("setosa","versicolor","virginica"))
    expect_equal(unname(attr(y, "labels", T)), c(1,2,3))

    ## character vector
    x <- i_labelled(c("A","B","C","X"), na_values = "X", labels = c("Eins" = "A", "Zwei" = "B", "Drei" = "C", "Null" = "X"))
    ### keep missing labels
    y <- i_missing_to_na(x, remove_missing_labels = F)
    expect_true(is.na(y[4]))
    expect_equal(unname(attr(y, "labels", T)), c("A","B","C","X"))
    ### remove missing labels
    y <- i_missing_to_na(x, remove_missing_labels = T)
    expect_equal(unname(attr(y, "labels", T)), c("A","B","C"))
    ### missing label not in vector
    x <- i_labelled(c("A","B","C","X"), na_values = "X", labels = c("Eins" = "A", "Zwei" = "B", "Drei" = "C", "Null" = "ZZZ"))
    y <- i_missing_to_na(x, remove_missing_labels = T)
    expect_true(is.na(y[4]))
    expect_equal(unname(attr(y, "labels", T)), c("A","B","C","ZZZ"))
    expect_equal(names(attr(y, "labels", T)), c("Eins","Zwei","Drei","Null"))

    # remove missing labels - method

    ## numeric vector
    x <- i_labelled(c(-9,1,2,3,1,2,3), na_values = -9, labels = c(A = 1, B = 2, X = -9, C = 3))
    y <- i_remove_missing_labels(x)
    expect_true(!is.na(y[1])) # remove only label not value
    expect_equal(names(attr(y, "labels", T)), c("A","B","C"))
    expect_equal(unname(attr(y, "labels", T)), c(1,2,3))
    ## character vector
    x <- i_labelled(c("X","A","B","C","A","B","C"), na_values = "X", labels = c("Eins" = "A", "Zwei" = "B", "Null" = "X", "Drei" = "C"))
    y <- i_remove_missing_labels(x)
    expect_true(!is.na(y[1])) # remove only label not value
    expect_equal(names(attr(y, "labels", T)), c("Eins","Zwei","Drei"))
    expect_equal(unname(attr(y, "labels", T)), c("A","B","C"))

  }
)
