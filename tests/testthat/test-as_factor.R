test_that(
  "labelled data to factor", {

    # Default behavior

    x <- i_as_factor(i_labelled(1:3))
    expect_equal(class(x), "factor")
    expect_equal(levels(x), c("1","2","3"))

    x <- i_as_factor(i_labelled(c("A", "B", "C")))
    expect_equal(class(x), "factor")
    expect_equal(levels(x), c("A","B","C"))
    expect_equal(as.numeric(x), 1:3)

    x <- c(1:3,-9)
    attr(x, "labels") <- c(A = 1, B = 2, C = 3, X = -9)
    x <- i_as_factor(x)
    expect_equal(class(x), "factor")
    expect_equal(levels(x), c("X", "A", "B", "C"))

    ## factor to i_labelled and back to factor: will levels stay equal
    old_lvl <- levels(iris$Species)
    new_lvl <- levels(i_as_factor(i_labelled(iris$Species)))
    expect_equal(old_lvl, new_lvl)

    ## i_labelled with all values being labelled
    x <- i_labelled(1:3, labels = c(A = 1, B = 2, C = 3))
    x <- i_as_factor(x)
    expect_equal(levels(x), c("A", "B", "C"))

    x <- i_labelled(c("A", "B", "C"), labels = c("Eins" = "A", "Zwei" = "B", "Drei" = "C"))
    x <- i_as_factor(x)
    expect_equal(levels(x), c("Eins", "Zwei", "Drei"))

    ## not all values being labelled - missing label will be generated from value
    x <- i_labelled(1:3, labels = c(A = 1, B = 2))
    x <- i_as_factor(x)
    expect_equal(levels(x), c("A", "B", "3"))
    x <- i_labelled(1:3, labels = c(A = 1, C = 3))
    x <- i_as_factor(x)
    expect_equal(levels(x), c("A", "2", "C"))
    x <- i_labelled(c("A", "B", "C"), labels = c("Eins" = "A", "Drei" = "C"))
    x <- i_as_factor(x)
    expect_equal(levels(x), c("Eins", "B", "Drei"))

    ##################################################

    # Testing parameters

    ## 'label' for setting labels manually

    x <- i_labelled(1:3)
    x <- i_as_factor(x, labels = c(A = 1, B = 2, C = 3))
    expect_equal(levels(x), c("A", "B", "C"))

    x <- i_labelled(c("A","B","C"))
    x <- i_as_factor(x, labels = c("Eins" = "A", "Zwei" = "B", "Drei" = "C"))
    expect_equal(levels(x), c("Eins", "Zwei", "Drei"))

    x <- i_labelled(c("A","B","C"))
    x <- i_as_factor(x, labels = c("Eins" = "A", "Drei" = "C"))
    expect_equal(levels(x), c("Eins", "B", "Drei"))

    ## 'require_all_labels' - break when labels are missing

    x <- i_labelled(1:3)
    expect_error(i_as_factor(require_all_labels = T))

    x <- i_labelled(LETTERS[1:3])
    expect_error(i_as_factor(require_all_labels = T))

    x <- i_labelled(1:3, labels = c(A = 1, B = 2))
    expect_error(i_as_factor(require_all_labels = T))

    x <- i_labelled(LETTERS[1:3], labels = c("Eins" = "A", "Drei" = "C"))
    expect_error(i_as_factor(require_all_labels = T))

    ## 'missing_to_na' - as missing descaled values will be set NA

    x <- i_labelled(c(1,2,3,-9), na_values = -9, labels = c(A = 1, B = 2, C = 3, X = -9))
    x <- i_as_factor(x, missing_to_na = T)
    expect_true(is.na(x[[4]]))
    expect_equal(levels(x), c("X", "A", "B", "C"))

    ## 'remove_missing_labels' - remove label for as missing desclared values - will not become factor levels

    x <- i_labelled(c(1,2,3,-9), na_values = -9, labels = c(A = 1, B = 2, C = 3, X = -9))
    x <- i_as_factor(x, missing_to_na = T, remove_missing_labels = T)
    expect_true(is.na(x[[4]]))
    expect_equal(levels(x), c("A", "B", "C"))

    ### ignore remove_missing_labels = T when missing_to_na = F
    x <- i_labelled(c(1,2,3,-9), na_values = -9, labels = c(A = 1, B = 2, C = 3, X = -9))
    x <- i_as_factor(x, missing_to_na = F, remove_missing_labels = T)
    expect_false(is.na(x[[4]]))
    expect_equal(levels(x), c("X", "A", "B", "C"))

    ## 'only_labelled' - convert only to factor when valid labels

    x <- c(1:3,-9)
    attr(x, "labels") <- c(A = 1, B = 2, C = 3, X = -9)
    x <- i_as_factor(x, only_labelled = T)
    expect_false(is.factor(x))
    expect_true(is.numeric(x))
    expect_equal(names(attr(x, "labels", T)), c("A", "B", "C", "X"))

  }
)

