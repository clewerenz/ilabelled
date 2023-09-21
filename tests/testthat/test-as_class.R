test_that(
  "Test changing data class of i_labelled objects", {

    # data to character
    x <- i_labelled(iris$Species, label = "Species")
    y <- i_data_to_character(x)
    expect_equal(class(y), "i_labelled")
    expect_equal(`attributes<-`(y, NULL)[1:5], c("1","1","1","1","1"))
    expect_equal(attr(y, "label", T), "Species")
    expect_equal(names(attr(y, "labels", T)), c("setosa","versicolor", "virginica"))
    expect_equal(unname(attr(y, "labels", T)), c(1,2,3))

    # data to integer
    x <- i_labelled(seq(1,5,.5), label = "Species", labels = c("bla" = 1, "bli" = 2, "blubb" = 3))
    y <- i_data_to_integer(x)
    expect_equal(class(y), "i_labelled")
    expect_equal(`attributes<-`(y, NULL)[1:5], c(1,1,2,2,3))
    expect_equal(attr(y, "label", T), "Species")
    expect_equal(names(attr(y, "labels", T)), c("bla","bli", "blubb"))
    expect_equal(unname(attr(y, "labels", T)), c(1,2,3))
    expect_warning(i_data_to_integer(i_labelled(as.character(iris$Species))))
    z <- suppressWarnings(i_data_to_integer(i_labelled(as.character(iris$Species))))
    expect_true(all(is.na(z)))


    # as factor
    x <- i_labelled(c(1,2,3,1,2,3), labels = c(A = 1, NULL = 2, C = 3), label = "Bla")
    y <- i_labelled(c(1,2,3,1,2,3), labels = c(A = 1, B = 2, C = 3), label = "Bla")
    z <- i_labelled(c(1,2,3,1,2,3), labels = c(B = 1, A = 2, C = 3), label = "Bla")
    # expect_error(i_as_factor(NULL))
    expect_error(i_as_factor(1:5))
    expect_error(i_as_factor(LETTERS[1:5]))
    expect_error(i_as_factor(x))
    expect_no_error(i_as_factor(y))
    expect_equal(class(i_as_factor(y)), "factor")
    expect_equal(attr(i_as_factor(y), "label", T), "Bla")
    expect_equal(attr(i_as_factor(y), "labels", T), NULL)
    expect_equal(levels(i_as_factor(y)), c("A","B","C"))
    expect_equal(levels(i_as_factor(z)), c("B","A","C"))

  }
)
