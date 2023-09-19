test_that(
  "Test labelling functions", {

    x <- i_labelled(iris$Species)
    x <- i_label(x, "Test label")
    expect_equal(attr(x, "label", T), "Test label")
    x <- i_label(x, "BlaBliBlubb")
    expect_equal(attr(x, "label", T), "BlaBliBlubb")
    x <- i_label(x, NULL)
    expect_equal(attr(x, "label", T), NULL)
    expect_false("label" %in% names(attributes(x)))

    expect_error(i_label(x))
    expect_error(i_label(x, c("Bla", "Bli", "Blubb")))
    expect_error(i_label(x, 1:5))
    expect_error(i_label(x, 1234))
    expect_error(i_label(x, factor(LETTERS[1:5])))
    expect_error(i_label(x, list(LETTERS, letters)))
    expect_error(i_label(x, Sys.Date()))
    expect_error(i_label(x, NA))

    x <- i_labelled(iris$Species)
    attr(x, "label") <- NA
    expect_false(i_valid_label(x))
    expect_false(i_valid_label(i_labelled(iris$Species)))
    expect_true(i_valid_label(i_labelled(iris$Species, label = "Test label")))

    # validate varaible label
    x <- i_labelled(iris$Species)
    expect_true(i_valid_label(i_labelled(iris$Species, label = "Test label")))
    expect_false(i_valid_label(i_labelled(iris$Species)))
    attr(x, "label") <- NA
    expect_false(i_valid_label(x))
    attr(x, "label") <- 1:5
    expect_false(i_valid_label(x))
    attr(x, "label") <- LETTERS[1:5]
    expect_false(i_valid_label(x))
    attr(x, "label") <- NULL
    expect_false(i_valid_label(x))
    attr(x, "label") <- character(0)
    expect_false(i_valid_label(x))
    attr(x, "label") <- TRUE
    expect_false(i_valid_label(x))
    attr(x, "label") <- FALSE
    expect_false(i_valid_label(x))

  }
)
