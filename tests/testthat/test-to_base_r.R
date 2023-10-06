test_that(
  "to_base_class conversion", {

    # Defaut behavior

    ## convert vector to factor, when all value labels are give
    ### numeric vector
    x <- i_labelled(c(1:3,-9), labels = c("A"=1,"B"=2,"C"=3,"X"=-9))
    expect_no_error(i_to_base_class(x))
    y <- i_to_base_class(x)
    expect_true(is.factor(y))
    expect_equal(levels(y), c("X","A","B","C"))
    expect_equal(as.numeric(y), c(2,3,4,1))
    ### character vector
    x <- i_labelled(c(LETTERS[1:3],"X"), labels = c("Eins"="A","Zwei"="B","Drei"="C","Null"="X"))
    expect_no_error(i_to_base_class(x))
    y <- i_to_base_class(x)
    expect_true(is.factor(y))
    expect_equal(levels(y), c("Eins","Zwei","Drei","Null"))
    expect_equal(as.numeric(y), c(1,2,3,4))

    ## convert NOT to factor, when value labels are missing
    ### when numeric value - return numeric vector
    x <- i_labelled(c(1:3,-9), labels = c("A"=1,"B"=2,"C"=3))
    expect_no_error(i_to_base_class(x))
    y <- i_to_base_class(x)
    expect_false(is.factor(y))
    expect_true(is.numeric(y))
    expect_null(levels(y))
    expect_equal(y, c(1,2,3,-9))
    ### when character vector - return character vector
    x <- i_labelled(c(LETTERS[1:3],"X"), labels = c("Eins"="A","Zwei"="B","Drei"="C"))
    expect_no_error(i_to_base_class(x))
    y <- i_to_base_class(x)
    expect_false(is.factor(y))
    expect_true(is.character(y))
    expect_null(levels(y))
    expect_equal(y, c(LETTERS[1:3],"X"))
    ### no value labels given - return unclassed i_labelled
    #### i_labelled double
    x <- i_labelled(c(1:3,-9))
    expect_no_error(i_to_base_class(x))
    y <- i_to_base_class(x)
    expect_false(is.factor(y))
    expect_true(is.numeric(y))
    expect_null(levels(y))
    expect_equal(y, c(1:3,-9))
    #### i_labelled character
    x <- i_labelled(c(LETTERS[1:3],"X"))
    expect_no_error(i_to_base_class(x))
    y <- i_to_base_class(x)
    expect_false(is.factor(y))
    expect_true(is.character(y))
    expect_null(levels(y))
    expect_equal(y, c(LETTERS[1:3],"X"))

    # Parameter testing

    ## 'as_factor': default is true (tested with default behavior)
    ### when as_factor = F, never return factor - return unclassed object
    x <- i_labelled(c(1:3,-9), labels = c("A"=1,"B"=2,"C"=3,"X"=-9))
    expect_no_error(i_to_base_class(x, as_factor = F))
    y <- i_to_base_class(x, as_factor = F)
    expect_false(is.factor(y))
    expect_false(is.i_labelled(y))
    expect_true(is.numeric(y))
    expect_null(levels(y))
    expect_equal(y, c(1:3,-9))

  }
)
