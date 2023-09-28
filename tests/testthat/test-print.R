test_that(
  "Test print generic", {

    x <- i_labelled(iris$Species, label = "Species", na_values = 999, na_range = c(-9,-1))
    y <- i_labelled_df(iris)
    y$Species <- i_label(y$Species, label = "Species")
    y$Species <- i_na_range(y$Species, values = c(-9,-1))
    y$Species <- i_na_values(y$Species, values = 999)
    expect_output(print.i_labelled(x))

    # i_get_labels
    ## print output when vector; return list when data.frame
    yLabs <- i_get_labels(y)
    expect_output(i_get_labels(x))
    expect_true(is.list(yLabs))
    expect_true(is.data.frame(yLabs$Species))
    expect_null(yLabs$Sepal.Length)
    expect_null(yLabs$Sepal.Width)
    expect_null(yLabs$Petal.Length)
    expect_null(yLabs$Petal.Width)

    # i_get_label
    ## print output when vector; return list when data.frame
    yLabs <- i_get_label(y)
    expect_output(i_get_labels(x))
    expect_true(is.list(yLabs))
    expect_equal(yLabs$Species, "Species")
    expect_null(yLabs$Sepal.Length)
    expect_null(yLabs$Sepal.Width)
    expect_null(yLabs$Petal.Length)
    expect_null(yLabs$Petal.Width)

    # i_get_na_values
    ## print output when vector; return list when data.frame
    yNa <- i_get_na_values(y)
    expect_output(i_get_na_values(x))
    expect_true(is.list(yNa))
    expect_equal(yNa$Species, 999)
    expect_null(yNa$Sepal.Length)
    expect_null(yNa$Sepal.Width)
    expect_null(yNa$Petal.Length)
    expect_null(yNa$Petal.Width)

    # i_get_na_range
    ## print output when vector; return list when data.frame
    yNa <- i_get_na_range(y)
    expect_output(i_get_na_range(x))
    expect_true(is.list(yNa))
    expect_equal(yNa$Species, c(-9,-1))
    expect_null(yNa$Sepal.Length)
    expect_null(yNa$Sepal.Width)
    expect_null(yNa$Petal.Length)
    expect_null(yNa$Petal.Width)

  }
)
