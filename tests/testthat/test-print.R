test_that(
  "Test print", {

    x <- i_labelled(iris$Species, label = "Species", na_values = 999, na_range = c(-9,-1))
    y <- i_labelled_df(iris)
    y$Species <- i_label(y$Species, label = "Species")
    y$Species <- i_na_range(y$Species, values = c(-9,-1))
    y$Species <- i_na_values(y$Species, values = 999)
    expect_output(print.i_labelled(x))

  }
)
