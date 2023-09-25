test_that(
  "Test print generic", {

    x <- i_labelled(iris$Species)
    expect_output(print.i_labelled(x))

    expect_output(i_print_labels(x))

  }
)
