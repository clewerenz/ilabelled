test_that(
  "remove attributes functions", {

    # remove variable label
    x <- data.frame(
      V1 = i_labelled(1:5, label = "Varaible label 1"),
      V2 = i_labelled(6:10, label = "Varaible label 2")
    )
    attr(x$V1, "test") <- T
    expect_no_error(i_remove_label(x$V1))
    expect_no_error(i_remove_label(x))
    y <- i_remove_label(x)
    expect_null(attr(y$V1, "label", T))
    expect_null(attr(y$V2, "label", T))
    expect_true(attr(y$V1, "test"))

    # remove value labels
    x <- data.frame(
      V1 = i_labelled(1:3, labels = c(A = 1, B = 2, C = 3)),
      V2 = i_labelled(LETTERS[1:3], labels = c("Eins" = "A", "Zwei" = "B", "Drei" = "C"))
    )
    attr(x$V1, "test") <- T
    expect_no_error(i_remove_labels(x$V1))
    expect_no_error(i_remove_labels(x))
    y <- i_remove_labels(x)
    expect_null(attr(y$V1, "labels", T))
    expect_null(attr(y$V2, "labels", T))
    expect_true(attr(y$V1, "test"))

    # remove na values
    x <- data.frame(
      V1 = i_labelled(c(1:3,-9), na_values = -9),
      V2 = i_labelled(c(LETTERS[1:3],"X"), na_values = "X")
    )
    attr(x$V1, "test") <- T
    expect_no_error(i_remove_na_values(x$V1))
    expect_no_error(i_remove_na_values(x))
    y <- i_remove_na_values(x)
    expect_null(attr(y$V1, "na_values", T))
    expect_null(attr(y$V2, "na_values", T))
    expect_true(attr(y$V1, "test"))

    # remove na range
    x <- data.frame(
      V1 = i_labelled(c(1:3,-8,-9), na_range = c(-8,-9)),
      V2 = i_labelled(c(11:13,-18,-19), na_range = c(-19,-18))
    )
    attr(x$V1, "test") <- T
    expect_no_error(i_remove_na_range(x$V1))
    expect_no_error(i_remove_na_range(x))
    y <- i_remove_na_range(x)
    expect_null(attr(y$V1, "na_range", T))
    expect_null(attr(y$V2, "na_range", T))
    expect_true(attr(y$V1, "test"))

  }
)
