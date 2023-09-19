test_that(
  "Test function caining", {

    x <- iris$Species |>
      i_labelled(na_values = 1:3) |>
      i_label("Test") |>
      i_label(NULL) |>
      i_labelled(na_range = c(-9,-1)) |>
      i_label("AlleMeineEntchen") |>
      i_labels(A = 1, B = 2, C = 3) |>
      i_labels(test = 1, NULL = 3)

    expect_equal(class(x), "i_labelled")
    expect_equal(attr(x, "label", T), "AlleMeineEntchen")
    expect_equal(attr(x, "na_values", T), 1:3)
    expect_equal(attr(x, "na_range", T), NULL)
    expect_equal(!"na_range" %in% names(attr(x, "na_range", T)), TRUE)
    expect_equal(names(attr(x, "labels", T)), c("test","B"))
    expect_equal(unname(attr(x, "labels", T)), 1:2)

  }
)
