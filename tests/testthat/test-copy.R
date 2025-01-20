test_that("i_copy: default behaviour", {
    x <- i_labelled(iris$Species, label = "Species", na_values = 1, na_range = c(2,3))
    y <- i_labelled(as.numeric(iris$Species))
    expect_no_error(i_copy(to = y, from = x, "all"))
    z <- i_copy(to = y, from = x, "all")
    expect_equal(attr(z, 'label', T), "Species")
    expect_equal(unname(attr(z, 'labels', T)), 1:3)
    expect_equal(names(attr(z, 'labels', T)), c("setosa", "versicolor", "virginica"))
    expect_equal(attr(z, 'na_values', T), 1)
    expect_equal(attr(z, 'na_range', T), 2:3)
  }
)

test_that("i_copy: overwrite parameter", {
  x <- i_labelled(iris$Species, label = "SpeciesX", annotation = "my annotation")
  y <- i_labelled(as.numeric(iris$Species), label = "SpeciesY")
  z <- i_copy(to = y, from = x, what = "all", overwrite = FALSE)

  expect_equal(attr(z, "label", exact = TRUE), "SpeciesY")
})
