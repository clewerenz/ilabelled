test_that("subsetting via [", {
  # attributes are preserved

  x <- i_labelled(1:3, labels = c("A" = 1, "B" = 2, "C" = 3))
  ref <- i_labelled(2:3, labels = c("A" = 1, "B" = 2, "C" = 3))

  expect_equal(x[c(2,3)], ref)
})


test_that("subsetting via [[", {
  # attributes are preserved

  x <- i_labelled(1:3, labels = c("A" = 1, "B" = 2, "C" = 3))
  ref <- i_labelled(2, labels = c("A" = 1, "B" = 2, "C" = 3))

  expect_equal(x[[2]], ref)
})



