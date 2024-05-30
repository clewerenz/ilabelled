# test_that("'%lin%' works on labels - 'in' on underlying values", {
#   x <- i_labelled(sample(c(1:3,NA), 10, replace = T), labels = c("A" = 1, "B" = 2, "C" = 3), na_values = 3)
#   expect_true(all.equal(x %lin% "A", x %in% 1))
# })

test_that("'%in% over labels", {
  x <- i_labelled(sample(c(1:3,NA), 10, replace = T), labels = c("A" = 1, "B" = 2, "C" = 3), na_values = 3)
  expect_true(all.equal(x %in% "A", x %in% 1))
})

