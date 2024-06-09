
# i_valid_scale()

test_that("i_valid_scale: correct return values", {
  testData <- data.frame(
    validScale = i_labelled(1:3, scale = c("nominal")),
    invalidScale = i_labelled(1:3),
    missingScale = i_labelled(1:3)
  )
  attr(testData$invalidScale, "scale") <- "invalid"

  retList <- list(validScale = TRUE, invalidScale = FALSE, missingScale = FALSE)

  expect_true(i_valid_scale(testData$validScale))
  expect_false(i_valid_scale(testData$invalidScale))
  expect_false(i_valid_scale(testData$missingScale))
  expect_equal(i_valid_scale(testData), retList)
})
