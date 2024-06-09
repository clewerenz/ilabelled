library(devtools)
devtools::load_all()

testData <- data.frame(
  validScale = i_labelled(1:3, scale = c("nominal")),
  invalidScale = i_labelled(1:3),
  missingScale = i_labelled(1:3)
)
attr(testData$invalidScale, "scale") <- "invalid"


i_labelled(1:2, na_values = "A")
i_labelled(c("A", "B"), na_values = 1)

i_labelled(c("A", "B"), na_range = c(1,3))
i_labelled(1:3, na_range = c(1,3))



