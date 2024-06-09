library(devtools)
devtools::load_all()

testData <- data.frame(
  V1 = i_labelled(1:3, labels = c("A" = 1, "B" = 2, "A_dubplicated_" = 3))
)

testVec <- i_labelled(1:3, labels = c("A" = 1, "B" = 2, "A_dubplicated_" = 3))


i_recode(testVec, "A" = 1 ~ x == 3, copy = FALSE)
i_recode(testVec, "A" = 1 ~ x == 3, copy = TRUE)



i_recode(testData, "A" = 1 ~ V1 == 3)
i_recode(testData, "A" = 1 ~ V1 == 3, copy = "V1")


