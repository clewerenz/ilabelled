library(devtools)
devtools::load_all()

x <- i_labelled(c(1:4,-9), labels = c("A" = 1, "B" = 2, "C" = 3, "D" = 4), na_values = -9, na_range = c(-9,-1), scale = "nominal", wording = "How are you?")
x

x <- i_labelled(factor(c(1:4,-9)), labels = c("A" = 1, "B" = 2, "C" = 3, "D" = 4), na_values = -9, na_range = c(-9,-1), scale = "nominal", wording = "How are you?")
x
