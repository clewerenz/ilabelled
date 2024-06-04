library(devtools)
devtools::load_all()


x <- i_labelled(c(1:3,NA,5,9), labels = c("A" = 1, "B" = 2, "C" = 3, "D" =5, "E" = 9), na_values = c(3,9))


i_labels(x, setNames(1, "AA"))
i_labels(x, "AA" = 1)
i_labels(x, NULL)
i_labels(x, A = 1, B = 2, C = "bla")
i_labels(i_labelled(1:3), list(A = 1, NULL = 2, C = 3))
