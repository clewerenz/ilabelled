library(devtools)
devtools::load_all()


x <- i_labelled(c("A", "B", "C"), labels = c("AA" = "A", "BB" = "B", "CC" = "C"), na_values = c("C"))

