library(devtools)
devtools::load_all()


ww <- i_labelled(c("A", "1"), labels = c("A" = "1"))

xx %in% 1
xx == 1

ww %in% "A"
ww == "A"

ww %in% i_labelled(c("A"))
ww == i_labelled(c("A"))

ww %in% i_labelled(1)
ww == i_labelled(1)

ww %in% i_labelled(c("1"), labels = c("A" = "1"))
ww == i_labelled(c("1"), labels = c("A" = "1"))

ww %in% i_labelled(1, labels = c("A" = 1))
ww == i_labelled(1, labels = c("A" = 1))



