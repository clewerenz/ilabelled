library(devtools)
devtools::load_all()


x <- i_labelled(c(1:3,NA,5,9,0), labels = c("A" = 1, "B" = 2, "C" = 3, "D" =5, "E" = 9, "X" = 0), na_values = c(3,9))

x != T


# When applied to a single vector:
# keep in mind that when function is applied to vector, instead of a column use x
myVector <- i_labelled(1:4, labels = c("A" = 1, "B" = 2, "C" = 3, "D" = 4))
i_recode(x = myVector, "AB" = 1 ~ x %in% c("A", "B"), "CD" = 2 ~ x == c(3, 4))

# When applied to data.frame (multiple conditions)
myData <- data.frame(
  V1 = i_labelled(1:3, labels = c("A" = 1, "B" = 2, "C" = 3)),
  V2 = i_labelled(c(2:3,-9))
)
i_recode(x = myData, A = 1 ~ V1 %in% c("A", "B"), 2 ~ "V2" == 3, "C" = 999 ~ V2 == -9)

