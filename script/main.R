library(devtools)
devtools::load_all()

dataWithSubjects <- data.frame(
  V1 = i_labelled(1:3, subject = "my subject 1"),
  V2 = i_labelled(1:3, subject = "my subject 1"),
  V3 = i_labelled(1:3, subject = "my subject 2"),
  V4 = i_labelled(1:3)
)


i_get_equal_subject(dataWithSubjects, subject = 1:3)
