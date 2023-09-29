# survey_data_raw <- data.frame(
#   V1 = sample(c(1:5,1:5,-9,-8), 50, replace = T),
#   V2 = sample(c(1:2,1:2,-9,-8), 50, replace = T),
#   V3 = sample(c(lorem::ipsum_words(50, F),-9,-9,-8), 50, replace = T),
#   V4 = sample(c(abs(round(rnorm(20,mean = 5,sd = 2),2)),-9,-9,-8),50, replace = T)
# )
#
# survey_data_codebook <- list(
#   V1 = list(
#     label = "Ordinal Scale",
#     labels = c("Very Good" = 1, "2" = 2, "3" = 3, "4" = 4, "Very Poor" = 5, "Not answered" = -9, "Dropout" = -8),
#     na_range = -9:-8
#   ),
#   V2 = list(
#     label = "Nominal Scale",
#     labels = c("Yes" = 1, "No" = 2, "Not answered" = -9, "Dropout" = -8),
#     na_range = -9:-8
#   ),
#   V3 = list(
#     label = "Text",
#     labels = c("Not answered" = -9, "Dropout" = -8),
#     na_range = -9:-8
#   ),
#   V4 = list(
#     label = "Metric Scale",
#     labels = c("Not answered" = -9, "Dropout" = -8),
#     na_range = -9:-8
#   )
# )
#
# usethis::use_data(survey_data_raw, overwrite = T)
# usethis::use_data(survey_data_codebook, overwrite = T)
