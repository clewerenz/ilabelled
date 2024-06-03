devtools::load_all()

i_labelled(c(1:3,5,9), labels = c("A" = 1, "B" = 2, "C" = 3, "D" =5, "E" = 9), na_values = c(3,9)) |>
  i_as_factor(missing_to_na = T)

i_labelled(c(1:3,5,9), labels = c("A" = 1, "B" = 2, "C" = 3, "D" =5, "E" = 9), na_values = c(3,9)) |>
  i_remove_missing_labels()

i_labelled(c(1:3,5,9), labels = c("A" = 1, "B" = 2, "C" = 3, "D" =5, "E" = 9), na_range = c(3,10)) |>
  i_remove_missing_labels()

i_labelled(c(1:3,5,9), labels = c("A" = 1, "B" = 2, "C" = 3, "D" =5, "E" = 9), na_range = c(3,10)) |>
  i_missing_to_na(remove_missing_labels = F)

i_labelled(c(1:3,5,9), labels = c("A" = 1, "B" = 2, "C" = 3, "D" =5, "E" = 9), na_values = c(3,9), na_range = c(5,10)) |>
  i_missing_to_na(remove_missing_labels = T)

i_labelled(c("A","B","C","X"), na_values = "X", labels = c("Eins" = "A", "Zwei" = "B", "Drei" = "C", "Null" = "X")) |>
  i_missing_to_na(remove_missing_labels = F)
