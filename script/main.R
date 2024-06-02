devtools::load_all()


set.seed(1234)

a <- sample(c(1:3, NA), 10, replace = T)
b <- i_labelled(sample(c(1:3,NA), 10, replace = T), labels = c("A" = 1, "B" = 2, "C" = 3))
c <- factor(sample(c("X","Y","Z", NA), 10, replace = T))
df <- data.frame(a,b,c)

i_table(a,b)
i_table(df, table_args = list(useNA = "ifany"))
