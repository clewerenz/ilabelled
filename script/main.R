devtools::load_all()


set.seed(1234)

a <- sample(c("X","Y","Z", NA), 10, replace = T)
b <- sample(c(1:3, NA), 10, replace = T)
x <- i_labelled(sample(c(1:3,NA), 10, replace = T), labels = c("A" = 1, "B" = 2, "C" = 3))
y <- factor(sample(c("X","Y","Z", NA), 10, replace = T))
z <- i_labelled(sample(c(1,2), 10, replace = T), labels = c("male" = 1, "female" = 2))

df <- data.frame(a,b,x,y,z)

i_table(df[c(1,2,3)], NA)
