evalDotsLabels <- function(...){
  input <- unlist(list(...))
  eval(input)
}

evalDotsLabels(setNames(1:3, LETTERS[1:3]))
evalDotsLabels(A = 1, B = 2, C = 3)
evalDotsLabels(c(A = 1, B = 2, C = 3))
evalDotsLabels(list(A = 1, B = 2, C = 3))
