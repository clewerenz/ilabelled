

#' backend for i_labelled
#' @description
#' all arguments are passed from i_labelled
#'
#' @param x vector
#' @param label variable label
#' @param labels value labels as named vector
#' @param na_values missing values (e.g. c(888, 999))
#' @param na_range range of missing values (e.g. c(-9,-1))
#' @param ... further attributes passed to class
.init <- function(x, label = NULL, labels = NULL, na_values = NULL, na_range = NULL, ...) {

  if(!is.i_labelled(x)){
    x <- structure(
      x,
      class = "i_labelled",
      label = label,
      labels = labels,
      na_values = na_values,
      na_range = na_range,
      ...
    )
  }
  return(x)
}

