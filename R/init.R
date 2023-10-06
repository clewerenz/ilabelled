

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
  UseMethod(".init")
}


.init.default <- function(x, label = NULL, labels = NULL, na_values = NULL, na_range = NULL, ...) {
  # set class i_labelled
  if(is.numeric(x) || is.factor(x)){
    structure(
      .Data = as.double(x),
      class = "i_labelled",
      label = label,
      labels = labels,
      na_values = na_values,
      na_range = na_range,
      ...
    )
  }else{
    structure(
      .Data = as.character(x),
      class = "i_labelled",
      label = label,
      labels = labels,
      na_values = na_values,
      na_range = na_range,
      ...
    )
  }
}


.init.i_labelled <- function(x, label = NULL, labels = NULL, na_values = NULL, na_range = NULL, ...) {
  # do nothing
  x
}
