

#' backend for i_labelled
#' @description
#' all arguments are passed from i_labelled
#'
#' @returns x as i_labelled object with attributes applied to it
#' @param x vector
#' @param label variable label
#' @param labels value labels as named vector
#' @param na_values missing values (e.g. c(888, 999))
#' @param na_range range of missing values (e.g. c(-9,-1))
#' @param scale scale level (nominal, ordinal, scale)
#' @param annotation additional information about variable
#' @param ... further attributes passed to class
.init <- function(x, label = NULL, labels = NULL, na_values = NULL, na_range = NULL, scale = NULL, annotation = NULL, ...) {
  UseMethod(".init")
}


.init.default <- function(x, label = NULL, labels = NULL, na_values = NULL, na_range = NULL, scale = NULL, annotation = NULL, ...) {
  # set class i_labelled
  if(is.numeric(x) || is.factor(x) || is.logical(x)){
    structure(
      .Data = as.double(x),
      class = c("i_labelled", "double"),
      label = label,
      labels = labels,
      na_values = na_values,
      na_range = na_range,
      scale = scale,
      annotation = annotation,
      ...
    )
  }else{
    structure(
      .Data = as.character(x),
      class = c("i_labelled", "character"),
      label = label,
      labels = labels,
      na_values = na_values,
      na_range = na_range,
      scale = scale,
      annotation = annotation,
      ...
    )
  }
}


.init.i_labelled <- function(x, label = NULL, labels = NULL, na_values = NULL, na_range = NULL, scale = NULL, annotation = NULL, ...) {
  # do nothing
  x
}
