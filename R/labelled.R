

#' class constructor
#' @export
#' @param x vector
#' @param label variable label
#' @param labels value labels as named vector or named list (e.g. list("A"=1, "B"=2) or c("A"=1, "B"=2) or setNames(c(1,2), c("A","B")))
#' @param na_values missing values (e.g. c(888, 999))
#' @param na_range range of missing values (e.g. c(-9,-1))
#' @param ... further attributes passed to class
i_labelled <- function(x, label = NULL, labels = NULL, na_values = NULL, na_range = NULL, ...){
  stopifnot("Date" %in% class(x) || (is.vector(x) && is.atomic(x)) || is.factor(x) || "i_labelled" %in% class(x))
  if(is.null(labels) && is.factor(x)){
    labels <- stats::setNames(1:length(levels(x)), levels(x))
    .valid_labels(labels)
  }else if(!is.null(labels)){
    .valid_labels(labels)
  }
  if(!is.null(label)){
    .valid_label(label)
  }
  return(.init(x, label = label, labels = labels, na_values = na_values, na_range = na_range, ...))
}


#' check for class i_labelled
#' @export
#' @importFrom methods is
#' @param x vector of class i_labelled
is.i_labelled <- function(x){
  is(x,'i_labelled') | is(x,'i_labelled')
}


#' subsetting vectors of class i_labelled
#' @export
#' @param x vector of class i_labelled
#' @param ... not used
`[.i_labelled` <- function(x, ...){
  vctrs::vec_restore(NextMethod("["), x)
}

