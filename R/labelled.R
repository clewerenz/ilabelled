

#' class constructor
#' @export
#' @param x vector
#' @param label variable label
#' @param labels value labels as named vector (e.g. c("A"=1, "B"=2) or setNames(c(1,2), c("A","B")))
#' @param na_values missing values (e.g. c(888, 999))
#' @param na_range range of missing values (e.g. c(-9,-1))
#' @param ... further attributes passed to class
i_labelled <- function(x, label = NULL, labels = NULL, na_values = NULL, na_range = NULL, ...){
  stopifnot(is.atomic(x))
  if(is.null(labels) && is.factor(x)){
    labels <- stats::setNames(1:length(levels(x)), levels(x))
  }else if(!is.null(labels) || !is.null(attr(x, "labels", T))){
    labels <- .merge_labels(as.list(attr(x, "labels", T)), as.list(labels))
  }
  if(!is.null(label)){
    stopifnot(.valid_label(label))
  }
  return(.init(x, label = label, labels = labels, na_values = na_values, na_range = na_range, ...))
}


#' make all variables in data.frame i_labelled
#' @export
#' @param x data.frame
i_labelled_df <- function(x){
  x[] <- lapply(x, i_labelled)
  x
}


#' check for class i_labelled
#' @export
#' @importFrom methods is
#' @param x vector of class i_labelled
is.i_labelled <- function(x){
  is(x,'i_labelled')
}


#' subsetting vectors of class i_labelled
#' @export
#' @param x vector of class i_labelled
#' @param ... not used
`[.i_labelled` <- function(x, ...){
  vctrs::vec_restore(NextMethod("["), x)
}

