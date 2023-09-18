

i_labelled <- function(x, ...){
  stopifnot("Date" %in% class(x) || (is.vector(x) && is.atomic(x)) || is.factor(x) || "i_labelled" %in% class(x))
  return(.init(x, ...))
}


i_labelled_origin <- function(x){
  .deinit(x)
}


#' @importFrom methods is
is.i_labelled <- function(x){
  is(x,'i_labelled') | is(x,'i_labelled')
}


`[.i_labelled` <- function(x, ...){
  vctrs::vec_restore(NextMethod("["), x)
}

