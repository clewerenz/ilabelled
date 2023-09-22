

#' set variable label
#' @export
#' @param x vector
#' @param label variable label as string or NULL (NULL will remove label)
i_label <- function(x, label){
  stopifnot(.valid_label(label)) #  || is.null(label)
  structure(
    x,
    label = label
  )
}


#' validate variable label - intern
#' @description
#' returns boolean
#'
#' @param x vector
.valid_label <- function(x){
  is.null(x) || (is.character(x) && length(x) == 1 && !is.logical(x))
}


#' validate variable labels
#' @description
#' returns boolean
#' returns a named list when applied to data.frame
#'
#' @param x vector or data.frame
#' @export
i_valid_label <- function(x){
  UseMethod("i_valid_label")
}


#' @export
i_valid_label.default <- function(x){
  y <- attr(x, "label", T)
  !is.null(y) && .valid_label(y)
}


#' @export
i_valid_label.data.frame <- function(x){
  sapply(x, i_valid_label, USE.NAMES = T, simplify = F)
}
