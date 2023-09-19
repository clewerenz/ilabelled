

#' set variable label
#' @export
#' @param x vector
#' @param label variable label as string or NULL (NULL will remove label)
i_label <- function(x, label){
  stopifnot(.valid_label(label) || is.null(label))
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
  is.character(x) && length(x) == 1
}


#' validate variable labels
#' @export
#' @description
#' returns boolean
#'
#' @param x vector or data.frame
i_valid_label <- function(x){
  y <- attr(x, "label", T)
  .valid_label(y)
}

