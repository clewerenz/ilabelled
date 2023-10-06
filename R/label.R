

#' set variable label
#' @export
#' @param x vector
#' @param label variable label as string or NULL (NULL will remove label)
i_label <- function(x, label){
  .valid_label(label)
  structure(
    x,
    label = label
  )
}


#' validate variable label - intern
#' @description
#' run-time-tests for variable label
#' runs internally
#'
#' @param x vector
.valid_label <- function(x){
  if(is.null(x)){
    return(invisible(NULL))
  }
  if(!(is.character(x) && length(x) == 1 && !is.logical(x))){
    stop("label must be character vector of length 1")
  }
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
  is_valid <- !"try-error" %in% class(try(.valid_label(y), silent = T))
  !is.null(y) && is_valid
}


#' @export
i_valid_label.data.frame <- function(x){
  sapply(x, i_valid_label, USE.NAMES = T, simplify = F)
}
