

#' set variable label
#' @returns x with variable label applied
#' @export
#' @param x vector
#' @param label variable label as string or NULL (NULL will remove label)
i_label <- function(x, label){
  if(!.valid_label(label)){
    stop("label must be character vector of length 1")
  }
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
#' @returns T/F
#' @param x vector
.valid_label <- function(x){
  if(is.null(x)){
    TRUE
  }else if(!is.character(x)){
    FALSE
  }else if(is.logical(x)){
    FALSE
  }else if(any(is.na(x))){
    FALSE
  }else if(!length(x) == 1){
    FALSE
  }else{
    TRUE
  }
}


#' validate variable labels
#' @description
#' returns boolean when applied to vector
#'
#' returns a named list when applied to data.frame
#'
#' @returns T/F
#' @param x vector or data.frame
#' @export
i_valid_label <- function(x){
  UseMethod("i_valid_label")
}


#' @export
i_valid_label.default <- function(x){
  y <- attr(x, "label", TRUE)
  !is.null(y) && .valid_label(y)
}


#' @export
i_valid_label.data.frame <- function(x){
  sapply(x, i_valid_label, USE.NAMES = TRUE, simplify = FALSE)
}
