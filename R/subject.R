
#' add subject to variable
#' @description
#' add subject to i_labelled object
#'
#' @returns x with subject applied
#' @export
#' @param x vector
#' @param subject variable label as string or NULL (NULL will remove label)
i_subject <- function(x, subject){
  if(!.valid_subject(subject)){
    stop("subject must be character vector of length 1 or NULL")
  }
  structure(
    x,
    subject = subject
  )
}


#' validate subject - intern
#' @description
#' contains run-time-tests subject
#' runs internally
#'
#' @returns T/F
#' @param x character vector or NULL
.valid_subject <- function(x){
  if(is.null(x)){
    TRUE
  }else if(is.atomic(x) && is.character(x) && length(x) == 1){
    TRUE
  }else{
    FALSE
  }
}


#' validate subject
#' @description
#' returns boolean when applied to vector
#'
#' returns a named list when applied to data.frame
#'
#' @returns T/F
#' @param x vector or data.frame
#' @export
i_valid_subject <- function(x){
  UseMethod("i_valid_subject")
}


#' @export
i_valid_subject.default <- function(x){
  y <- attr(x, "subject", TRUE)
  !is.null(y) && .valid_subject(y)
}


#' @export
i_valid_subject.data.frame <- function(x){
  sapply(x, i_valid_subject, USE.NAMES = TRUE, simplify = FALSE)
}
