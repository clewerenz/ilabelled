
#' add wording to variable
#' @description
#' add wording to i_labelled object
#'
#' can be used to store question text
#'
#' @returns x with wording applied
#' @export
#' @param x vector
#' @param wording variable label as string or NULL (NULL will remove label)
i_wording <- function(x, wording){
  if(!.valid_wording(wording)){
    stop("wording must be character vector of length 1 or NULL")
  }
  structure(
    x,
    wording = wording
  )
}


#' validate wording - intern
#' @description
#' contains run-time-tests wording
#' runs internally
#'
#' @returns T/F
#' @param x character vector or NULL
.valid_wording <- function(x){
  if(is.null(x)){
    TRUE
  }else if(is.atomic(x) && is.character(x) && length(x) == 1){
    TRUE
  }else{
    FALSE
  }
}


#' validate wording
#' @description
#' returns boolean when applied to vector
#'
#' returns a named list when applied to data.frame
#'
#' @returns T/F
#' @param x vector or data.frame
#' @export
i_valid_wording <- function(x){
  UseMethod("i_valid_wording")
}


#' @export
i_valid_wording.default <- function(x){
  y <- attr(x, "wording", TRUE)
  !is.null(y) && .valid_wording(y)
}


#' @export
i_valid_wording.data.frame <- function(x){
  sapply(x, i_valid_wording, USE.NAMES = TRUE, simplify = FALSE)
}
