
#' set scale level
#' @export
#' @returns Returns x with scale label set
#' @param x vector
#' @param scale scale level (nominal, ordinal, scale) as string or NULL (NULL will remove scale level)
i_scale <- function(x, scale = NULL){
  if(!is.null(scale)){
    scale <- tolower(scale)
  }
  if(!.valid_scale(scale)){
    stop("scale must be character vector of length 1")
  }
  if(!is.null(scale) && !scale %in% c("nominal", "ordinal", "scale")){
    stop("scale must be either 'nominal', 'ordinal' or 'scale'")
  }
  structure(
    x,
    scale = scale
  )
}


#' validate scale label - intern
#' @description
#' run-time-tests for scale level
#' runs internally
#'
#' @returns T/F
#' @param x vector
.valid_scale <- function(x){
  if(is.null(x)){
    TRUE
  }else if(!(is.character(x) && length(x) == 1 && !is.logical(x))){
    FALSE
  }else{
    TRUE
  }
}


#' validate variable scale level
#' @description
#' returns boolean when applied to vector
#'
#' returns a named list when applied to data.frame
#'
#' @returns T/F
#' @param x vector or data.frame
#' @export
i_valid_scale <- function(x){
  UseMethod("i_valid_scale")
}


#' @export
i_valid_scale.default <- function(x){
  y <- attr(x, "scale", TRUE)
  !is.null(y) && .valid_scale(y) && any(y %in% c("nominal", "ordinal", "scale"))
}


#' @export
i_valid_scale.data.frame <- function(x){
  sapply(x, i_valid_scale, USE.NAMES = TRUE, simplify = FALSE)
}


