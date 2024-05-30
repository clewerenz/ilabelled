
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
  if(!scale %in% c("nominal", "ordinal", "scale")){
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
    T
  }
  if(!(is.character(x) && length(x) == 1 && !is.logical(x))){
    F
  }else{
    T
  }
}
