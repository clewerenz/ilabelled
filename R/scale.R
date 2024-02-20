
#' set scale level
#' @export
#' @param x vector
#' @param scale scale level (nominal, ordinal, scale) as string or NULL (NULL will remove scale level)
i_scale <- function(x, scale){
  if(!is.null(scale)){
    scale <- tolower(scale)
  }
  .valid_scale(scale)
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
#' @param x vector
.valid_scale <- function(x){
  if(is.null(x)){
    return(invisible(NULL))
  }
  if(!(is.character(x) && length(x) == 1 && !is.logical(x))){
    stop("scale level must be character vector of length 1")
  }
  if(!x %in% c("nominal", "ordinal", "scale")){
    stop("scale level must be either 'nominal', 'ordinal' or 'scale'")
  }
}
