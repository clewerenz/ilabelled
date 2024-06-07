
#' add annotation to variable
#' @description
#' add annotation to i_labelled object
#'
#' can be used to store additional information about a variable
#'
#' @returns x with annotation applied
#' @export
#' @param x vector
#' @param annotation variable label as string or NULL (NULL will remove label)
#' @param overwrite overwrite existing annotation and replace with new annotation
i_annotation <- function(x, annotation, overwrite = FALSE){
  if(!.valid_annotation(annotation)){
    stop("annotation must be character vector or NULL")
  }
  if(!overwrite && !is.null(annotation)){
    old_annotation <- attr(x, "annotation", TRUE)
    if(!is.null(old_annotation) && !.valid_annotation(old_annotation)){
      stop("cannot add annotation - invald existing annotation")
    }
    annotation <- c(old_annotation, annotation)
  }
  structure(
    x,
    annotation = annotation
  )
}


#' validate annotation - intern
#' @description
#' contains run-time-tests annotation
#' runs internally
#'
#' @returns T/F
#' @param x character vector or NULL
.valid_annotation <- function(x){
  if(is.null(x)){
    TRUE
  }else if(is.atomic(x) && is.character(x)){
    TRUE
  }else{
    FALSE
  }
}


#' validate annotation
#' @description
#' returns boolean when applied to vector
#'
#' returns a named list when applied to data.frame
#'
#' @returns T/F
#' @param x vector or data.frame
#' @export
i_valid_annotation <- function(x){
  UseMethod("i_valid_annotation")
}


#' @export
i_valid_annotation.default <- function(x){
  y <- attr(x, "annotation", TRUE)
  !is.null(y) && .valid_annotation(y)
}


#' @export
i_valid_annotation.data.frame <- function(x){
  sapply(x, i_valid_annotation, USE.NAMES = TRUE, simplify = FALSE)
}
