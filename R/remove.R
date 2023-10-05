#' remove missing labels
#' @description
#' remove values labels from values which are declared as missing
#'
#' @param x vector or data.frame
#' @export
i_remove_missing_labels <- function(x){
  UseMethod("i_remove_missing_labels")
}


#' @export
i_remove_missing_labels.default <- function(x){
  stopifnot(is.atomic(x))
  nas <- c(attr(x, "na_values", T), attr(x, "na_range", T))
  labels <- attr(x, "labels", T)
  if(!is.null(labels)){
    labels <- labels[!labels %in% nas]
    if(length(labels) < 1){
      labels <- NULL
    }
    return(structure(x, labels = labels))
  }else{
    return(x)
  }
}


#' @export
i_remove_missing_labels.data.frame <- function(x){
  x[] <- lapply(x, i_remove_missing_labels)
  x
}



#' remove variable label
#' @description
#' remove variable label
#' keep other attributes
#'
#' @param x vector or data.frame
#' @export
i_remove_label <- function(x){
  UseMethod("i_remove_label")
}


#' @export
i_remove_label.default <- function(x){
  stopifnot(is.atomic(x))
  structure(
    x,
    label = NULL
  )
}


#' @export
i_remove_label.data.frame <- function(x){
  x[] <- lapply(x, i_remove_label)
  x
}


#' remove all value labels
#' @description
#' remove all value labels
#' keep other attributes
#'
#' @param x vector or data.frame
#' @export
i_remove_labels <- function(x){
  UseMethod("i_remove_labels")
}


#' @export
i_remove_labels.default <- function(x){
  stopifnot(is.atomic(x))
  structure(
    x,
    labels = NULL
  )
}


#' @export
i_remove_labels.data.frame <- function(x){
  x[] <- lapply(x, i_remove_labels)
  x
}


#' remove as na values
#' @description
#' remove na values (information which values should be handled as missing)
#' keep other attributes
#'
#' @param x vector or data.frame
#' @export
i_remove_na_values <- function(x){
  UseMethod("i_remove_na_values")
}


#' @export
i_remove_na_values.default <- function(x){
  stopifnot(is.atomic(x))
  structure(
    x,
    na_values = NULL
  )
}


#' @export
i_remove_na_values.data.frame <- function(x){
  x[] <- lapply(x, i_remove_na_values)
}


#' remove as na range
#' @description
#' remove na range (information which values should be handled as missing)
#' keep other attributes
#'
#' @param x vector or data.frame
#' @export
i_remove_na_range <- function(x){
  UseMethod("i_remove_na_range")
}


#' @export
i_remove_na_range.default <- function(x){
  stopifnot(is.atomic(x))
  structure(
    x,
    na_range = NULL
  )
}


#' @export
i_remove_na_range.data.frame <- function(x){
  x[] <- lapply(x, i_remove_na_range)
}


