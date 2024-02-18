
#' remove class i_labelled and return base R class
#' @description
#' - when value labels for all values are available will return factor
#' - when value labels are missing will unclass i_labelled
#' - remove class i_labelled and return variable as base R class
#'
#' @param x vector or data.frame
#' @param as_factor convert to factor, when all value labels are available
#' @param keep_attributes should attributes be preserved
#' @export
i_to_base_class <- function(x, as_factor = T, keep_attributes = F){
  UseMethod("i_to_base_class")
}


#' @export
i_to_base_class.default <- function(x, as_factor = T, keep_attributes = F){
  stopifnot(is.atomic(x))
  stopifnot(is.logical(as_factor) && length(as_factor) == 1)
  stopifnot(is.logical(keep_attributes) && length(keep_attributes) == 1)

  if(!is.i_labelled(x)){
    return(x)
  }

  # x <- i_missing_to_na(x)
  # x <- i_remove_missing_labels(x)

  labels <- attr(x, "labels", T)
  labels <- unique(labels)
  values <- unique(x)
  values <- values[!is.na(values)]

  if(as_factor && length(labels) > 0){ #  && all(.i_find_in(values, labels))
    i_as_factor(x, keep_attributes = keep_attributes)
  }else{
    i_unclass(x, keep_attributes = keep_attributes)
  }
}


#' @export
i_to_base_class.data.frame <- function(x, as_factor = T, keep_attributes = F){
  x[] <- lapply(x, function(y) i_to_base_class(y, as_factor = as_factor, keep_attributes = keep_attributes))
  x
}
