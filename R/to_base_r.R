
#' remove class i_labelled and return base R class
#' @description
#' - remove class i_labelled and return variable as base R class
#' - when value labels for all values are available will return factor
#' - when value labels are missing will unclass i_labelled
#'
#' @param x vector or data.frame
#' @export
i_to_base_class <- function(x){
  UseMethod("i_to_base_class")
}


#' @export
i_to_base_class.default <- function(x){
  if(!is.i_labelled(x)){
    return(x)
  }

  x <- i_missing_to_na(x)
  x <- i_remove_missing_labels(x)

  labels <- attr(x, "labels", T)
  labels <- unique(labels)
  values <- unique(x)
  values <- values[!is.na(values)]

  if(all(.i_find_in(values, labels))){
    i_as_factor(x)
  }else{
    i_unclass(x)
  }
}


#' @export
i_to_base_class.data.frame <- function(x){
  x[] <- lapply(x, i_to_base_class)
  x
}
