

#' define missing values
#' @description
#' define which values will be handled as missing values
#'
#' @param x vector
#' @param values vector with missing values e.g. c(888,999) or NULL (NULL will remove all missing values)
#' @param sort sort values
#' @param desc sort values in descending order
#' @export
i_na_values <- function(x, values, sort = T, desc = F){
  UseMethod("i_na_values")
}


#' @export
i_na_values.default <- function(x, values, sort = T, desc = F){
  stopifnot(.valid_na_values(values))
  if(sort){
    values <- sort(values, desc)
  }
  structure(
    x,
    na_values = values
  )
}


#' @export
i_na_values.data.frame <- function(x, values, sort = T, desc = F){
  x[] <- lapply(x, function(y) i_na_values(y, values, sort, desc))
  x
}


#' define missing range
#' @description
#' define which values will be handled as missing values
#'
#' @param x vector
#' @param values vector with missing range e.g. c(-9:-1) or NULL (NULL will remove all missing values)
#' @export
i_na_range <- function(x, values){
  UseMethod("i_na_range")
}


#' @export
i_na_range.default <- function(x, values){
  stopifnot(.valid_na_range(values))
  structure(
    x,
    na_range = values
  )
}


#' @export
i_na_range.data.frame <- function(x, values){
  x[] <- lapply(x, function(y) i_na_range(y, values))
  x
}


#' missing values to NA
#' @description
#' all values declared as missing will be recoded as NA
#' set missing values via i_labelled(), i_na_values() or i_na_range()
#'
#' @param x vector or data.frame
#' @param remove_missing_labels remove values labels from values which are declared as missing
#' @export
i_missing_to_na <- function(x, remove_missing_labels = F){
  UseMethod("i_missing_to_na")
}


#' @export
i_missing_to_na.default <- function(x, remove_missing_labels = F){
  stopifnot(is.atomic(x) || is.null(x))

  na_vals <- attr(x, "na_values", T)
  na_range <- attr(x, "na_range", T)
  stopifnot(.valid_na_values(na_vals) || .valid_na_range(na_range))
  if(!is.null(na_range)){
    na_range <- seq(min(na_range), max(na_range), 1)
  }
  na_all <- unique(c(na_vals, na_range))

  if(is.null(na_all)){
    return(x)
  }

  labels <- attr(x, "labels", T)
  na_all <- c(na_all, names(labels)[labels %in% na_all])

  if(remove_missing_labels && !is.null(labels)){
    labels <- labels[!match(labels, na_all, nomatch = F) > 0]
    if(length(labels) < 1){
      labels <- NULL
    }
  }

  # nas_ident <- .i_find_in(x, na_all)
  nas_ident <- x %in% na_all
  x[nas_ident] <- NA
  structure(
    x,
    labels = labels
  )
}


#' @export
i_missing_to_na.data.frame <- function(x, remove_missing_labels = F){
  x[] <- lapply(x, function(y) i_missing_to_na(y, remove_missing_labels))
  x
}


.valid_na_values <- function(x){
  if(is.null(x)){
    T
  }else if(is.character(x)){
    !any(is.na(x))
  }else if(is.numeric(x)){
    !any(is.na(x)) && !any(x%%1 > 0)
  }else{
    F
  }
}


.valid_na_range <- function(x){
  if(is.null(x)){
    T
  }else if(is.character(x)){
    (!any(is.na(x)) && length(x) <= 2)
  }else if(is.numeric(x)){
    (!any(is.na(x)) && length(x) <= 2) && !any(x%%1 > 0)
  }else{
    F
  }
}


#' validate missing values/range - intern
#' @param x vector
.valid_missing <- function(x){
  is.atomic(x) && !any(is.na(x))
}
