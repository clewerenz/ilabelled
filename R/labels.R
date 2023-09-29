

#' set value labels
#' @export
#' @param x vector
#' @param ... set labels for values (e.g. label_of_choice = 1 or "Label of Choice" = 1); remove single label with NULL = value (e.g. NULL = 1); removes all value labels when only NULL (e.g. i_label(x, NULL))
#' @param overwrite should new labels be merged with existing labels or remove existing labels
i_labels <- function(x, ..., overwrite = F){
  if(!is.null(attr(x, "labels", T)) & !overwrite){
    old_labs <- as.list(attr(x, "labels", T))
  }else{
    old_labs <- NULL
  }
  new_labs <- list(...)
  if(!length(new_labs)){
    new_labs <- NULL
  }
  if(length(new_labs) == 1 && is.null(new_labs[[1]])){
    all_labs <- NULL
  }else{
    stopifnot(.valid_labels(new_labs))
    all_labs <- .merge_labels(old_labs, new_labs)
  }
  structure(
    x,
    labels = all_labs
  )
}


#' combine old value labels with new value labels
#' @description
#' return named vector
#'
#' @param old_labs named vector
#' @param new_labs named vector
.merge_labels <- function(old_labs, new_labs){
  stopifnot((is.null(old_labs) || is.list(old_labs)) && (is.null(new_labs) || is.list(new_labs)))
  if(length(old_labs) < 1){
    old_labs <- NULL
  }
  if(length(new_labs) < 1){
    new_labs <- NULL
  }
  stopifnot(.valid_labels(old_labs) && .valid_labels(new_labs))
  stopifnot(length(unique(c(lapply(old_labs, is.numeric), lapply(new_labs, is.numeric)))) <= 1)
  all_labs <- append(new_labs, old_labs)
  all_labs <- unlist(all_labs)
  all_labs <- all_labs[!duplicated(all_labs)]
  all_labs <- sort(all_labs)
  all_labs <- all_labs[!names(all_labs) %in% "NULL"]
  if(length(all_labs) < 1){
    all_labs <- NULL
  }
  all_labs
}


#' validate value labels - intern
#' @description
#' returns boolean
#'
#' @param x named vector (label = value)
.valid_labels <- function(x){
  if(is.null(x)){
    T
  }else if(any(is.na(x))){
    F
  }else if(is.list(x)){
    length(names(x)) == length(x) &&
      length(unique(unlist(lapply(x, function(x) class(x))))) == 1 &&
      length(x) > 0 &&
      !any(duplicated(names(x)[!names(x) == "NULL"])) &&
      !any(duplicated(unlist(x)))
  }else{
    length(names(x)) == length(x) && length(x) > 0 &&
      !any(duplicated(names(x)[!names(x) == "NULL"])) &&
      !any(duplicated(x))
  }
}


#' validate value labels
#' @description
#' returns boolean when i_labelled
#' returns NA when not i_labelled
#' returns a named list when applied to data.frame
#'
#' @param x vector or data.frame
#' @export
i_valid_labels <- function(x){
  UseMethod("i_valid_labels")
}


#' @export
i_valid_labels.default <- function(x){
  y <- attr(x, "labels", T)
  (!is.null(y) && .valid_labels(y)) &&
    all(unique(x[!is.na(x)]) %in% y)
}


#' @export
i_valid_labels.data.frame <- function(x){
  sapply(x, i_valid_labels, USE.NAMES = T, simplify = F)
}


#' sort value labels by values or by labels
#' @param x vector or data.frame
#' @param by either values or labels
#' @param decreasing sort decreasing
#' @export
i_sort_labels <- function(x, by = "values", decreasing = F){
  UseMethod("i_sort_labels")
}


#' @export
i_sort_labels.default <- function(x, by = "values", decreasing = F){
  stopifnot(is.atomic(x) & length(x) > 0)
  stopifnot(is.atomic(by) & length(by) == 1)
  stopifnot(is.logical(decreasing) & length(decreasing) == 1)
  by <- tolower(by)
  if(!by %in% c("values", "labels")){
    stop("'by' must be either 'values' or 'labels'")
  }
  labels <- attr(x, "labels", T)
  if(!is.null(labels) && .valid_labels(labels)){
    if(by == "values"){
      labels <- labels[order(labels, decreasing = decreasing)]
    }else{
      labels <- labels[order(names(labels), decreasing = decreasing)]
    }
  }
  structure(
    x,
    labels = labels
  )
}


#' @export
i_sort_labels.data.frame <- function(x, by = "values", decreasing = F){
  x[] <- lapply(x, function(y) i_sort_labels(y, by = by, decreasing = decreasing))
  x
}

