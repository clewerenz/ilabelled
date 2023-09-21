

#' set value labels
#' @export
#' @param x vector
#' @param ... set labels for values (e.g. label_of_choice = 1 or "Label of Choice" = 1); remove single label with NULL = value (e.g. NULL = 1); removes all value labels when only NULL (e.g. i_label(x, NULL))
#' @param sort_desc sort value labels in descending order according to values
i_labels <- function(x, ..., sort_desc = F){
  if(!is.null(attr(x, "labels", T))){
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
    all_labs <- .merge_labels(old_labs, new_labs, sort_desc)
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
#' @param sort_desc sort value labels in descending order according to values
.merge_labels <- function(old_labs, new_labs, sort_desc = F){
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
  all_labs <- sort(all_labs, decreasing = sort_desc)
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
      length(x) > 0
  }else{
    length(names(x)) == length(x) && length(x) > 0
  }
}


#' validate value labels
#' @description
#' returns boolean when i_labelled
#' returns NA when not i_labelled
#' returns a named list when applied to data.frame
#'
#' @param x vector ot data.frame
#' @export
i_valid_labels <- function(x){
  UseMethod("i_valid_labels")
}


#' @export
i_valid_labels.default <- function(x){
  NA
}


#' @export
i_valid_labels.i_labelled <- function(x){
  y <- attr(x, "labels", T)
  (!is.null(y) && .valid_labels(y)) &&
    all(unique(x[!is.na(x)]) %in% y)
}


#' @export
i_valid_labels.data.frame <- function(x){
  sapply(x, i_valid_labels, USE.NAMES = T, simplify = F)
}
