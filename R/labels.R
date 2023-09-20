

#' set value labels
#' @export
#' @param x vector
#' @param ... set labels for values (e.g. label_of_choice = 1 or "Label of Choice" = 1); remove single label with NULL = value (e.g. NULL = 1); removes all value labels when only NULL (e.g. i_label(x, NULL))
#' @param sort_desc sort value labels in descending order according to values
i_labels <- function(x, ..., sort_desc = F){
  old_labs <- attr(x, "labels", T)
  new_labs <- list(...)
  if(!length(new_labs)){
    new_labs <- NULL
  }
  if(length(new_labs) == 1 && is.null(new_labs[[1]])){
    new_labs <- NULL
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
#' @param old_labs named vector or named list
#' @param new_labs named vector or named list
#' @param sort_desc sort value labels in descending order according to values
.merge_labels <- function(old_labs, new_labs, sort_desc = F){
  stopifnot(.valid_labels(old_labs) && .valid_labels(new_labs))
  stopifnot(class(old_labs) != class(new_labs))
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
  }else if(is.list(x)){
    length(names(x)) == length(x) &&
      length(unique(unlist(lapply(x, function(x) class(x))))) == 1 &&
      length(x) > 0
  }else{
    length(names(x)) == length(x) && length(x) > 0
  }
}


#' validate value labels
#' @export
#' @description
#' returns boolean
#'
#' @param x vector
i_valid_labels <- function(x){
  y <- attr(x, "labels", T)
  !is.null(y) && .valid_labels(y)
}

