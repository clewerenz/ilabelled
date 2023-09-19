

#' set value labels
#' @export
#' @param x vector
#' @param ... set labels for values (e.g. label_of_choice = 1 or "Label of Choice" = 1); remove single label with NULL = value (e.g. NULL = 1); removes all value labels when only NULL (e.g. i_label(x, NULL))
#' @param sort_desc sort value labels in descending order according to values
i_labels <- function(x, ..., sort_desc = F){
  new_labs <- list(...)
  stopifnot(.valid_labels(new_labs) || is.null(new_labs) || is.null(unlist(new_labs)) || length(new_labs) < 1)
  if(length(new_labs) == 1 && is.null(new_labs[[1]])){
    all_labs <- NULL
  }else{
    old_labs <- attr(x, "labels", T)
    all_labs <- append(new_labs, old_labs)
    all_labs <- unlist(all_labs)
    all_labs <- all_labs[!duplicated(all_labs)]
    all_labs <- sort(all_labs, decreasing = sort_desc)
    all_labs <- all_labs[!names(all_labs) %in% "NULL"]
    if(length(all_labs) < 1){
      all_labs <- NULL
    }
  }
  structure(
    x,
    labels = all_labs
  )
}


#' validate value labels - intern
#' @description
#' returns boolean
#'
#' @param x named vector (label = value)
.valid_labels <- function(x){
  if(is.list(x)){
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
  .valid_labels(y)
}

