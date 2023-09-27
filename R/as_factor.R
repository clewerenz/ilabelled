
#' as factor
#' @description
#' make factor from i_labelled
#'
#' @param x vector
#' @param labels value labels as named vector (e.g. c("A"=1, "B"=2) or setNames(c(1,2), c("A","B")))
#' @param missing_to_na as missing declared values will become NA
#' @param remove_missing_labels as missing declared values will be removed from levels (ignored when 'missing_to_na = F')
#' @param require_all_labels process will be interrupted, when not all values have valid labels
#' @param ordered order labels by the underlying values
#' @param only_labelled only variables of class 'i_labelled' will become factor
#' @export
i_as_factor <- function(x, labels = NULL, require_all_labels = F, missing_to_na = F, remove_missing_labels = F, ordered = T, only_labelled = F){
  UseMethod("i_as_factor")
}


#' @export
i_as_factor.default <- function(x, labels = NULL, require_all_labels = F, missing_to_na = F, remove_missing_labels = F, ordered = T, only_labelled = F){
  stopifnot(is.atomic(x))
  if(only_labelled && !is.i_labelled(x)){
    return(x)
  }

  if(missing_to_na){
    x <- i_missing_to_na(x, remove_missing_labels = remove_missing_labels)
  }

  if(is.null(labels)){
    labels <- attr(x, "labels", T)
  }
  stopifnot(.valid_labels(labels))

  tmp_attr <- attributes(x)[!names(attributes(x)) %in% c("class", "levels", "labels")]

  variable_values <- unique(x)
  variable_values <- variable_values[!is.na(variable_values)]
  labels_values <- unique(labels)
  labels_values <- labels_values[!is.na(labels_values)]
  missing_values <- variable_values[!variable_values %in% labels_values]

  if(require_all_labels && length(missing_values) > 0){
    stop("missing or invalid value labels")
  }else if(length(missing_values) > 0){
    labels <- c(labels, stats::setNames(missing_values, missing_values))
  }

  # if(ordered){
  labels <- sort(labels)
  # }

  x <- factor(x, levels = unname(labels), labels = names(labels))
  attributes(x) <- c(attributes(x), tmp_attr)
  x
}


#' @export
i_as_factor.data.frame <- function(x, labels = NULL, require_all_labels = F, missing_to_na = F, remove_missing_labels = F, ordered = T, only_labelled = T){
  x[] <- lapply(x, function(y){
    i_as_factor(
      y, missing_to_na = missing_to_na, remove_missing_labels = remove_missing_labels,
      require_all_labels = require_all_labels, ordered = ordered, only_labelled = only_labelled
    )
  })
  x
}


#' @export
i_as_factor.factor <- function(x, ...){
  # do nothing
  x
}
