
#' as factor
#' @description
#' make factor from i_labelled
#'
#' @param x vector
#' @param missing_to_na as missing declared values will become NA
#' @param remove_missing_labels as missing declared values will be removed from levels (ignored when 'missing_to_na = F')
#' @param require_all_labels process will be interrupted, when not all values have valid labels
#' @param only_labelled convert only variables with valid value labels to factor
#' @export
i_as_factor <- function(x, missing_to_na = F, remove_missing_labels = F, require_all_labels = F, only_labelled = F){
  UseMethod("i_as_factor")
}


#' @export
i_as_factor.default <- function(x, missing_to_na = F, remove_missing_labels = F, require_all_labels = F, only_labelled = F){
  stopifnot(is.atomic(x) || is.null(x))

  labels <- attr(x, "labels", T)

  stopifnot(.valid_labels(labels))

  if(only_labelled & is.null(labels)){
    return(x)
  }

  if(missing_to_na){
    x <- i_missing_to_na(x)
  }

  if(remove_missing_labels){
    x <- i_remove_missing_labels(x)
    labels <- attr(x, "labels", T)
  }

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

  if(any(duplicated(names(labels)))){
    stop("cannot convert to factor: duplicate labels in value labels")
  }
  if(any(duplicated(labels))){
    stop("cannot convert to factor: duplicate values in value labels")
  }

  labels <- sort(labels)

  tmp_attr <- attributes(x)[!names(attributes(x)) %in% c("class", "levels")]
  x <- factor(x, levels = unname(labels), labels = names(labels))
  attributes(x) <- c(attributes(x), tmp_attr)
  x
}


#' @export
i_as_factor.factor <- function(x, ...){
  # do nothing
  x
}


#' @export
i_as_factor.data.frame <- function(x, missing_to_na = F, remove_missing_labels = F, require_all_labels = F, only_labelled = F){
  x[] <- lapply(x, function(y){
    i_as_factor(
      y, missing_to_na = missing_to_na, remove_missing_labels = remove_missing_labels,
      require_all_labels = require_all_labels, only_labelled = only_labelled
    )
  })
  x
}

