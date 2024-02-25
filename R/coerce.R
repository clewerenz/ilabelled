
#' @export
as.character.i_labelled <- function(x, ...){
  if(is.null(attr(x, "labels", T))){
    as.character(unclass(x))
  }else{
    .Call("asCharILabelled", x, PACKAGE = "ilabelled")
  }
}


#' @export
as.i_labelled <- function(x, ...){
  keepAttr <- setdiff(names(attributes(x)), names(list(...)))
  if(length(keepAttr) > 0){
    attributes(x) <- attributes(x)[keepAttr]
  }else{
    attributes(x) <- NULL
  }
  i_labelled(x, ...)
}


#' as factor
#' @description
#' make factor from i_labelled
#'
#' @param x vector
#' @param missing_to_na as missing declared values will become NA
#' @param remove_missing_labels as missing declared values will be removed from levels (ignored when 'missing_to_na = F')
#' @param require_all_labels process will be interrupted, when not all values have valid labels
#' @param only_labelled convert only variables with valid value labels to factor
#' @param keep_attributes should attributes be preserved
#' @export
i_as_factor <- function(x, missing_to_na = F, remove_missing_labels = F, require_all_labels = F, only_labelled = F, keep_attributes = F){
  UseMethod("i_as_factor")
}

#' @export
i_as_factor.default <- function(x, missing_to_na = F, remove_missing_labels = F, require_all_labels = F, only_labelled = F, keep_attributes = F){
  stopifnot(is.atomic(x) || is.null(x))

  labels <- attr(x, "labels", T)

  .valid_labels(labels)

  if(only_labelled & is.null(labels)){
    return(x)
  }

  if(missing_to_na){
    x <- i_missing_to_na(x)
  }

  if(remove_missing_labels | missing_to_na){
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

  x <- factor(unclass(x), levels = unname(labels), labels = names(labels))

  if(keep_attributes){
    attributes(x) <- c(attributes(x), tmp_attr)
  }

  x
}

#' @export
i_as_factor.factor <- function(x, ...){
  # do nothing
  x
}

#' @export
i_as_factor.data.frame <- function(x, missing_to_na = F, remove_missing_labels = F, require_all_labels = F, only_labelled = F, keep_attributes = F){
  x[] <- lapply(x, function(y){
    i_as_factor(
      y, missing_to_na = missing_to_na, remove_missing_labels = remove_missing_labels,
      require_all_labels = require_all_labels, only_labelled = only_labelled, keep_attributes = keep_attributes
    )
  })
  x
}


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