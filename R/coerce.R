
# as.character.i_labelled <- function(x, ...){
#   if(is.null(attr(x, "labels", TRUE))){
#     as.character(unclass(x))
#   }else{
#     .Call("asCharILabelled", x, PACKAGE = "ilabelled")
#   }
# }

#' as character
#' @description
#' make character from i_labelled
#'
#' @returns Character vector
#' @param x vector
#' @param missing_to_na as missing declared values will become NA
#' @param require_all_labels process will be interrupted, when not all values have valid labels
#' @param keep_attributes should attributes be preserved
#' @importFrom stats setNames
#' @export
i_as_character <- function(x, missing_to_na = FALSE, require_all_labels = FALSE, keep_attributes = FALSE){
  UseMethod("i_as_character")
}

#' @export
i_as_character.default <- function(x, missing_to_na = FALSE, require_all_labels = FALSE, keep_attributes = FALSE){
  stopifnot(is.atomic(x) || is.null(x))

  labels <- attr(x, "labels", TRUE)

  if(is.null(labels)){
    return(as.character(unclass(x)))
  }

  .valid_labels(labels)

  if(missing_to_na){
    x <- i_missing_to_na(x)
    x <- i_remove_missing_labels(x)
    labels <- attr(x, "labels", TRUE)
  }

  missing_values <- unique(unclass(x[!match(unclass(x), labels, nomatch = FALSE) > 0]))
  missing_values <- missing_values[!is.na(missing_values)]

  if(require_all_labels && length(missing_values) > 0){
    stop("missing value labels")
  }
  if(any(duplicated(names(labels)))){
    stop("cannot convert to character: duplicate labels in value labels")
  }
  if(any(duplicated(labels))){
    stop("cannot convert to character: duplicate values in value labels")
  }

  tmp_attr <- attributes(x)[!names(attributes(x)) %in% c("class", "levels")]

  if(keep_attributes){
    x <- .Call("asCharILabelled", x, PACKAGE = "ilabelled")
    attributes(x) <- tmp_attr
    x
  }else{
    .Call("asCharILabelled", x, PACKAGE = "ilabelled")
  }
}

# i_as_character.character <- function(x, missing_to_na = FALSE, require_all_labels = FALSE, keep_attributes = FALSE){
#   # do nothing
#   x
# }


#' coerce to i_labelled class
#' @returns Vector of class i_labelled
#' @param x vector
#' @param ... attributes passed to class
#' @export
as.i_labelled <- function(x, ...){
  arguments <- list(...)
  keepAttr <- setdiff(names(attributes(x)), names(list(...)))
  if(length(keepAttr) > 0){
    attributes(x) <- attributes(x)[keepAttr]
  }else{
    attributes(x) <- NULL
  }
  for(i in c("label", "labels", "scale")){
    arguments[[i]] <- attr(x, i, TRUE)
  }
  i_labelled(x, label = eval(arguments[["label"]]), na_values = eval(arguments[["na_values"]]), eval(arguments[["na_range"]]), scale = eval(arguments[["scale"]]), ...)
}


#' as factor
#' @description
#' make factor from i_labelled
#'
#' @returns Vector of class factor
#' @param x vector
#' @param missing_to_na as missing declared values will become NA
#' @param require_all_labels process will be interrupted, when not all values have valid labels
#' @param keep_attributes should attributes be preserved
#' @importFrom stats setNames
#' @export
i_as_factor <- function(x, missing_to_na = FALSE, require_all_labels = FALSE, keep_attributes = FALSE){
  UseMethod("i_as_factor")
}

#' @export
i_as_factor.default <- function(x, missing_to_na = FALSE, require_all_labels = FALSE, keep_attributes = FALSE){
  stopifnot(is.atomic(x) || is.null(x))

  if(!is.i_labelled(x)){
    x <- i_labelled(x)
  }

  labels <- attr(x, "labels", TRUE)
  na_vals <- c(attr(x, "na_values", TRUE))

  .valid_labels(labels)

  if(missing_to_na){
    x <- i_missing_to_na(x, remove_missing_labels = TRUE)
    x <- i_remove_missing_labels(x)
    labels <- attr(x, "labels", TRUE)
  }

  missing_values <- unique(unclass(x[!match(unclass(x), labels, nomatch = FALSE) > 0]))
  missing_values <- missing_values[!is.na(missing_values)]

  if(require_all_labels && length(missing_values) > 0){
    stop("missing value labels")
  }else if(length(missing_values) > 0){
    labels <- c(labels, stats::setNames(missing_values, missing_values))
  }
  if(any(duplicated(names(labels)))){
    stop("cannot convert to factor: duplicate labels in value labels")
  }
  if(any(duplicated(labels))){
    stop("cannot convert to factor: duplicate values in value labels")
  }

  labels <- names(sort(labels))

  tmp_attr <- attributes(x)[!names(attributes(x)) %in% c("class", "levels")]

  x <- i_as_character(x)
  x <- match(x, labels)
  if(keep_attributes){
    attributes(x) <- tmp_attr
  }
  attr(x, "levels") <- labels
  class(x) <- "factor"

  x
}

#' @export
i_as_factor.factor <- function(x, ...){
  # do nothing
  x
}


#' remove class i_labelled and return base R class
#' @description
#' - when value labels for all values are available will return factor
#'
#' - when value labels are missing will unclass i_labelled
#'
#' - remove class i_labelled and return variable as base R class
#'
#' @returns Returns x coerced to R base class
#' @param x vector or data.frame
#' @param missing_to_na missing values will become regular NA
#' @param as_factor convert to factor when value labels are available
#' @param keep_attributes should attributes be preserved
#' @export
i_to_base_class <- function(x, missing_to_na = TRUE, as_factor = TRUE, keep_attributes = FALSE){
  UseMethod("i_to_base_class")
}

#' @export
i_to_base_class.default <- function(x, missing_to_na = TRUE, as_factor = TRUE, keep_attributes = FALSE){
  stopifnot(is.atomic(x))
  stopifnot(is.logical(as_factor) && length(as_factor) == 1)
  stopifnot(is.logical(keep_attributes) && length(keep_attributes) == 1)
  stopifnot(is.logical(missing_to_na) && length(missing_to_na) == 1)

  if(!is.i_labelled(x)){
    return(x)
  }

  if(missing_to_na){
    x <- i_missing_to_na(x, remove_missing_labels = TRUE)
  }

  labels <- attr(x, "labels", TRUE)
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
i_to_base_class.data.frame <- function(x, missing_to_na = TRUE, as_factor = TRUE, keep_attributes = FALSE){
  x[] <- lapply(x, function(y) i_to_base_class(y, missing_to_na = missing_to_na, as_factor = as_factor, keep_attributes = keep_attributes))
  x
}
