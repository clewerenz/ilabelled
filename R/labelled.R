#' @importFrom methods setOldClass
methods::setOldClass("i_labelled")


#' class constructor
#' @export
#' @returns x as i_labelled object
#' @param x vector
#' @param label variable label
#' @param labels value labels as named vector (e.g. c("A"=1, "B"=2) or setNames(c(1,2), c("A","B")))
#' @param na_values missing values (e.g. c(888, 999))
#' @param na_range range of missing values as vector length 2 (e.g. c(-9,-1))
#' @param scale scale level (nominal, ordinal, scale)
#' @param ... further attributes passed to class
i_labelled <- function(x, label = NULL, labels = NULL, na_values = NULL, na_range = NULL, scale = NULL, ...){
  UseMethod("i_labelled")
}


#' class constructor
#' @export
#' @returns x as i_labelled object
#' @param x vector
#' @param label variable label
#' @param labels value labels as named vector (e.g. c("A"=1, "B"=2) or setNames(c(1,2), c("A","B")))
#' @param na_values missing values (e.g. c(888, 999))
#' @param na_range range of missing values as vector length 2 (e.g. c(-9,-1))
#' @param scale scale level (nominal, ordinal, scale)
#' @importFrom stats setNames
#' @param ... further attributes passed to class
i_labelled.default <- function(x, label = NULL, labels = NULL, na_values = NULL, na_range = NULL, scale = NULL, ...){
  if(!is.atomic(x)){
    stop("x must be vector")
  }

  .valid_label(label)
  stopifnot(.valid_na_values(na_values))
  stopifnot(.valid_na_range(na_range))

  if(is.null(labels) && is.factor(x)){
    labels <- stats::setNames(1:length(levels(x)), levels(x))
  }else if(!is.null(labels) || !is.null(attr(x, "labels", TRUE))){
    labels <- .merge_labels(as.list(attr(x, "labels", TRUE)), as.list(labels))
  }

  if(is.numeric(x) && !is.null(labels) && is_decimal(x)){
    stop("decimal numbers cannot be labelled")
  }

  if(!is.numeric(x) && !is.null(labels) && !is.character(labels)){
    stop("Cannot apply non-character value labels to non-numeric vector. Value labels must be character.")
  }else if(is.numeric(x) && !is.null(labels) && !is.numeric(labels)){
    stop("Cannot apply non-numeric value labels to numeric vector. Value labels must be numeric.")
  }

  if(!is.null(scale)){
    scale <- tolower(scale)
    .valid_scale(scale)
  }

  return(.init(x, label = label, labels = labels, na_values = na_values, na_range = na_range, scale = scale, ...))
}


#' class constructor
#' @export
#' @returns x as i_labelled object
#' @param x vector
#' @param label variable label
#' @param labels value labels as named vector (e.g. c("A"=1, "B"=2) or setNames(c(1,2), c("A","B")))
#' @param na_values missing values (e.g. c(888, 999))
#' @param na_range range of missing values as vector length 2 (e.g. c(-9,-1))
#' @param scale scale level (nominal, ordinal, scale)
#' @importFrom stats setNames
#' @param ... further attributes passed to class
i_labelled.factor <- function(x, label = NULL, labels = NULL, na_values = NULL, na_range = NULL, scale = NULL, ...){
  if(!is.atomic(x)){
    stop("x must be vector")
  }

  .valid_label(label)
  stopifnot(.valid_na_values(na_values))
  stopifnot(.valid_na_range(na_range))

  if(is.null(labels) && is.factor(x)){
    labels <- stats::setNames(1:length(levels(x)), levels(x))
  }
  if(!is.null(labels) || !is.null(attr(x, "labels", TRUE))){
    labels <- .merge_labels(as.list(attr(x, "labels", TRUE)), as.list(labels))
  }

  if(!is.null(labels) && !is.numeric(labels)){
    stop("Cannot apply non-numeric value labels to factor. Value labels must be numeric.")
  }

  if(!is.null(scale)){
    scale <- tolower(scale)
    .valid_scale(scale)
  }

  return(.init(x, label = label, labels = labels, na_values = na_values, na_range = na_range, scale = scale, ...))
}


#' make all variables in data.frame i_labelled (dedicated function)
#' @export
#' @returns x with all variables coerced to i_labelled object
#' @param x data.frame
i_labelled_df <- function(x){
  x[] <- lapply(x, i_labelled)
  x
}


#' check for class i_labelled
#' @returns T/F
#' @param x vector of class i_labelled
#' @importFrom methods is
#' @export
is.i_labelled <- function(x){
  methods::is(x,'i_labelled')
}


#' subsetting vectors of class i_labelled
#' @returns Subset of x
#' @export
#' @param x vector of class i_labelled
#' @param ... not used
`[.i_labelled` <- function(x, ...){
  r <- NextMethod("[")
  mostattributes(r) <- attributes(x)
  r
}

# old version using vctrs package
# `[.i_labelled` <- function(x, ...){
#   vctrs::vec_restore(NextMethod("["), x)
# }


#' custom unclass function
#' @returns x unclassed
#' @param x vector of class i_labelled
#' @param keep_attributes should attributes be preserved
#' @export
i_unclass <- function(x, keep_attributes = FALSE){
  UseMethod("i_unclass")
}


#' @export
i_unclass.default <- function(x, keep_attributes = FALSE){
  tmp_attr <- attributes(x)[!names(attributes(x)) %in% c("class", "levels")]
  x <- unclass(`attributes<-`(x, NULL))
  if(keep_attributes){
    attributes(x) <- c(attributes(x), tmp_attr)
  }
  x
}


#' @export
i_unclass.data.frame <- function(x, keep_attributes = FALSE){
  x[] <- lapply(x, i_unclass)
}



