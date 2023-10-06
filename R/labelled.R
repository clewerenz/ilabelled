

#' class constructor
#' @export
#' @param x vector
#' @param label variable label
#' @param labels value labels as named vector (e.g. c("A"=1, "B"=2) or setNames(c(1,2), c("A","B")))
#' @param na_values missing values (e.g. c(888, 999))
#' @param na_range range of missing values as vector length 2 (e.g. c(-9,-1))
#' @param ... further attributes passed to class
i_labelled <- function(x, label = NULL, labels = NULL, na_values = NULL, na_range = NULL, ...){
  if(!is.atomic(x)){
    stop("x must be vector")
  }
  .valid_label(label)
  stopifnot(.valid_na_values(na_values))
  stopifnot(.valid_na_range(na_range))

  if(is.null(labels) && is.factor(x)){
    labels <- stats::setNames(1:length(levels(x)), levels(x))
  }else if(!is.null(labels) || !is.null(attr(x, "labels", T))){
    labels <- .merge_labels(as.list(attr(x, "labels", T)), as.list(labels))
  }

  return(.init(x, label = label, labels = labels, na_values = na_values, na_range = na_range, ...))
}


#' make all variables in data.frame i_labelled
#' @export
#' @param x data.frame
i_labelled_df <- function(x){
  x[] <- lapply(x, i_labelled)
  x
}


#' check for class i_labelled
#' @param x vector of class i_labelled
#' @importFrom methods is
#' @export
is.i_labelled <- function(x){
  methods::is(x,'i_labelled')
}


#' subsetting vectors of class i_labelled
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
#' @param x vector of class i_labelled
#' @param keep_attributes should attributes be preserved
#' @export
i_unclass <- function(x, keep_attributes = F){
  UseMethod("i_unclass")
}


#' @export
i_unclass.default <- function(x, keep_attributes = F){
  tmp_attr <- attributes(x)[!names(attributes(x)) %in% c("class", "levels")]
  x <- unclass(`attributes<-`(x, NULL))
  if(keep_attributes){
    attributes(x) <- c(attributes(x), tmp_attr)
  }
  x
}


#' @export
i_unclass.data.frame <- function(x, keep_attributes = F){
  x[] <- lapply(x, i_unclass)
}
