#' @importFrom methods setOldClass
methods::setOldClass("i_labelled")


#' class constructor
#' @export
#' @returns vector or data.frame
#' @param x vector or data.frame
#' @param label variable label
#' @param labels value labels as named vector (e.g. c("A"=1, "B"=2) or setNames(c(1,2), c("A","B")))
#' @param na_values missing values (e.g. c(888, 999))
#' @param na_range range of missing values as vector length 2 (e.g. c(-9,-1))
#' @param scale scale level (nominal, ordinal, scale)
#' @param annotation additional information about variable
#' @param wording question text
#' @param subject subject
#' @param ... further attributes passed to class
#' @importFrom stats setNames
i_labelled <- function(x, label = NULL, labels = NULL, na_values = NULL, na_range = NULL, scale = NULL, annotation = NULL, wording = NULL, subject = NULL,...){
  UseMethod("i_labelled")
}


#' @export
i_labelled.default <- function(x, label = NULL, labels = NULL, na_values = NULL, na_range = NULL, scale = NULL, annotation = NULL, wording = NULL, subject = NULL, ...){
  if(!is.atomic(x)){
    stop("x must be vector")
  }

  if(is.logical(labels)){
    labels <- stats::setNames(as.numeric(labels), names(labels))
  }

  stopifnot(.valid_label(label))
  stopifnot(.valid_na_values(na_values))
  stopifnot(.valid_na_range(na_range))

  if(is.null(labels) && is.factor(x)){
    labels <- stats::setNames(1:length(levels(x)), levels(x))
  }else if(!is.null(labels) || !is.null(attr(x, "labels", TRUE))){
    labels <- .merge_labels(as.list(attr(x, "labels", TRUE)), as.list(labels))
  }

  if(!is.numeric(x) && !is.logical(x) && !is.null(labels) && !is.character(labels)){
    stop("Cannot apply non-character value labels to non-numeric vector. Value labels must be character.")
  }else if(is.numeric(x) && !is.null(labels) && !is.numeric(labels)){
    stop("Cannot apply non-numeric value labels to numeric vector. Value labels must be numeric.")
  }

  if(!is.null(na_values)){
    if(!is.numeric(x) && !is.logical(x) && !is.character(na_values)){
      stop("Cannot apply non-character na_values to non-numeric vector. Value na_values must be character.")
    }else if(is.numeric(x) && !is.numeric(na_values)){
      stop("Cannot apply non-numeric na_values to numeric vector. na_values must be numeric.")
    }
  }

  if(!is.null(na_range)){
    if(!is.numeric(x) && !is.logical(x) && !is.character(na_range)){
      stop("Cannot na_range to non-numeric vector.")
    }else if(is.numeric(x) && !is.numeric(na_range)){
      stop("Cannot apply non-numeric na_range to numeric vector. na_range must be numeric.")
    }
  }

  if(!is.null(scale)){
    scale <- tolower(scale)
    if(!.valid_scale(scale)){
      stop("scale must be character vector of length 1")
    }
    if(!scale %in% c("nominal", "ordinal", "scale")){
      stop("scale must be either 'nominal', 'ordinal' or 'scale'")
    }
  }

  if(!is.null(labels)){
    labels <- labels[order(labels, decreasing = FALSE)]
  }

  if(!is.null(annotation) && !.valid_annotation(annotation)){
    stop("invalid annotation")
  }

  if(!is.null(wording) && !.valid_wording(wording)){
    stop("wording must be character vector of length 1")
  }

  if(!is.null(subject) && !.valid_subject(subject)){
    stop("subject must be character vector of length 1")
  }

  return(.init(x, label = label, labels = labels, na_values = na_values, na_range = na_range, scale = scale, annotation = annotation, wording = wording, subject = subject, ...))
}


#' @export
i_labelled.factor <- function(x, label = NULL, labels = NULL, na_values = NULL, na_range = NULL, scale = NULL, annotation = NULL, wording = NULL, subject = NULL, ...){
  if(!is.atomic(x)){
    stop("x must be vector")
  }

  stopifnot(.valid_label(label))
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

  if(!is.null(na_values)){
    if(!is.numeric(x) && !is.factor(x) && !is.logical(x) && !is.character(na_values)){
      stop("Cannot apply non-character na_values to non-numeric vector. Value na_values must be character.")
    }else if((is.numeric(x) || is.factor(x)) && !is.numeric(na_values)){
      stop("Cannot apply non-numeric na_values to numeric vector. na_values must be numeric.")
    }
  }

  if(!is.null(na_range)){
    if(!is.numeric(x) && !is.factor(x) && !is.logical(x) && !is.character(na_range)){
      stop("Cannot na_range to non-numeric vector.")
    }else if((is.numeric(x) || is.factor(x)) && !is.numeric(na_range)){
      stop("Cannot apply non-numeric na_range to numeric vector. na_range must be numeric.")
    }
  }

  if(!is.null(scale)){
    scale <- tolower(scale)
    if(!.valid_scale(scale)){
      stop("scale must be character vector of length 1")
    }
    if(!scale %in% c("nominal", "ordinal", "scale")){
      stop("scale must be either 'nominal', 'ordinal' or 'scale'")
    }
  }

  if(!is.null(labels)){
    labels <- labels[order(labels, decreasing = FALSE)]
  }

  if(!is.null(annotation) && !.valid_annotation(annotation)){
    stop("invalid annotation")
  }

  if(!is.null(wording) && !.valid_wording(wording)){
    stop("invalid wording")
  }

  if(!is.null(subject) && !.valid_subject(subject)){
    stop("invalid subject")
  }

  return(.init(x, label = label, labels = labels, na_values = na_values, na_range = na_range, scale = scale, wording = wording, subject = subject, ...))
}


#' @export
i_labelled.data.frame <- function(x, label = NULL, labels = NULL, na_values = NULL, na_range = NULL, scale = NULL, annotation = NULL, wording = NULL, subject = NULL, ...){
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


#' unclass variables
#' @returns x unclassed
#' @param x vector or data.frame
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
  x[] <- lapply(x, function(x) i_unclass(x, keep_attributes = keep_attributes))
  x
}


