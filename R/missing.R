

#' define missing values
#' @description
#' define which values will be handled as missing values
#'
#' @returns Returns x with missing values set
#' @param x vector
#' @param values vector with missing values e.g. c(888,999) or NULL (NULL will remove all missing values)
#' @param sort sort values
#' @param desc sort values in descending order
#' @export
i_na_values <- function(x, values, sort = TRUE, desc = FALSE){
  UseMethod("i_na_values")
}


#' @export
i_na_values.default <- function(x, values, sort = TRUE, desc = FALSE){
  if(!.valid_na_values(values)) stop("invalid na_values")
  if(!is.null(values)){
    if(!is.numeric(x) && !is.factor(x) && !is.logical(x) && !is.character(values)){
      stop("Cannot apply non-character na_values to non-numeric vector. Value na_values must be character.")
    }else if((is.numeric(x) || is.factor(x)) && !is.numeric(values)){
      stop("Cannot apply non-numeric na_values to numeric vector. na_values must be numeric.")
    }
  }
  if(sort){
    values <- sort(values, desc)
  }
  structure(
    x,
    na_values = values
  )
}


#' @export
i_na_values.data.frame <- function(x, values, sort = TRUE, desc = FALSE){
  x[] <- lapply(x, function(y) i_na_values(y, values, sort, desc))
  x
}


#' define missing range
#' @description
#' define which values will be handled as missing values
#'
#' @returns Returns x with missing range set
#' @param x vector
#' @param values vector with missing range e.g. c(-9:-1) or NULL (NULL will remove all missing values)
#' @export
i_na_range <- function(x, values){
  UseMethod("i_na_range")
}


#' @export
i_na_range.default <- function(x, values){
  if(!.valid_na_range(values)) stop("invalid na_range")
  if(!is.null(values)){
    if(!is.numeric(x) && !is.factor(x) && !is.logical(x) && !is.character(values)){
      stop("Cannot na_range to non-numeric vector.")
    }else if((is.numeric(x) || is.factor(x)) && !is.numeric(values)){
      stop("Cannot apply non-numeric na_range to numeric vector. na_range must be numeric.")
    }
  }
  structure(
    x,
    na_range = values
  )
}


#' @export
i_na_range.data.frame <- function(x, values){
  x[] <- lapply(x, function(y) i_na_range(y, values))
  x
}


#' missing values to NA
#' @description
#' all values declared as missing will be recoded as NA
#'
#' @returns Returns x with missing values coerced to NA
#' @param x vector or data.frame
#' @param remove_missing_labels remove values labels from values which are declared as missing
#' @export
i_missing_to_na <- function(x, remove_missing_labels = FALSE){
  UseMethod("i_missing_to_na")
}


#' @export
i_missing_to_na.default <- function(x, remove_missing_labels = FALSE){
  stopifnot(is.atomic(x) || is.null(x))

  na_vals <- c(attr(x, "na_values", TRUE), names(attr(x, "na_values", TRUE)))
  na_range <- sort(attr(x, "na_range", TRUE))
  stopifnot(.valid_na_values(na_vals) || .valid_na_range(na_range))
  if(!is.null(na_range)){
    na_range <- seq(min(na_range), max(na_range), 1)
  }
  na_all <- unique(c(na_vals, na_range))

  if(is.null(na_all)){
    return(x)
  }

  labels <- attr(x, "labels", TRUE)
  na_all <- c(na_all, names(labels)[labels %in% na_all])

  if(remove_missing_labels && !is.null(labels)){
    labels <- labels[!match(labels, na_all, nomatch = FALSE) > 0]
    if(length(labels) < 1){
      labels <- NULL
    }
  }

  if(!is.numeric(x)){
    isnaval <- as.character(x) %in% na_all
  }else{
    isnaval <- x %in% na_all
  }

  if(length(na_range) > 0){
    isnarange <- x >= min(na_range) & x <= max(na_range)
    x[isnaval | isnarange] <- NA
  }else{
    x[isnaval] <- NA
  }

  structure(
    x,
    labels = labels
  )
}


#' @export
i_missing_to_na.data.frame <- function(x, remove_missing_labels = FALSE){
  x[] <- lapply(x, function(y) i_missing_to_na(y, remove_missing_labels))
  x
}


.valid_na_values <- function(x){
  if(is.null(x)){
    TRUE
  }else if(is.character(x)){
    !any(is.na(x))
  }else if(is.numeric(x)){
    !any(is.na(x)) && !any(x %% 1 > 0)
  }else{
    FALSE
  }
}


.valid_na_range <- function(x){
  if(is.null(x)){
    TRUE
  }else if(is.character(x)){
    (!any(is.na(x)) && length(x) <= 2)
  }else if(is.numeric(x)){
    (!any(is.na(x)) && length(x) <= 2) #  && !any(x %% 1 > 0)
  }else{
    FALSE
  }
}


#' validate missing values/range - intern
#' @returns T/F
#' @param x vector
.valid_missing <- function(x){
  is.atomic(x) && !any(is.na(x))
}
