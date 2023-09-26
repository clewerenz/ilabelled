

#' data to character
#' @export
#' @description
#' changes the data within class i_labelled
#'
#' will also change base R classes, since it is only a wrapper for keeping attributes
#'
#' @param x vector
i_data_to_character <- function(x){
  tmp_attr <- attributes(x)
  x <- as.character(x)
  attributes(x) <- tmp_attr
  x
}


#' data to numeric
#' @export
#' @description
#' changes the data within class i_labelled
#'
#' will also change base R classes, since it is only a wrapper for keeping attributes
#'
#' @param x vector
i_data_to_numeric <- function(x){
  tmp_attr <- attributes(x)
  x <- as.numeric(x)
  attributes(x) <- tmp_attr
  x
}


#' data to double
#' @export
#' @description
#' changes the data within class i_labelled
#'
#' will also change base R classes, since it is only a wrapper for keeping attributes
#'
#' @param x vector
i_data_to_double <- function(x){
  tmp_attr <- attributes(x)
  x <- as.double(x)
  attributes(x) <- tmp_attr
  x
}


#' data to integer
#' @export
#' @description
#' changes the data within class i_labelled
#'
#' will also change base R classes, since it is only a wrapper for keeping attributes
#'
#' @param x vector
i_data_to_integer <- function(x){
  tmp_attr <- attributes(x)
  x <- as.integer(x)
  attributes(x) <- tmp_attr
  x
}


#' as factor
#' @description
#' make factor from i_labelled
#'
#' @param x vector
#' @param labels value labels as named vector (e.g. c("A"=1, "B"=2) or setNames(c(1,2), c("A","B")))
#' @param missing_to_na as missing declared values will become NA
#' @param remove_missing_labels as missing declared values will be removed from levels
#' @param require_all_labels process will be interrupted, when not all values have valid labels
#' @param ordered order labels by the underlying values
#' @export
i_as_factor <- function(x, labels = NULL, require_all_labels = T, missing_to_na = F, remove_missing_labels = F, ordered = F){
  UseMethod("i_as_factor")
}


#' @export
i_as_factor.default <- function(x, labels = NULL, require_all_labels = T, missing_to_na = F, remove_missing_labels = F, ordered = F){
  stopifnot(is.atomic(x) && !is.null(x))
  if(is.factor(x)){
    if(is.null(labels)){
      labels <- levels(x)
    }
    x <- as.character(x)
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
  if(ordered){
    labels <- sort(labels)
  }
  x <- factor(x, levels = unname(labels), labels = names(labels))
  attributes(x) <- c(attributes(x), tmp_attr)
  x
}


#' @export
i_as_factor.data.frame <- function(x, labels = NULL, require_all_labels = T, missing_to_na = F, remove_missing_labels = F, ordered = F){
  x[] <- lapply(x, function(y){
    i_as_factor(
      y, missing_to_na = missing_to_na, remove_missing_labels = remove_missing_labels,
      require_all_labels = require_all_labels, ordered = ordered
    )
  })
  x
}

