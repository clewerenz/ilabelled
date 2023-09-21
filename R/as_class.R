

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
#' @export
#' @description
#' make factor from i_labelled
#'
#' @param x vector
#' @param labels value labels as named vector or named list (e.g. list("A"=1, "B"=2) or c("A"=1, "B"=2) or setNames(c(1,2), c("A","B")))
i_as_factor <- function(x, labels = NULL){
  stopifnot(is.atomic(x) && !is.null(x))
  if(is.null(labels)){
    val_labs <- attr(x, "labels", T)
  }else{
    stopifnot(.valid_labels(labels))
  }
  tmp_attr <- attributes(x)[!names(attributes(x)) %in% c("class", "levels", "labels")]
  u1 <- sort(unique(x))
  u1 <- u1[!is.na(u1)]
  u2 <- sort(unique(val_labs))
  u2 <- u2[!is.na(u2)]
  if(!all(u1 %in% u2))
    stop("missing or invalid value labels")
  x <- factor(x, levels = unname(val_labs), labels = names(val_labs))
  attributes(x) <- c(attributes(x), tmp_attr)
  x
}

