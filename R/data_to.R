

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

