

#' internal replacement of match function for remove missing values (match is much slower but can handle more data classes)
#' @description
#' description description
#'
#' @param x vector
#' @param y vector
.i_find_in <- function(x, y){
  stopifnot(is.logical(x) || is.numeric(x) || is.character(x) || is.factor(x) || "Date" %in% class(x))
  stopifnot(is.logical(y) || is.numeric(y) || is.character(y) || "Date" %in% class(x))

  if(is.numeric(x)){
    .Call("iFindIn", as.numeric(x), as.numeric(y), PACKAGE = "ilabelled")
  }else if(is.character(x)){
    .Call("iFindIn", as.character(x), as.character(y), PACKAGE = "ilabelled")
  }else if(is.logical(x)){
    .Call("iFindIn", x, y, PACKAGE = "ilabelled")
  }else if(is.factor(x) && is.numeric(y)){
    .Call("iFindIn", as.numeric(x), as.numeric(y), PACKAGE = "ilabelled")
  }else if(is.factor(x) && is.character(y)){
    .Call("iFindIn", as.character(x), as.character(y), PACKAGE = "ilabelled")
  }else if("Date" %in% class(x)){
    .Call("iFindIn", as.character(x), as.character(y), PACKAGE = "ilabelled")
  }else{
    NA
  }
}


#' checks if vector is numeric sequence
#' @param x vector
.is_sequential <- function(x){
  stopifnot(is.numeric(x))
  all(diff(x) == diff(x)[1])
}


#' Match values in i_labelled data via value labels
#' @description
#'
#' Find matches in vector (return T/F)
#' @examples
#' x <- i_labelled(c(1:3,NA), labels = c("A" = 1, "B" = 2, "C" = 3), na_values = 3)
#' x %lin% "A"
#' @param x vector or NULL: the values to be matched. Long vectors are supported.
#' @param table vector or NULL: the values to be matched against. Long vectors are not supported.
#' @export
"%lin%" <- function(x, table){
  if(is.i_labelled(x)){
    if(is.character(table)){
      match(i_as_character(x), table, nomatch = 0) > 0
    }else{
      match(unclass(x), table, nomatch = 0) > 0
    }
  }else{
    match(x, table, nomatch = 0) > 0
  }
}


# mtfrm.i_labelled <- function(x){
#   x
# }

#' match <- function(x, table, nomatch = NA_integer_, incomparables = NULL){
#'   UseMethod("match")
#' }

#' match.default <- function(x, table, nomatch = NA_integer_, incomparables = NULL){
#'   browser()
#'   .Internal(match(x, table, nomatch, incomparables))
#' }

#' match.i_labelled <- function(x, table, nomatch = NA_integer_, incomparables = NULL){
#'   browser()
#'   if(is.character(table)){
#'     .Internal(match(as.character(x), table, nomatch, incomparables))
#'   }else{
#'     .Internal(match(as.numeric(x), table, nomatch, incomparables))
#'   }
#' }


# i_find_in_test <- inline::cfunction(
#   sig = c(x = "ANY", y = "ANY"),
#   body = "
#   int xn = length(x);
#   int yn = length(y);
#   int one = 1;
#   bool t = true;
#
#   SEXP out = PROTECT(allocVector(LGLSXP, xn));
#
#   if(TYPEOF(x) != TYPEOF(y)){
#     SEXP err = PROTECT(allocVector(INTSXP, 1));
#     INTEGER(err)[0] = NA_INTEGER;
#     UNPROTECT(2);
#     return err;
#   }
#
#   for(int i = 0; i < xn; i++){
#     bool res = !t;
#     for(int j = 0; j < yn; j++){
#       if(TYPEOF(x) == REALSXP){
#         if(REAL(x)[i] == REAL(y)[j]){
#             res = t;
#             break;
#         }
#       }else if(TYPEOF(x) == STRSXP){
#         if(STRING_ELT(x, i) == STRING_ELT(y, j)){
#           res = t;
#           break;
#         }
#       }else if(TYPEOF(x) == LGLSXP){
#         if(LOGICAL(x)[i] == LOGICAL(y)[j]){
#           res = t;
#           break;
#         }
#       }
#     }
#     LOGICAL(out)[i] = res;
#   }
#
#   UNPROTECT(1);
#
#   return out;
#   "
# )

