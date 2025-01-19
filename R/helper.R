#' @title evaluate dots argument
#' @details
#' Make list from dots, unlist and evaluate.
#'
#' @param ... dots passed from function input
#' @param flatten should lists be flattened
#' @returns vector
.eval_dots_arg <- function(..., flatten = FALSE){
  ret <- eval(unlist(list(...), use.names = TRUE, recursive = FALSE), enclos = NULL)

  if(flatten && !is.null(ret)){
    ret <- unlist(lapply(seq(length(ret)), FUN = function(x){
      tmp_val <- unlist(ret[[x]])
      tmp_nam <- rep(names(ret)[[x]], length(tmp_val))
      stats::setNames(tmp_val, tmp_nam)
    }))
  }

  if(length(ret) < 1){
    ret <- NULL
  }

  return(ret)
}


#' Check if vector contains decimal values
#' @importFrom stats na.omit
#' @returns T/F
#' @param x numeric vector
is_decimal <- function(x){
  if(!is.atomic(x)) stop("x must be atomic vector")
  if(!is.numeric(x)) stop("x must be numeric")
  any(stats::na.omit(x %% 1 > 0))
}


#' internal replacement of match function for remove missing values (match is much slower but can handle more data classes)
#' @description
#' description description
#'
#' @returns Vector of T/F values with length of x
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
#' @returns T/F
#' @param x vector
.is_sequential <- function(x){
  stopifnot(is.numeric(x))
  all(diff(x) == diff(x)[1])
}



#' Match values
#' @description
#'
#' Find matches (return T/F)
#' @returns Vector of T/F values with length of x
#' @param x vector or NULL: the values to be matched. Long vectors are supported.
#' @param table vector or NULL: the values to be matched against. Long vectors are not supported.
.i_in <- function(x, table){
  if(is.i_labelled(x)){
    if(is.i_labelled(table)){
      match(i_as_character(x), i_as_character(table), nomatch = 0) > 0
    }else if(is.character(table)){
      match(i_as_character(x), table, nomatch = 0) > 0
    }else{
      match(unclass(x), table, nomatch = 0) > 0
    }
  }else{
    match(x, table, nomatch = 0) > 0
  }
}


# "%lin%" <- function(x, table){
#   .i_in(x, table)
# }


#' @title generic for \%in\%
#' @name grapesingrapes
#' @export
#' @docType methods
#' @rdname grapesingrapes-methods
#' @param x vector or NULL: the values to be matched. Long vectors are supported.
#' @param table vector or NULL: the values to be matched against. Long vectors are not supported.
#' @aliases %in%
#' @returns T/F
methods::setGeneric("%in%")

#' @name grapesingrapes
#' @aliases %in%,i_labelled,ANY-method
#' @docType methods
#' @rdname grapesingrapes-methods
suppressMessages(methods::setMethod("%in%", methods::signature(x="i_labelled"), .i_in))

#' @name grapesingrapes
#' @aliases %in%,ANY,i_labelled-method
#' @docType methods
#' @rdname grapesingrapes-methods
suppressMessages(methods::setMethod("%in%", methods::signature(table="i_labelled"), .i_in))

#' @name grapesingrapes
#' @aliases %in%,i_labelled,i_labelled-method
#' @docType methods
#' @rdname grapesingrapes-methods
suppressMessages(methods::setMethod("%in%", methods::signature(x="i_labelled", table="i_labelled"), .i_in))


# mtfrm.i_labelled <- function(x){
#   browser()
#   x
# }


########################################################################


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

