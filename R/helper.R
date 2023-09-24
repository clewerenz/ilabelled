

#' clear global environment and restart r ression
restartR <- function(){
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  rstudioapi::restartSession()
}



# find_nas_numeric <- inline::cfunction(
#   sig = c(x = "numeric", y = "numeric"),
#   body = "
#   int xn = length(x);
#   int yn = length(y);
#   int one = 1;
#   bool t = true;
#
#   SEXP out = PROTECT(allocVector(LGLSXP, xn));
#
#   for(int i = 0; i < xn; i++){
#     int res = 0;
#     for(int j = 0; j < yn; j++){
#       if(res < 1 & (REAL(x)[i] == REAL(y)[j])){
#         res = one;
#       }
#     }
#     if(res > 0){
#       LOGICAL(out)[i] = t;
#     }else{
#       LOGICAL(out)[i] = !t;
#     }
#   }
#
#   UNPROTECT(1);
#
#   return out;
#   "
# )


#' @export
findMissing <- function(x, y){
  if(is.numeric(x) & is.numeric(y)){
    .Call("findNaNumeric", as.numeric(x), as.numeric(y), PACKAGE = "i_labelled")
  }else if(is.character(x) & is.character(y)){
    stop("findNaCharacter for character still to do")
  }else{
    stop("class x does not match class y")
  }
}






