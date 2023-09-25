#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>

SEXP iFindIn(SEXP x, SEXP y) {

  int xn = length(x);
  int yn = length(y);
  bool t = true;

  SEXP out = PROTECT(allocVector(LGLSXP, xn));

  if(TYPEOF(x) != TYPEOF(y)){
    SEXP err = PROTECT(allocVector(INTSXP, 1));
    INTEGER(err)[0] = NA_INTEGER;
    UNPROTECT(2);
    return err;
  }

  for(int i = 0; i < xn; i++){
    bool res = !t;
    for(int j = 0; j < yn; j++){
      if(TYPEOF(x) == REALSXP){
        if(REAL(x)[i] == REAL(y)[j]){
            res = t;
            break;
        }
      }else if(TYPEOF(x) == STRSXP){
        if(STRING_ELT(x, i) == STRING_ELT(y, j)){
          res = t;
          break;
        }
      }else if(TYPEOF(x) == LGLSXP){
        if(LOGICAL(x)[i] == LOGICAL(y)[j]){
          res = t;
          break;
        }
      }
    }
    LOGICAL(out)[i] = res;
  }

  UNPROTECT(1);

  return out;
}
