#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>

SEXP findNaNumeric(SEXP x, SEXP y) {

  int xn = length(x);
  int yn = length(y);
  int one = 1;
  int t = true;

  SEXP out = PROTECT(allocVector(LGLSXP, xn));

  for(int i = 0; i < xn; i++){
    int res = 0;
    for(int j = 0; j < yn; j++){
      if(res < 1 && (REAL(x)[i] == REAL(y)[j])){
        res = one;
      }
    }
    if(res > 0){
      LOGICAL(out)[i] = t;
    }else{
      LOGICAL(out)[i] = !t;
    }
  }

  UNPROTECT(1);

  return out;
}
