#include <R.h>
#include <Rinternals.h>
#include <math.h>

SEXP asCharILabelled(SEXP x) {
  int xn = length(x);
  int convertVal = 0;
  SEXP attribute = PROTECT(getAttrib(x, install("labels")));
  SEXP ret = PROTECT(allocVector(STRSXP, xn));
  SEXP labels = PROTECT(getAttrib(attribute, install("names")));
  SEXP values = PROTECT(duplicate(attribute));
  setAttrib(values, install("names"), R_NilValue);

  if (labels == R_NilValue || values == R_NilValue || LENGTH(labels) != LENGTH(values) || TYPEOF(labels) != STRSXP) {
    error("Invalid labels");
  }

  // Convert values to double if it's of type integer
  if (TYPEOF(x) == REALSXP && TYPEOF(values) == INTSXP) {
    convertVal = 1;
    values = PROTECT(coerceVector(values, REALSXP));
  }

  int xv = LENGTH(values);

  for (int i = 0; i < xn; i++) {
    int foundMatch = 0; // Flag to check if a match is found

    switch (TYPEOF(x)) {
    case REALSXP: {
      double currentXValue = REAL(x)[i];
      if (ISNA(currentXValue)) {
        SET_STRING_ELT(ret, i, NA_STRING);
        continue;
      }

      for (int j = 0; j < xv; j++) {
        if (currentXValue == REAL(values)[j]) {
          SET_STRING_ELT(ret, i, STRING_ELT(labels, j));
          foundMatch = 1;
          break;
        }
      }
      break;
    }
    case INTSXP: {
      int currentXValue = INTEGER(x)[i];
      if (currentXValue == NA_INTEGER) {
        SET_STRING_ELT(ret, i, NA_STRING);
        continue;
      }

      for (int j = 0; j < xv; j++) {
        if (currentXValue == REAL(values)[j]) {
          SET_STRING_ELT(ret, i, STRING_ELT(labels, j));
          foundMatch = 1;
          break;
        }
      }
      break;
    }
    case STRSXP: {
      SEXP currentXValue = STRING_ELT(x, i);
      if (currentXValue == NA_STRING) {
        SET_STRING_ELT(ret, i, NA_STRING);
        continue;
      }

      for (int j = 0; j < xv; j++) {
        if (strcmp(CHAR(currentXValue), CHAR(STRING_ELT(values, j))) == 0) {
          SET_STRING_ELT(ret, i, STRING_ELT(labels, j));
          foundMatch = 1;
          break;
        }
      }
      break;
    }
    default:
      error("Unsupported type of 'x'");
    }

    // If no match found, use a character version of the original value
    if (!foundMatch) {
      char buffer[100];  // Adjust the buffer size accordingly
      switch (TYPEOF(x)) {
      case REALSXP: {
        // snprintf(buffer, sizeof(buffer), "%f", REAL(x)[i]);
        double roundedXValue = round(REAL(x)[i]);  // Round to zero digits
        snprintf(buffer, sizeof(buffer), "%.*f", roundedXValue == (int)roundedXValue ? 0 : 1, roundedXValue);
        break;
      }
      case INTSXP:
        snprintf(buffer, sizeof(buffer), "%d", INTEGER(x)[i]);
        break;
      case STRSXP:
        strcpy(buffer, CHAR(STRING_ELT(x, i)));
        break;
      default:
        error("Unsupported type for conversion");
      }
      SET_STRING_ELT(ret, i, mkChar(buffer));
    }
  }

  if (convertVal == 1) {
    UNPROTECT(1);
  }
  UNPROTECT(4);
  return ret;
}
