

#' custom print method
#' @export
#' @param x array of class i_labelled
#' @param ... not used
print.i_labelled <- function(x, ...){

  cat("<", paste0(class(x), collapse = " - "), ">\n", sep = "")
  print(`attributes<-`(x, NULL))
  if(!is.null(attr(x, "na_values", T))){
    cat(paste0("\nMissing values: [", paste0(attr(x, "na_values", T), collapse = ","), "]"))
  }
  if(!is.null(attr(x, "na_range", T))){
    cat(paste0("\nMissing range: [", min(attr(x, "na_range", T), na.rm = T), ":", max(attr(x, "na_range", T), na.rm = T), "]"))
  }
  if(!is.null(attr(x, "label", T))){
    cat("\nVariable label:", attr(x, "label", T), "")
  }
  if(!is.null(attr(x, "labels", T))){
    cat("\nValue labels:")
    for (i in seq(attr(x, "labels", T))) {
      cat("\n\t", attr(x, "labels", T)[i], names(attr(x, "labels", T)[i]))
    }
  }

  cat("\n")

  invisible(x)
}

