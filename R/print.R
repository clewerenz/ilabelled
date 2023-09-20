

#' custom print method for i_labelled
#' @export
#' @param x vector of class i_labelled
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



#' custom print method for Date
#' @export
#' @param x vector of class Date
#' @param ... not used
print.Date <- function(x, ...){

  max <- getOption("max.print", 9999L)
  if (max < length(x)) {
    print(format(x[seq_len(max)]), max = max + 1, ...)
    cat(" [ reached 'max' / getOption(\"max.print\") -- omitted",
        length(x) - max, "entries ]\n")
  }
  else if (length(x))
    print(format(x), max = max, ...)
  else cat(class(x)[1L], "of length 0\n")

  if(!is.null(attr(x, "label", T))){
    cat("\nVariable label:", attr(x, "label", T), "")
  }

  invisible(x)
}


