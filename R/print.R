

print.i_labelled <- function(x, ...){

  n <- getOption("max.print")
  if(n > length(x)){
    n <- length(x)
  }

  # cat("<", paste0(class(x), " (", attr(x, "class_origin", T), ")"), ">", sep = "")
  cat("<", paste0(class(x), collapse = " - "), ">", sep = "")
  cat("\n")
  cat(x[1:n])

  if(!is.null(attr(x, "label", T))){
    cat("\n\nVariable label:", attr(x, "label", T), "")
  }

  if(!is.null(attr(x, "labels", T))){
    cat("\n\nValue labels:")
    for (i in seq(attr(x, "labels", T))) {
      cat("\n\t", attr(x, "labels", T)[i], names(attr(x, "labels", T)[i]))
    }
  }

  cat("\n")

  invisible(x)
}


