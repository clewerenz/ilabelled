
#' custom print method for i_labelled
#' @export
#' @param x vector of class i_labelled
#' @param ... not used
print.i_labelled <- function(x, ...){

  if(is.double(unclass(x))){
    cat("<", "i_labelled double", ">\n", sep = "")
  }else if(is.character(unclass(x))){
    cat("<", "i_labelled character", ">\n", sep = "")
  }else if(is.numeric(unclass(x))){
    cat("<", "i_labelled numeric", ">\n", sep = "")
  }else{
    stop("print.i_labelled unknown class")
  }

  print(`attributes<-`(x, NULL))

  i_print_na_values(x)
  i_print_na_range(x)
  i_print_scale(x)
  i_print_label(x)
  i_print_labels(x)

  invisible(x)
}


#' print value labels
#' @param x vector
#' @export
i_print_labels <- function(x){
  UseMethod("i_print_labels")
}

#' @export
i_print_labels.default <- function(x){
  labels <- attr(x, "labels", T)
  if(is.null(labels)){
    return(invisible(labels))
  }
  cat("\nValue labels:\n")
  labels <- data.frame(value = labels, label = names(labels), row.names = NULL, stringsAsFactors = F)
  print(labels, row.names = F)
}


#' print variable label
#' @param x vector
#' @export
i_print_label <- function(x){
  UseMethod("i_print_label")
}

#' @export
i_print_label.default <- function(x){
  label <- attr(x, "label", T)
  if(is.null(label)){
    return(invisible(label))
  }
  cat("\nVariable label:", label, "\n")
}


#' print missing values
#' @param x vector
#' @export
i_print_na_values <- function(x){
  UseMethod("i_print_na_values")
}

#' @export
i_print_na_values.default <- function(x){
  na_values <- attr(x, "na_values", T)
  if(is.null(na_values)){
    return(invisible(na_values))
  }
  cat(paste0("\nMissing values: [", paste0(sort(na_values), collapse = ","), "]", "\n"))
}


#' print missing range
#' @param x vector
#' @export
i_print_na_range <- function(x){
  UseMethod("i_print_na_range")
}

#' @export
i_print_na_range.default <- function(x){
  na_range <- attr(x, "na_range", T)
  if(is.null(na_range)){
    return(invisible(na_range))
  }
  cat(paste0("\nMissing range: [", min(na_range, na.rm = T), ":", max(na_range, na.rm = T), "]"), "\n")
}


#' print scale level
#' @param x vector
#' @export
i_print_scale <- function(x){
  UseMethod("i_print_scale")
}

#' @export
i_print_scale.default <- function(x){
  scale <- attr(x, "scale", T)
  if(is.null(scale)){
    return(invisible(scale))
  }
  cat(paste0("\nScale level: ", scale), "\n")
}


