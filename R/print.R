

#' custom print method for i_labelled
#' @export
#' @param x vector of class i_labelled
#' @param ... not used
print.i_labelled <- function(x, ...){

  if(is.double(unclass(x))){
    cat("<", "i_labelled double", ">\n", sep = "")
  }else if(is.character(unclass(x))){
    cat("<", "i_labelled character", ">\n", sep = "")
  }else{
    stop("print.i_labelled unknown class")
  }

  print(`attributes<-`(x, NULL))

  i_get_na_values(x)
  i_get_na_range(x)
  i_get_label(x)
  i_get_labels(x)

  invisible(x)
}


#' get value labels
#' #' @description
#' print variable labels when applied to vector
#' return list when applied to data.frame
#'
#' @param x vector or data.frame
#' @export
i_get_labels <- function(x){
  UseMethod("i_get_labels")
}


#' @export
i_get_labels.default <- function(x){
  labels <- attr(x, "labels", T)
  if(is.null(labels)){
    return(invisible(labels))
  }
  cat("\nValue labels:\n")
  labels <- data.frame(value = labels, label = names(labels), row.names = NULL, stringsAsFactors = F)
  print(labels, row.names = F)
}


#' @export
i_get_labels.data.frame <- function(x){
  sapply(x, function(y){
    labels <- attr(y, "labels", T)
    if(is.null(labels)){
      NA
    }else{
      data.frame(value = labels, label = names(labels), row.names = NULL, stringsAsFactors = F)
    }
  }, simplify = F)
}


#' get variable label
#' @description
#' print variable label when applied to vector
#' return list when applied to data.frame
#'
#' @param x vector or data.frame
#' @export
i_get_label <- function(x){
  UseMethod("i_get_label")
}


#' @export
i_get_label.default <- function(x){
  label <- attr(x, "label", T)
  if(is.null(label)){
    return(invisible(label))
  }
  cat("\nVariable label:", label, "\n")
}


#' @export
i_get_label.data.frame <- function(x){
  sapply(x, function(x){
    label <- attr(x, "label", T)
    if(is.null(label)){
      NA
    }else{
      label
    }
  }, simplify = F)
}


#' print missing values
#' @description
#' print missing values when applied to vector
#' return list when applied to data.frame
#'
#' @param x vector or data.frame
#' @export
i_get_na_values <- function(x){
  UseMethod("i_get_na_values")
}


#' @export
i_get_na_values.default <- function(x){
  na_values <- attr(x, "na_values", T)
  if(is.null(na_values)){
    return(invisible(na_values))
  }
  cat(paste0("\nMissing values: [", paste0(sort(na_values), collapse = ","), "]", "\n"))
}


#' @export
i_get_na_values.data.frame <- function(x){
  sapply(x, function(y){
    na_values <- attr(y, "na_values", T)
    if(is.null(na_values)){
      NA
    }else{
      sort(na_values)
    }
  }, simplify = F)
}


#' print missing range
#' @description
#' print missing range when applied to vector
#' return list when applied to data.frame
#'
#' @param x vector or data.frame
#' @export
i_get_na_range <- function(x){
  UseMethod("i_get_na_range")
}


#' @export
i_get_na_range.default <- function(x){
  na_range <- attr(x, "na_range", T)
  if(is.null(na_range)){
    return(invisible(na_range))
  }
  cat(paste0("\nMissing range: [", min(na_range, na.rm = T), ":", max(na_range, na.rm = T), "]"), "\n")
}


#' @export
i_get_na_range.data.frame <- function(x){
  sapply(x, function(y){
    na_range <- attr(y, "na_range", T)
    if(is.null(na_range)){
      NA
    }else{
      sort(na_range)
    }
  }, simplify = F)
}
