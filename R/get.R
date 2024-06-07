
#' get value labels
#' @description
#' return labels when applied to vector
#' return list when applied to data.frame
#'
#' @returns values and value labels as data.frame
#' @param x vector or data.frame
#' @export
i_get_labels <- function(x){
  UseMethod("i_get_labels")
}

#' @export
i_get_labels.default <- function(x){
  labels <- attr(x, "labels", TRUE)
  if(is.null(labels)){
    return(NA)
  }
  data.frame(value = labels, label = names(labels), row.names = NULL, stringsAsFactors = FALSE)
}

#' @export
i_get_labels.data.frame <- function(x){
  sapply(x, function(y){
    labels <- attr(y, "labels", TRUE)
    if(is.null(labels)){
      NA
    }else{
      data.frame(value = labels, label = names(labels), row.names = NULL, stringsAsFactors = FALSE)
    }
  }, simplify = FALSE)
}


#' get variable label
#' @description
#' return variable label when applied to vector
#' return list when applied to data.frame
#'
#' @returns variable label
#' @param x vector or data.frame
#' @export
i_get_label <- function(x){
  UseMethod("i_get_label")
}

#' @export
i_get_label.default <- function(x){
  label <- attr(x, "label", TRUE)
  if(is.null(label)){
    return(NA)
  }
  label
}

#' @export
i_get_label.data.frame <- function(x){
  sapply(x, function(x){
    label <- attr(x, "label", TRUE)
    if(is.null(label)){
      NA
    }else{
      label
    }
  }, simplify = FALSE)
}


#' get missing values
#' @description
#' return missing values when applied to vector
#' return list when applied to data.frame
#'
#' @returns return missing values
#' @param x vector or data.frame
#' @export
i_get_na_values <- function(x){
  UseMethod("i_get_na_values")
}

#' @export
i_get_na_values.default <- function(x){
  na_values <- attr(x, "na_values", TRUE)
  if(is.null(na_values)){
    return(NA)
  }
  sort(na_values)
}

#' @export
i_get_na_values.data.frame <- function(x){
  sapply(x, function(y){
    na_values <- attr(y, "na_values", TRUE)
    if(is.null(na_values)){
      NA
    }else{
      sort(na_values)
    }
  }, simplify = FALSE)
}


#' get missing range
#' @description
#' return missing range when applied to vector
#' return list when applied to data.frame
#'
#' @returns return missing range
#' @param x vector or data.frame
#' @export
i_get_na_range <- function(x){
  UseMethod("i_get_na_range")
}

#' @export
i_get_na_range.default <- function(x){
  na_range <- attr(x, "na_range", TRUE)
  if(is.null(na_range)){
    return(NA)
  }
  sort(na_range)
}

#' @export
i_get_na_range.data.frame <- function(x){
  sapply(x, function(y){
    na_range <- attr(y, "na_range", TRUE)
    if(is.null(na_range)){
      NA
    }else{
      sort(na_range)
    }
  }, simplify = FALSE)
}


#' get scale level
#' @description
#' return scale level when applied to vector
#' return list when applied to data.frame
#'
#' @returns returns scale level
#' @param x vector or data.frame
#' @export
i_get_scale <- function(x){
  UseMethod("i_get_scale")
}

#' @export
i_get_scale.default <- function(x){
  scale <- attr(x, "scale", TRUE)
  if(is.null(scale)){
    return(NA)
  }
  scale
}

#' @export
i_get_scale.data.frame <- function(x){
  sapply(x, function(y){
    scale <- attr(y, "scale", TRUE)
    if(is.null(scale)){
      NA
    }else{
      scale
    }
  }, simplify = FALSE)
}


#' get annotation
#' @description
#' return annotation as character vector applied to vector
#' return list when applied to data.frame
#'
#' @returns returns annotation
#' @param x vector or data.frame
#' @export
i_get_annotation <- function(x){
  UseMethod("i_get_annotation")
}

#' @export
i_get_annotation.default <- function(x){
  annotation <- attr(x, "annotation", TRUE)
  if(is.null(annotation)){
    return(NA)
  }
  annotation
}

#' @export
i_get_annotation.data.frame <- function(x){
  sapply(x, function(y){
    annotation <- attr(y, "annotation", TRUE)
    if(is.null(annotation)){
      NA
    }else{
      annotation
    }
  }, simplify = FALSE)
}
