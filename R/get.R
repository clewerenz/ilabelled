
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


#' get wording
#' @description
#' return wording as character vector applied to vector
#' return list when applied to data.frame
#'
#' @returns returns wording
#' @param x vector or data.frame
#' @export
i_get_wording <- function(x){
  UseMethod("i_get_wording")
}

#' @export
i_get_wording.default <- function(x){
  wording <- attr(x, "wording", TRUE)
  if(is.null(wording)){
    return(NA)
  }
  wording
}

#' @export
i_get_wording.data.frame <- function(x){
  sapply(x, function(y){
    wording <- attr(y, "wording", TRUE)
    if(is.null(wording)){
      NA
    }else{
      wording
    }
  }, simplify = FALSE)
}


#' get subject
#' @description
#' return subject as character vector applied to vector
#' return list when applied to data.frame
#'
#' @returns returns subject
#' @param x vector or data.frame
#' @export
i_get_subject <- function(x){
  UseMethod("i_get_subject")
}

#' @export
i_get_subject.default <- function(x){
  subject <- attr(x, "subject", TRUE)
  if(is.null(subject)){
    return(NA)
  }
  subject
}

#' @export
i_get_subject.data.frame <- function(x){
  sapply(x, function(y){
    subject <- attr(y, "subject", TRUE)
    if(is.null(subject)){
      NA
    }else{
      subject
    }
  }, simplify = FALSE)
}


#' get variable names by subject
#' @description
#' return all variable names by subjects
#'
#' one, several, or all subjects can be looked up
#'
#'
#' @returns named list or NA. return named list with one list entry for each subject. when no subject in data or no match for subjects, return NA.
#' @param x data.frame
#' @param subject one or more subjects as character vector. when NULL return all variable names by all subjects in data
#' @export
i_get_equal_subject <- function(x, subject = NULL){
  if(!inherits(x, "data.frame")) stop("x must be data.frame")
  if(!is.null(subject) && !is.atomic(subject)){
    stop("subject must be character vector")
  }else if(!is.null(subject) && is.atomic(subject) && !is.character(subject)){
    stop("subject must be character vector")
  }

  # get all subject and check for valid subjects
  all_subjects <- i_get_subject(x)
  is_valid_subject <- unlist(lapply(all_subjects, function(y){ .valid_subject(y) }))
  all_subjects <- all_subjects[is_valid_subject]

  if(is.null(subject)){
    # for all subjects in data get variables for each subject
    unique_subjects <- unlist(all_subjects)
    unique_subjects <- unique(unique_subjects)
    unique_subjects <- unique_subjects[!is.na(unique_subjects)]
    ret <- sapply(unique_subjects, function(y){
      all_subjects_temp <- unlist(all_subjects)
      names(all_subjects_temp[all_subjects_temp %in% y])
    }, USE.NAMES = TRUE, simplify = FALSE)
  }else{
    # get variables for each subject provided to function
    ret <- sapply(subject, function(y){
      all_subjects_temp <- unlist(all_subjects)
      names(all_subjects_temp[all_subjects_temp %in% y])
    }, USE.NAMES = TRUE, simplify = FALSE)

    not_available <- unlist(lapply(ret, function(y) length(y) < 1))
    not_available <- names(not_available[not_available])
    for(i in not_available) warning("no matching subject for '", i, "'")

    ret <- ret[!names(ret) %in% not_available]
  }

  # return list when not empty.
  if(length(ret) > 0){
    return(ret)
  }else{
    return(NA)
  }
}



#' get variable names by wording
#' @description
#' return all variable names by wordings
#'
#' one, several, or all wordings can be looked up
#'
#'
#' @returns named list or NA. return named list with one list entry for each wording. when no wording in data or no match for wordings, return NA.
#' @param x data.frame
#' @param wording one or more wordings as character vector. when NULL return all variable names by all wordings in data
#' @export
i_get_equal_wording <- function(x, wording = NULL){
  if(!inherits(x, "data.frame")) stop("x must be data.frame")
  if(!is.null(wording) && !is.atomic(wording)){
    stop("wording must be character vector")
  }else if(!is.null(wording) && is.atomic(wording) && !is.character(wording)){
    stop("wording must be character vector")
  }

  # get all wording and check for valid wordings
  all_wordings <- i_get_wording(x)
  is_valid_wording <- unlist(lapply(all_wordings, function(y){ .valid_wording(y) }))
  all_wordings <- all_wordings[is_valid_wording]

  if(is.null(wording)){
    # for all wordings in data get variables for each wording
    unique_wordings <- unlist(all_wordings)
    unique_wordings <- unique(unique_wordings)
    unique_wordings <- unique_wordings[!is.na(unique_wordings)]
    ret <- sapply(unique_wordings, function(y){
      all_wordings_temp <- unlist(all_wordings)
      names(all_wordings_temp[all_wordings_temp %in% y])
    }, USE.NAMES = TRUE, simplify = FALSE)
  }else{
    # get variables for each wording provided to function
    ret <- sapply(wording, function(y){
      all_wordings_temp <- unlist(all_wordings)
      names(all_wordings_temp[all_wordings_temp %in% y])
    }, USE.NAMES = TRUE, simplify = FALSE)

    not_available <- unlist(lapply(ret, function(y) length(y) < 1))
    not_available <- names(not_available[not_available])
    for(i in not_available) warning("no matching wording for '", i, "'")

    ret <- ret[!names(ret) %in% not_available]
  }

  # return list when not empty.
  if(length(ret) > 0){
    return(ret)
  }else{
    return(NA)
  }
}
