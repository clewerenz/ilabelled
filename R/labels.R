

#' set value labels
#' @export
#' @param x vector
#' @param ... set labels for values (e.g. label_of_choice = 1 or "Label of Choice" = 1); remove single label with NULL = value (e.g. NULL = 1); removes all value labels when only NULL (e.g. i_label(x, NULL))
#' @param overwrite when TRUE, all existing labels are dropped and replaced with the new ones
#' @details
#' In order to assign a specific label to multiple values a named list can also be provided to ... (e.g. list(missing = -9:-1, valid = 1:3))
#'
#' A named vector can also be provided (e.g. setNames(c(1,2), c("A","B")))
#' @importFrom stats setNames
#' @returns returns x with value labels applied
i_labels <- function(x, ..., overwrite = FALSE){
  if(!is.null(attr(x, "labels", TRUE)) && !overwrite){
    old_labs <- attr(x, "labels", exact = TRUE)
  }else{
    old_labs <- NULL
  }

  new_labs <- .eval_dots_arg(..., flatten = TRUE)
  if(!length(new_labs)) new_labs <- NULL

  if(is.null(new_labs)){
    all_labs <- NULL
  }else{
    all_labs <- .merge_labels(old_labs, new_labs)
  }

  if(!is.numeric(x) && !is.null(all_labs) && !is.character(all_labs)){
    stop("Cannot apply non-character value labels to non-numeric vector. Value labels must be character.")
  }else if(is.numeric(x) && !is.null(all_labs) && !is.numeric(all_labs)){
    stop("Cannot apply non-numeric value labels to numeric vector. Value labels must be numeric.")
  }

  if(!is.null(all_labs)){
    all_labs <- all_labs[order(all_labs, decreasing = FALSE)]
  }

  structure(x, labels = all_labs)
}


#' combine old value labels with new value labels
#' @returns Returns names vector of value labels
#' @param old_labs named vector
#' @param new_labs named vector
.merge_labels <- function(old_labs, new_labs){
  if(length(old_labs) < 1) old_labs <- NULL
  if(length(new_labs) < 1) new_labs <- NULL
  if(!.valid_labels(old_labs)) stop("invalid labels value")
  if(!.valid_labels(new_labs)) stop("invalid labels value")

  all_labs <- c(new_labs, old_labs)
  all_labs <- all_labs[!duplicated(all_labs)]
  all_labs <- all_labs[!names(all_labs) %in% "NULL"]
  if(length(all_labs) < 1) all_labs <- NULL

  return(all_labs)
}


#' validate value labels - intern
#' @description
#' contains several run-time-tests for value labels
#' runs internally
#'
#' @returns No return value. Aborts process when run-time-tests fail.
#' @param x named vector c(label = value)
.valid_labels <- function(x){
  if(is.null(x)){
    TRUE
  }else if(any(is.na(x))){
    FALSE
  }else if(is.list(x) || is.data.frame(x)){
    FALSE
  }else if(length(grep("[[:alnum:]]", names(x))) != length(x)){
    FALSE
  }else if(length(x) < 1){
    FALSE
  }else if(any(duplicated(x))){
    FALSE
  }else{
    TRUE
  }
}


#' validate value labels
#' @description
#' returns boolean when applied to vector
#'
#' returns a named list when applied to data.frame
#'
#' @returns No return value. Aborts process when run-time-tests fail
#' @param x vector or data.frame
#' @export
i_valid_labels <- function(x){
  UseMethod("i_valid_labels")
}


#' @export
i_valid_labels.default <- function(x){
  y <- attr(x, "labels", TRUE)
  # is_valid <- !"try-error" %in% class(try(.valid_labels(y), silent = TRUE))
  is_valid <- .valid_labels(y)
  (!is.null(y) && is_valid) && all(unique(x[!is.na(x)]) %in% y)
}


#' @export
i_valid_labels.data.frame <- function(x){
  sapply(x, i_valid_labels, USE.NAMES = T, simplify = FALSE)
}


#' sort value labels by values or by labels
#' @returns Returns x with sorted value labels
#' @param x vector or data.frame
#' @param by either values or labels
#' @param decreasing sort decreasing
#' @export
i_sort_labels <- function(x, by = "values", decreasing = FALSE){
  UseMethod("i_sort_labels")
}


#' @export
i_sort_labels.default <- function(x, by = "values", decreasing = FALSE){
  by <- tolower(by)
  if(!(is.atomic(x) & length(x)) > 0){
    stop("x must be vector")
  }
  if(!(is.atomic(by) & length(by) == 1)){
    stop("by must be character vector length 1")
  }
  if(!(is.logical(decreasing) & length(decreasing) == 1)){
    stop("decreasing must be either TRUE or FALSE")
  }
  if(!by %in% c("values", "labels")){
    stop("'by' must be either 'values' or 'labels'")
  }
  labels <- attr(x, "labels", TRUE)
  if(!is.null(labels)){
    if(!.valid_labels(labels)) stop("invalid labels attribute")
    if(by == "values"){
      labels <- labels[order(labels, decreasing = decreasing)]
    }else{
      labels <- labels[order(names(labels), decreasing = decreasing)]
    }
  }
  structure(
    x,
    labels = labels
  )
}


#' @export
i_sort_labels.data.frame <- function(x, by = "values", decreasing = FALSE){
  x[] <- lapply(x, function(y) i_sort_labels(y, by = by, decreasing = decreasing))
  x
}


#' Check for required value labels in set of variables
#'
#' @returns No return value (exept when verbose = T). Aborts process when test not valid.
#' @param x data.frame
#' @param labels character vector
#' @param info string with info message (purpose of assertion) - optional
#' @param verbose return TRUE when assertion is successful
#' @export
i_assert_labels <- function(x, labels, info = NULL, verbose = TRUE){
  UseMethod("i_assert_labels")
}


#' @export
i_assert_labels.default <- function(x, labels, info = NULL, verbose = TRUE){
  # prepare info message
  if(!is.null(info)){
    info <- paste0(info, ": ")
  }
  # all variables must be factor or i_labelled
  wrongVariableClass <- !any(c("factor", "i_labelled") %in% class(x))
  if(wrongVariableClass){
    stop(paste0(info, "variable is not factor or i_labelled"))
  }
  # get value labels
  if("i_labelled" %in% class(x)){
    varLabels <- names(attr(x, "labels", TRUE))
  }else if("factor" %in% class(x)){
    varLabels <- levels(x)
  }else{
    stop(paste0(info, "variable is not factor or i_labelled"))
  }
  # all required labels must be in value labels
  if(!all(labels %in% varLabels)){
    stop(paste0(info, "variable does not have the required labels"))
  }

  if(verbose){
    TRUE
  }
}


#' @export
i_assert_labels.data.frame <- function(x, labels, info = NULL, verbose = TRUE){
  myVars <- names(x)
  # run assert labels for all vars
  res <- sapply(myVars, function(y){
    tryCatch({
      i_assert_labels(x[[y]], labels = labels, info = NULL, verbose = TRUE)
    }, error = function(e){
      return(e$message)
    })
  }, simplify = FALSE)
  # print error messages, when error occurred
  res <- res[unlist(lapply(res, function(y) y != "TRUE"))]
  if(length(res) > 0){
    res <- unlist(lapply(names(res), function(y) paste0(y, " - ", res[[y]])))
    if(!is.null(info)){
      res <- c(paste0(info, ":"), res)
    }
    stop(message(strwrap(res, prefix = "\n", initial = "")))
  }
  if(verbose){
    TRUE
  }
}


