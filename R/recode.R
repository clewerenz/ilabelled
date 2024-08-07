#' i_recode
#' Function for recoding new variable from origin variable(s).
#'
#' Returns a vector object of class i_labelled
#'
#' Can be applied to either vector or data.frame. When x is data.frame the formula passed to ... is different from when it is applied to single vector.
#' When function is applied to a data.frame, multiple conditions on multiple variables are possible (e.g when variable X is equal to this, do that; when variable Y is not equal to this, do that, etc.). See examples for further clarification.
#'
#' You can recode directly via value labels by using %in%.
#'
#' @examples
#' # When applied to a single vector:
#' # keep in mind that when function is applied to vector, instead of a column use x
#' myVector <- i_labelled(1:4, labels = c("A" = 1, "B" = 2, "C" = 3, "D" = 4))
#' i_recode(x = myVector, "AB" = 1 ~ x %in% c("A", "B"), "CD" = 2 ~ x == c(3, 4))
#'
#' # When applied to data.frame (multiple conditions)
#' myData <- data.frame(
#'    V1 = i_labelled(1:3, labels = c("A" = 1, "B" = 2, "C" = 3)),
#'    V2 = i_labelled(c(2:3,-9))
#' )
#' i_recode(x = myData, A = 1 ~ V1 %in% c("A", "B"), 2 ~ "V2" == 3, "C" = 999 ~ V2 == -9)
#'
#' @returns Returns i_labelled vector with values defined by formula and information given to function.
#' @param x vector or data.frame
#' @param ... formula for recoding of values. See examples.
#' @param label variable label
#' @param na_values a vector with missing values
#' @param na_range a vector for missing range
#' @param scale scale level (nominal, ordinal, metric)
#' @param annotation addition information about variable
#' @param wording question text
#' @param subject subject
#' @param copy When applied to vector: T/F. When applied to a data.frame: a variable from x. Copy the values of an existing variable or x before recoding values according to ...
#' @param keep_labels keep value labels from origin vector when copy TRUE or variable from x
#' @importFrom stats setNames
#' @export
i_recode <- function(x, ..., label = NULL, na_values = NULL, na_range = NULL, scale = NULL, annotation = NULL, wording = NULL, subject = NULL, copy = NULL, keep_labels = FALSE){

  is_atomic <- is.atomic(x)
  is_data_frame <- is.data.frame(x)

  if(!is_atomic & !is_data_frame){
    stop("'copy' can only be applied to vector or data.frame")
  }

  if(!is.null(copy) && !is.atomic(copy) && length(copy) != 1){
    stop("'copy' must be variable name of length 1 on data.frame or T/F on vector.")
  }

  if(!is.null(copy) && !is_data_frame && !is.logical(copy)){
    stop("'copy' must be T/F when i_recode is applied to vector.")
  }else if(!is.null(copy) && is_data_frame){
    if(!copy %in% names(x)){
      stop("'copy' is not column of data.frame x")
    }
  }

  if(is_atomic && isTRUE(copy)){
    old_labels <- attr(x, "labels", exact = TRUE)
  }else if(is_data_frame && !is.null(copy)){
    old_labels <- attr(x[[copy]], "labels", exact = TRUE)
  }else{
    old_labels <- NULL
  }
  if(is_atomic){
    x <- data.frame(x = x, stringsAsFactors = FALSE)
  }

  recode_map <- list(...)

  if(!all(unlist(lapply(recode_map, function(x) inherits(x, "formula"))))){
    stop("... must be formula")
  }

  tryCatch({
    recode_map <- lapply(seq(recode_map), function(y){
      y <- list(
        new_lab = names(recode_map)[[y]],
        new_val = eval(recode_map[[y]][[2]]),
        formula = recode_map[[y]][[3]]
      )
      y$which_val <- with(x, eval(y$formula))
      y
    })
  }, error = function(e){
    stop("invalid ... formula")
  })

  new_labels <- stats::setNames(
    unlist(lapply(recode_map, function(x){ x$new_val })),
    unlist(lapply(recode_map, function(x){ x$new_lab }))
  )
  new_labels <- new_labels[!names(new_labels) %in% ""]

  if(length(new_labels) > 0){
    new_labels <- new_labels[order(new_labels, decreasing = FALSE)]
    if(keep_labels && !is.null(old_labels)){
      new_labels <- .merge_labels(old_labs = as.list(old_labels), new_labs = as.list(new_labels))
    }
  }else{
    new_labels <- NULL
  }

  # copy part
  if(is_atomic && !is.null(copy) && copy){
    x <- i_unclass(x[[1]])
  }else if(is_data_frame && !is.null(copy)){
    x <- i_unclass(x[[copy]])
  }else{
    x <- rep(NA, length(x))
  }

  for(i in seq(recode_map)){
    x[recode_map[[i]]$which_val] <- recode_map[[i]]$new_val
  }

  i_labelled(x, labels = new_labels, label = label, na_values = na_values, na_range = na_range, scale = scale, annotation = annotation, subject = subject, wording = wording)
}
