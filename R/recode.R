
#' Function for recoding new variable from origin variable(s).
#'
#' Returns a vector object of class i_labelled
#'
#' Can be applied to either vector or data.frame. When x is data.frame the formula passed to ... is different than when it is applied to single vector.
#' When function is applied to a data.frame, multiple conditions on multiple variables are possible (e.g when variable X is equal to this, do that; when variable Y is not equal to this, do that, etc.). See examples for further clarification.
#' @examples
#' # When applied to a single vector:
#' myVector <- i_labelled(1:4, labels = c("A" = 1, "B" = 2, "C" = 3, "D" = 4))
#' i_recode(myVector, "AB" = 1 ~ c("A", "B"), "CD" = 2 ~ c(3, 4))
#'
#' # When applied to data.frame (multiple conditions)
#' myData <- data.frame(
#'    V1 = i_labelled(1:3, labels = c("A" = 1, "B" = 2, "C" = 3)),
#'    V2 = i_labelled(c(2:3,-9))
#' )
#' i_recode(myData, A = 1 ~ V1 %in% c("A", "B"), 2 ~ "V2" == 3, "C" = 999 ~ V2 == -9)
#'
#' @param x vector
#' @param ... formula for recoding of values. When function is applied to a single vector, go "NewValueLabel = NewValue ~ c(OriginValues)". When function is apllied to data.frame, go "NewValueLabel = NewValue ~ Variable logical expr. OriginValues". See examples.
#' @param label variable label
#' @param na_values a vector with missing values
#' @param na_rage a vector for missing range
#' @export
i_recode <- function(x, ..., label = NULL, na_values = NULL, na_range = NULL){

  is_atomic <- is.atomic(x)
  is_data_frame <- is.data.frame(x)

  stopifnot(is_atomic | is_data_frame)

  recode_map <- list(...)

  if(!all(unlist(lapply(recode_map, rlang::is_formula)))){
    stop("... must be formular")
  }

  if(is_data_frame){
    recode_map <- lapply(seq(recode_map), function(y){
      y <- list(
        new_lab = names(recode_map)[[y]],
        new_val = eval(recode_map[[y]][[2]]),
        formula = paste0("x$", deparse(recode_map[[y]][[3]]))
      )
      y$which_val <- eval(parse(text = y$formula))
      y
    })
  }else{
    recode_map <- lapply(seq(recode_map), function(y){
      y <- list(
        new_lab = names(recode_map)[[y]],
        new_val = eval(recode_map[[y]][[2]]),
        ori_val = recode_map[[y]][[3]]
      )
      if(length(y$ori_val) > 2){
        y$ori_val <- unlist(lapply(2:length(y$ori_val), function(z){ eval(y$ori_val[[z]]) }))
      }else{
        y$ori_val <- eval(y$ori_val)
      }
      y$which_val <- which(x %in% y$ori_val)
      y
    })
  }

  new_labels <- setNames(
    unlist(lapply(recode_map, function(x){ x$new_val })),
    unlist(lapply(recode_map, function(x){ x$new_lab }))
  )
  new_labels <- new_labels[!names(new_labels) %in% ""]

  x <- rep(NA, length(x))
  for(i in seq(recode_map)){
    x[recode_map[[i]]$which_val] <- recode_map[[i]]$new_val
  }

  i_labelled(x, labels = new_labels, label = label, na_values = na_values, na_range = na_range)
}
