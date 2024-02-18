
#' Function for recoding new variable from origin vairiable.
#' Does not copy old variable, but creates new variable.
#'
#' Returns an object of class i_labelled
#' @examples
#' i_recode(i_labelled(1:4, labels = c("A" = 1, "B" = 2, "C" = 3, "D" = 4)), "AB" = 1 ~ c("A", "B"), "CD" = 2 ~ c(3, 4))
#'
#' @param x vector
#' @param ... formular for recoding of values as follows: newValueLabel = newValue ~ c(oldValues)
#' @param label variable label
#' @param na_values a vector with missing values
#' @param na_rage a vector for missing range
#' @export
i_recode <- function(x, ..., label = NULL, na_values = NULL, na_range = NULL){

  stopifnot(is.atomic(x))

  recode_map <- list(...)

  if(!all(unlist(lapply(recode_map, rlang::is_formula)))){
    stop("... must be formular: val_label = new_value ~ c(old_values)")
  }

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
