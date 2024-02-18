
# x <- i_labelled(c(1:4, -9), labels = c("A" = 1, "B" = 2, "C" = 3, "D" = 4), label = "MyVar", na_values = -9)
# x

#' Function for recoding new variable from origin vairiable.
#' Does not copy old variablen, but creates new variable.
#' @examples
#' # i_recode(myData$oldVar, "newLab1" = 1 = c("oldLab1", "oldLab2"))
#'
#' @param x vector
#' @param ... formular for recoding of values
#' @param label variable label
#' @param na_values a vector with missing values
#' @param na_rage a vector for missing range
#' @export
i_recode <- function(x, ..., label = NULL, na_values = NULL, na_range = NULL){

  recode_map <- list(...)

  is_formular <- all(unlist(lapply(recode_map, function(x) x[[1]] == "~")))
  has_new_val <- all(unlist(lapply(recode_map, function(x) length(x[[2]]) >= 1)))
  has_old_val <- all(unlist(lapply(recode_map, function(x) length(x[[3]]) >= 2)))
  if(!(is_formular & has_new_val & has_old_val)){
    stop("... must be formular (nm = new_value ~ c(old_values)")
  }

  recode_map <- lapply(seq(recode_map), function(y){
    y <- list(
      new_lab = names(recode_map)[[y]],
      new_val = recode_map[[y]][[2]],
      ori_val = recode_map[[y]][[3]]
    )
    y$ori_val <- unlist(lapply(2:length(y$ori_val), function(z){ y$ori_val[[z]] }))
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

# i_recode(x, 1 ~ c("A", "B"), "CD" = 2 ~ c(3, 4), copy = F)

