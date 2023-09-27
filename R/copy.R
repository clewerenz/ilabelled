
#' copy labels from one variable to another
#' @param to vector
#' @param from vector
#' @param what character vector describing which labels are copied: 'all' (default), 'label', 'labels', 'na_values', 'na_range'
#' @param ... further attributes passed to structure
i_copy <- function(to, from, what = "all", ...){
  stopifnot(is.atomic(to) || is.atomic(from))
  what <- tolower(what)
  label <- attr(to, "labels", T)
  labels <- attr(to, "labels", T)
  na_values <- attr(to, "na_values", T)
  na_range <- attr(to, "na_range", T)

  if(any(c("all", "label") %in% what)){
    label <- attr(from, "label", T)
  }
  if(any(c("all", "labels") %in% what)){
    labels <- attr(from, "labels", T)
  }
  if(any(c("all", "na_values") %in% what)){
    na_values <- attr(from, "na_values", T)
  }
  if(any(c("all", "na_range") %in% what)){
    na_range <- attr(from, "na_range", T)
  }

  structure(
    to,
    label = label,
    labels = labels,
    na_values = na_values,
    na_range = na_range,
    ...
  )
}
