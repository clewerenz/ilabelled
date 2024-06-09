
#' copy meta information from one variable to another
#' @returns Returns 'to' with ilabelled attributes copied from 'from'
#' @param to vector
#' @param from vector
#' @param what character vector describing which labels are copied: 'all' (default), 'label', 'labels', 'na_values', 'na_range'
#' @param ... further attributes passed to structure
i_copy <- function(to, from, what = "all", ...){
  stopifnot(is.atomic(to) || is.atomic(from))
  what <- tolower(what)

  label <- attr(to, "labels", TRUE)
  labels <- attr(to, "labels", TRUE)
  na_values <- attr(to, "na_values", TRUE)
  na_range <- attr(to, "na_range", TRUE)
  scale <- attr(to, "scale", TRUE)
  annotation <- attr(to, "annotation", TRUE)
  wording <- attr(to, "wording", TRUE)
  subject <- attr(to, "subject", TRUE)

  if(any(c("all", "label") %in% what)){
    label <- attr(from, "label", TRUE)
  }
  if(any(c("all", "labels") %in% what)){
    labels <- attr(from, "labels", TRUE)
  }
  if(any(c("all", "na_values") %in% what)){
    na_values <- attr(from, "na_values", TRUE)
  }
  if(any(c("all", "na_range") %in% what)){
    na_range <- attr(from, "na_range", TRUE)
  }
  if(any(c("all", "scale") %in% what)){
    scale <- attr(from, "scale", TRUE)
  }
  if(any(c("all", "annotation") %in% what)){
    annotation <- attr(from, "annotation", TRUE)
  }
  if(any(c("all", "wording") %in% what)){
    wording <- attr(from, "wording", TRUE)
  }
  if(any(c("all", "subject") %in% what)){
    subject <- attr(from, "subject", TRUE)
  }

  structure(
    to,
    label = label,
    labels = labels,
    na_values = na_values,
    na_range = na_range,
    scale = scale,
    annotation = annotation,
    wording = wording,
    subject = subject,
    ...
  )
}
