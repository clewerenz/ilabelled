
#' copy meta information from one variable to another
#' @returns Returns 'to' with attributes copied from 'from'
#' @param to vector
#' @param from vector
#' @param what character vector describing which attributes are copied. When 'all' (default), all attributes are copied.
#' @param overwrite overwrite existing attributes when present in attributes of from.
#' @param ... further attributes passed to structure
i_copy <- function(to, from, what = "all", overwrite = TRUE, ...){
  stopifnot(is.atomic(to) || is.atomic(from))

  if(any(grepl("^[Aa]ll$", what))) what <- names(attributes(from))

  if(!overwrite){
    what <- what[!what %in% names(attributes(to))]
  }

  for(i in what){
    attr(to, i) <- attr(from, i, exact = TRUE)
  }

  structure(
    to,
    ...
  )
}
