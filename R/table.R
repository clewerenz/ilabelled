
#' cross tabulation and table creation using i_labelled labels
#' @description
#' wrapper for base::table
#'
#' convert i_labelled objects to base class and pass to table function
#'
#' @param ... one or more atomic vectors
#' @param missing_to_na make as missing declared values NA
#' @param as_factor make labelled data factor before pass to table
#' @param table_args arguments of base::table as named list
#' @returns returns a contingency table, an object of class "table"
#' @export
i_table <- function(..., missing_to_na = TRUE, as_factor = TRUE, table_args = NULL){

  # validate table_args
  if(!is.null(table_args)){
    possible_table_args <- names(formals(table))[-1]
    if("table_args" %in% names(formals()) && !is.list(table_args)){
      stop("table_args must be named list containing arguments from base::table")
    }else if(is.null(names(table_args))){
      stop("table_args must be named list containing arguments from base::table")
    }else if(any(nchar(names(table_args)) < 1)){
      stop("all list elements of table_args must have names with arguments from base::table")
    }else if(!all(names(table_args) %in% possible_table_args)){
      stop("table_args can take base::table arguments ", paste0(possible_table_args, collapse = ", "))
    }
  }

  input <- list(...)

  # check input type
  input_is_df <- is.data.frame(input[[1]])
  if(input_is_df && length(input) != 1){
    stop("... takes either atomic vectors or one data.frame")
  }else if(input_is_df && length(input) == 1){
    input <- input[[1]]
  }

  # convert ... vectors from i_labelled to base classes
  input <- lapply(input, function(x){
    if(!is.atomic(x)) stop("... takes either atomic vectors or one data.frame")
    i_to_base_class(x, missing_to_na = missing_to_na, as_factor = as_factor)
  })

  # make table: pass arguments to base::table
  do.call(
    what = table,
    args = c(input, table_args)
  )
}
