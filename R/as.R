
#' @export
as.character.i_labelled <- function(x, ...){
  if(is.null(attr(x, "labels", T))){
    as.character(unclass(x))
  }else{
    .Call("asCharILabelled", x, PACKAGE = "ilabelled")
  }
}


#' @export
as.i_labelled <- function(x, ...){
  keepAttr <- setdiff(names(attributes(x)), names(list(...)))
  if(length(keepAttr) > 0){
    attributes(x) <- attributes(x)[keepAttr]
  }else{
    attributes(x) <- NULL
  }
  i_labelled(x, ...)
}

