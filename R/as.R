
#' @export
as.character.i_labelled <- function(x, ...){
  if(is.null(attr(x, "labels", T))){
    as.character(unclass(x))
  }else{
    .Call("asCharILabelled", x, PACKAGE = "ilabelled")
  }
}

