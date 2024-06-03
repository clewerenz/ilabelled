
#' @export
summary.i_labelled <- function(object, ...){
  if(!is.null(attr(object, "labels", TRUE))){
    summary(i_as_factor(object))
  }else{
    summary(as.numeric(object))
  }
}
