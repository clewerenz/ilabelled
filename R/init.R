

.init <- function(x, ...) {

  if(!is.i_labelled(x)){
    x <- structure(
      x,
      ...,
      class = c("i_labelled", class(x)),
      # class_origin = class(x),
      labels = if(is.factor(x)){
        stats::setNames(1:length(levels(x)), levels(x))
      }
    )
  }
  return(x)
}


.deinit <- function(x){
  class(x) <- setdiff(class(x), 'i_labelled')
  return(x)
}
