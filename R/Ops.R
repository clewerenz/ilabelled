
#' @export
Ops.i_labelled <- function(e1, e2) {

  miss_e2 <- missing(e2)

  # convert character when class is "i_labelled" so value-labels are used
  if(inherits(e1, "i_labelled")){
    # class(e1) <- setdiff(class(e1), "i_labelled")
    e1 <- i_as_character(e1)
  }
  if(!miss_e2 && inherits(e2, "i_labelled")){
    # class(e2) <- setdiff(class(e2), "i_labelled")
    e2 <- i_as_character(e1)
  }

  if(!miss_e2){
    get(.Generic)(e1, e2)
  }else{
    get(.Generic)(e1)
  }
}

