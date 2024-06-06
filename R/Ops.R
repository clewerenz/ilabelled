
#' @export
Ops.i_labelled <- function(e1, e2) {

  miss_e2 <- missing(e2)

  if(!miss_e2){
    if(inherits(e2, "i_labelled")){
      e1 <- as.numeric(e1)
      e2 <- as.numeric(e2)
    }else if(inherits(e2, c("numeric", "double", "integer", "logical"))){
      e1 <- as.numeric(e1)
    }else{
      e1 <- i_as_character(e1)
    }
  }

  if(!miss_e2){
    get(.Generic)(e1, e2)
  }else{
    get(.Generic)(e1)
  }
}

