
#' @export
Ops.i_labelled <- function(e1, e2) {

  miss_e2 <- missing(e2)

  if(!miss_e2){
    if(inherits(e2, "i_labelled")){
      if(inherits(e2, c("numeric", "double", "integer", "logical")) && is.numeric(e1)){
        e1 <- as.numeric(e1)
        e2 <- as.numeric(e2)
      }else{
        e2 <- i_as_character(e2)
        e1 <- i_as_character(e1)
      }
    }else if(inherits(e2, c("numeric", "double", "integer", "logical")) && is.numeric(e1)){
      e1 <- as.numeric(e1)
    }else{
      e2 <- as.character(e2)
      e1 <- i_as_character(e1)
    }
  }

  if(!miss_e2){
    get(.Generic)(e1, e2)
  }else{
    get(.Generic)(e1)
  }
}

