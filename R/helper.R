

#' clear global environment and restart r ression
restartR <- function(){
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  rstudioapi::restartSession()
}
