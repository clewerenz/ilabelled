.onAttach <- function(libname, pkgname){
  packageStartupMessage(
    paste(pkgname, "was created under R version 4.4.1")
  )
}


