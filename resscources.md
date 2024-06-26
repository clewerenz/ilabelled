# Resscources

## General

- How to write R packages: https://r-pkgs.org/; https://cran.r-project.org/doc/manuals/R-exts.html
- How to deploy R packages: https://cran.r-project.org/web/packages/policies.html

## Use C code in R

- write code in /src/file.c
- collect functions in header file under /src
- compile C code
- when package is compiled or loaded via <code>devtools::load_all()</code>, C code get compiled as .so or .dll file. Alternatively, code can be compiled manually.
- add line to NAMESPACE: <code>useDynLib(ilabelled, .registration = TRUE)</code> (ilabelled in this case is the filename of the .so). Caution, when roxygen2 is used to create NAMEPACE file, this line will be deleted each time <code>devtools::document()</code> is called and has to be added again. In order to avoid deleting useDynLib from NAMESPACE the arguments from the document function can be specified (i.e. <code>devtools::document(roclets = c("collate", "rd"))</code>).
- in order to call C functions from within R functions use <code>.Call()</code>
  - https://stat.ethz.ch/pipermail/r-devel/attachments/20120323/a13f948a/attachment.pdf
- http://adv-r.had.co.nz/C-interface.html
- https://marlin-na.github.io/r-api/
  - https://rstudio.github.io/r-manuals/r-exts/The-R-API.html
- https://marlin-na.github.io/r-api/
  - https://github.com/hadley/r-internals
- https://github.com/Rdatatable/data.table/tree/master
- https://cran.r-project.org/doc/manuals/R-exts.html#Portable-C-and-C_002b_002b-code
- https://stackoverflow.com/questions/1176455/portable-use-of-dyn-load-to-call-a-c-function-in-an-r-package
- in order to compile C code manually use: https://rdrr.io/r/utils/SHLIB.html; https://rdrr.io/github/richfitz/rcmdshlib/man/shlib.html

## run code when package is loaded (<code>.onLoad()</code>)

- https://stackoverflow.com/questions/20223601/r-how-to-run-some-code-on-load-of-package
- https://www.rdocumentation.org/packages/pkgmaker/versions/0.32.10/topics/onLoad
