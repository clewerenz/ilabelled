
# ilabelled

A function library for working with labelled data in R.

# Motivation

  - <b>Data consistency</b> 
    - Allow labelling of values <= 0
    - Do not change numeric data when data is labelled (as factors do) 
      - Allow labelling of nonsequential values
    - Do not loose attributes when recoding or subsetting data
  - <b>Easy access</b> 
    - Intuitive functions for handling labels and missing values 
    - Print labels when data is printed 
  - <b>Easy validation</b> 
    - Missing labels 
    - Incorrect labels 
  - <b>Reduce dependencies</b> 
    - The most possible minimum of third party dependencies. Right now the package depends on no third party libraries.

The inspiration for this package was drawn from the [Sticky](https://github.com/cran/sticky) package and the [labelled](https://github.com/larmarange/labelled) package with the aim to bring dependencies on bloated third party libraries to zero. The self-understanding of this package is not to be better than comparable libraries, but be a lightweight alternative.

# To-Do
  
  - tests for <code>i_recode()</code> on data.frame
  - better messages for run-time-tests
  - <code>i_as_factor()</code>: more tests
  - <code>i_to_base_class()</code>: more tests
  - documentation: rmarkdown html with examples of all features
  - custom r_bind function (maybe):
    - checks if value labels (if present) match
    - checks if classes match
    - returns 'talking' output
  - example data: add correctly to or remove from package
  
# Rescources

## General

  - How to write R packages: https://r-pkgs.org/

## Use C code in R

  - write code in /src/file.c
  - collect functions in header file under /src
  - compile C code
    - when package is compiled or loaded via <code>devtools::load_all()</code>, C code get compiled as .so or .dll file. Alternatively, code can be compiled manually.
  - add line to NAMESPACE: <code>useDynLib(ilabelled, .registration = TRUE)</code> (ilabelled in this case is the filename of the .so). Caution, when roxygen2 is used to create NAMEPACE file, this line will be deleted each time <code>devtools::document()</code> is called and has to be added again. In order to avoid deleting useDynLib from NAMESPACE the arguments from the document function can be specified (i.e. <code>devtools::document(roclets = c("collate", "rd"))</code>).
  - in order to call C functions from within R functions use <code>.Call()</code>
  - http://adv-r.had.co.nz/C-interface.html
  - https://marlin-na.github.io/r-api/
  - https://github.com/Rdatatable/data.table/tree/master
  - https://cran.r-project.org/doc/manuals/R-exts.html#Portable-C-and-C_002b_002b-code
  - https://stackoverflow.com/questions/1176455/portable-use-of-dyn-load-to-call-a-c-function-in-an-r-package
  - in order to compile C code manually use: https://rdrr.io/r/utils/SHLIB.html; https://rdrr.io/github/richfitz/rcmdshlib/man/shlib.html

## run code when package is loaded (<code>.onLoad()</code>)

  - https://stackoverflow.com/questions/20223601/r-how-to-run-some-code-on-load-of-package
  - https://www.rdocumentation.org/packages/pkgmaker/versions/0.32.10/topics/onLoad
