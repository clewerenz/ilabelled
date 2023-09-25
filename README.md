
# ilabelled

Providing a function library for working with labelled data in R.

# Motivation

  - <b>Data consistency</b> 
    - Do not change numeric data when data is labelled (as factors do) 
    - Allow labelling of values <= 0 
    - Do not loose attributes when recoding or subsetting data
  - <b>Easy access</b> 
    - Print labels when data is printed - Intuitive functions for handling labels and missing values 
  - <b>Easy validation</b> 
    - missing labels 
    - incorrect labels 
  - <b>Easy recoding</b> 
  - <b>Reduce dependencies</b> 
    - The most possible minimum. Right now it depends on: cli, glue, lifecycle, methods, rlang, utils, vctrs

The inspiration for this package comes from the [Sticky](https://github.com/cran/sticky) package as well as the [labelled](https://github.com/larmarange/labelled) package.

# To-Do
  
  - tests for <code>i_missings_to_na()</code>
  - code>i_valid_labels()</code> should work on all data classes
  - custom r_bind function:
    - checks if value labels (if present) match
    - checks if classes match
    - returns 'talking' output
  - <code>i_data_to_*</code>: make use of structure(.Data = "what i want") to not take the extra step of copying attributes
  - <code>i_as_factor()</code> as method:
    - two options: 1. all i_labelled and character vars become factor; 2. only i_labelled vars become factor (boolean)
    - can be applied to data.frame and vars
    - add i_missing_to_na() to i_as_factor()
  - funciton for recoding data:
    - <code>i_recode(x, args, label = NULL, labels = NULL, na_values = NULL, na_range = NULL, ...)</code>
    - <code>structure(x = .rec(x), label = label, labels = labels, na_range = na_range, na_values = na_values, ...)</code>
    - inspiration from case_match from dplyr <https://github.com/tidyverse/dplyr/blob/main/R/case-match.R>
    - args as list input - e.g. <code>list(1 = c(1,2), 2 = c(3,4,5))</code>
    - process recoding in internal <code>.rec()</code> function
    - for each recode argument generate logical vector
    - loop over data <code>for( i in seq(logical-vectors)){}</code> and recode values according to input
      - find solution to accelerate for loop (can i do it in C/C++?)

# Memory
## Use C code in R

  - write code in \src\file.c
  - collect functions in header file under \src
  - when package is compiled or loaded via <code>devtools::load_all()</code>, C code get compiled as .so or .dll
  - to use C functions in R functions via <code>.Call()</code> add line to NAMESPACE: <code>useDynLib(ilabelled, .registration = TRUE)</code>
    - first argument is the name of the .so or .dll under \src you want to use in the package
  - <http://adv-r.had.co.nz/C-interface.html>
  - https://github.com/Rdatatable/data.table/tree/master
  - https://cran.r-project.org/doc/manuals/R-exts.html#Portable-C-and-C_002b_002b-code
  - https://stackoverflow.com/questions/1176455/portable-use-of-dyn-load-to-call-a-c-function-in-an-r-package
  - in order to compile C code manually use: https://rdrr.io/r/utils/SHLIB.html; https://rdrr.io/github/richfitz/rcmdshlib/man/shlib.html

## rund code when package is loaded

  - https://stackoverflow.com/questions/20223601/r-how-to-run-some-code-on-load-of-package
  - https://www.rdocumentation.org/packages/pkgmaker/versions/0.32.10/topics/onLoad
