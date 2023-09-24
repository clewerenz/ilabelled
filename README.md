
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

  - <code>i_missing_to_na()</code>: the recoding of values via <code>%in%</code> is very slow, but <code>==</code> does not work with na_values > 1. Hence, a solution is needed, which replaces the slow <code>%in%</code> function (C/C++ maybe).
    - added a new function <code>findMissing()</code> (helper.R) which calls C function via <code>.Call()</code>. Need further knowledge on how to use this in R package. Throws error <i>"findNaNumeric" not available for .Call() for package "i_labelled"</i> when calling function and <i>File ‘ilabelled/libs/ilabelled.so’: Found no calls to: ‘R_registerRoutines’, ‘R_useDynamicSymbols’</i> when devtools::check. Maybe functions must be compiled first.
      - look at output devtools::check() suggestions
      - https://www.r-bloggers.com/2014/02/three-ways-to-call-cc-from-r/
      - https://www.r-bloggers.com/2021/07/using-r-callhello-2/
  - custom r_bind function
    - checks if value labels (if present) match
    - checks if classes match
    - returns 'talking' output
  - <code>i_data_to_*</code>: make use of structure(.Data = "what i want") to not take the extra step of copying attributes
  - <code>i_as_factor()</code> as method
    - two options: 1. all i_labelled and character vars become factor; 2. only i_labelled vars become factor (boolean)
    - can be applied to data.frame and vars
    - add i_missing_to_na() to i_as_factor()
  - funciton for recoding data
    - <code>i_recode(x, args, label = NULL, labels = NULL, na_values = NULL, na_range = NULL, ...)</code>
    - <code>structure(x = .rec(x), label = label, labels = labels, na_range = na_range, na_values = na_values, ...)</code>
    - inspiration from case_match from dplyr <https://github.com/tidyverse/dplyr/blob/main/R/case-match.R>
    - args as list input - e.g. <code>list(1 = c(1,2), 2 = c(3,4,5))</code>
    - process recoding in internal <code>.rec()</code> function
    - for each recode argument generate logical vector
    - loop over data <code>for( i in seq(logical-vectors)){}</code> and recode values according to input
        - find solution to accelerate for loop (can i do it in C/C++?)
            - <http://adv-r.had.co.nz/C-interface.html>
            - <https://www.biostat.jhsph.edu/~rpeng/docs/interface.pdf>
