# ilabelled

Providing a function library for working with labelled data in R. 

# Motivation

Main goals to achieve:
  1. <b>Data Consistency</b>
  	- Do not change numeric data when data is labelled (as factors do)
  	- Allow labelling of values <= 0
  	- Do not loose attributes when recoding or subsetting data
  2. <b>Easy access</b>
  	- Print labels when data is printed
  	- Intuitive functions for handling labels and missing values
  3. <b>Easy validation</b>
  	- missing labels
  	- incorrect labels
  5. <b>Easy recoding</b>
  4. <b>Reduce dependencies</b>
  	- The most possible minimum. Right now it depends on: cli, glue, lifecycle, methods, rlang, utils, vctrs 
  
The inspiration for this package comes from the [Sticky](https://github.com/cran/sticky) package as well as the [labelled](https://github.com/larmarange/labelled) package.

# To-Do

  - apply missing values/range -> i_missing_values() / i_missing_range()
  - set missing values NA -> i_missing_to_na()
    - as function: should applicable on i_labelled and data.frame
  - add i_missing_to_na() to i_as_factor()
  - custom r_bind function for package
    - checks if value labels (if present) match
    - checks if classes match
    - returns 'talking' output
  - i_as_factor as method
    - two options: 1. all i_labelled and character vars become factor; 2. only i_labelled vars become factor
    - can be applied to data.frame and vars
      

