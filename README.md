# ilabelled

Providing a function library for working with labelled data in R. 

Main goals to achieve:
  1. Do not change numeric data when data is labelled
  2. Do not loose attributes when recoding data or changing classes
  3. Easy access to applied labels (easy to see, easy to change)
  
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
      

