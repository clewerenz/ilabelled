# ilabelled

Providing a function library for working with labelled data in R. 

Main goals to achieve:
  1. Do not change numeric data when data is labelled
  2. Do not loose attributes when recoding data or changing classes
  3. Easy access to applied labels (easy to see, easy to change)
  
The inspiration for this package comes from the [Sticky](https://github.com/cran/sticky) package as well as the [labelled](https://github.com/larmarange/labelled) package.

# To-Do

  1. Test for class equality between data and labels 
  2. i_as_factor behavior with single NA/NULL values
  3. control for NA in labels
