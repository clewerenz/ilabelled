# ilabelled

A function library for working with labelled data in R. This package introduces the data class 'i_labelled' for atomic vector objects which can be used within data.frames.

## Description

R is a great software for working with data. However, there is a non-negligible disadvantage when it comes to handling categorical data and meta-information, such as variable labels or scale levels, and the like, as known from other statistical software, especially SPSS.

The aim of this package is to introduce a data class in which meta information (variable-label, value-labels, missing-values, scale-level) can and should be directly included. In addition, one of the biggest disadvantages of the base R factor class is to be addressed: Numerical values are not changed if they are provided with value labels. Negative and non-sequential consecutive values are not changed. In other words: the underlying database is not changed by labeling. Nevertheless, the option of using value labels for indexing is offered. 

An intuitive syntax enables easy access to package-specific functions. All functions available for data manipulation can be controlled by an “i_*”.

## Motivation

  - <b>Data consistency</b> 
    - Allow labeling of values <= 0
    - Allow labeling of non-sequential values
      - Do not change numeric data when data is labelled (as factors do) 
    - Do not loose attributes when recoding or subsetting data
  - <b>Easy access</b> 
    - Intuitive functions for handling labels and missing values 
    - Transparency: Show meta-information (e.g. value-labels, missing-values, etc.) when data is printed
  - <b>Easy validation</b> 
    - Missing and incorrect meta-information
  - <b>Reduce dependencies</b> 
    - The most possible minimum of third party dependencies. Right now the package depends on no third party libraries.

## Inspiration

The inspiration for this package was drawn from the [lfactors](https://github.com/pdbailey0/lfactors) package and the [labelled](https://github.com/larmarange/labelled) package with the aim to bring dependencies on bloated third party libraries to zero. The self-understanding of this package is not to be better than comparable libraries, but be a lightweight and slightly more intuitive alternative.


