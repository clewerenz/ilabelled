This is the development repository for the R package ilabelled.

# ilabelled

A function library for working with labelled data in R. This package introduces the data class 'i_labelled' for atomic vector objects which can be used within data.frames.

## Installation

Install from CRAN <code>install.packages('ilabelled')</code> or the development version from this github repository <code>remotes::install_github('https://github.com/clewerenz/ilabelled')</code>

At this state of development the package version on github is ahead of CRAN's version, since the package is still under development in order to take specific use cases into account.

## Context

R is a great software for working with data. However, there is a non-negligible disadvantage when it comes to handling categorical data and meta-information, such as variable labels or scale levels, and the like, as known from other statistical software, especially SPSS.

The aim of this package is to introduce a data class in which meta information (variable-label, value-labels, missing-values, scale-level) can and should be directly included. In addition, one of the biggest disadvantages of the base R factor class is to be addressed: Numerical values are not changed if they are provided with value labels. Negative and non-sequential consecutive values are not changed. In other words: the underlying database is not changed by labeling. Nevertheless, the option of using value labels for indexing is offered. 

An intuitive syntax enables easy access to package-specific functions. All functions available for data manipulation can be controlled by an “i_*”.

### Motivation

  - <b>Data consistency</b> 
    - Allow labeling of values <= 0 without altering them
    - Allow labeling of non-sequential values without altering them
      - Do not change numeric data when data is labelled (as factors do) 
    - Do not loose attributes when recoding or subsetting data
  - <b>Easy access</b> 
    - Intuitive functions for handling labels and missing values 
    - Transparency: Show meta-information (e.g. value-labels, missing-values, etc.) when data is printed
  - <b>Metadata management</b>:
    - Metadata in form of attributes can easily be set and altered by dedicated functions.
  - <b>Easy validation</b> 
    - Missing and incorrect meta-information
  - <b>Reduce dependencies</b> 
    - The most possible minimum of third party dependencies. Right now the package depends on no third party libraries besides packages which come with the installation of R.

### Inspiration

The inspiration for this package was drawn from the [lfactors](https://github.com/pdbailey0/lfactors) package, the [labelled](https://github.com/larmarange/labelled) package and the [sticky](https://github.com/cran/sticky) package with the aim to bring dependencies on bloated third party libraries to zero. The self-understanding of this package is not to be better than comparable libraries, but be a lightweight and slightly more intuitive alternative.


