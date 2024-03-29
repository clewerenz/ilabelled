% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{\%lin\%}
\alias{\%lin\%}
\title{Match values in i_labelled data via value labels}
\usage{
x \%lin\% table
}
\arguments{
\item{x}{vector or NULL: the values to be matched. Long vectors are supported.}

\item{table}{vector or NULL: the values to be matched against. Long vectors are not supported.}
}
\description{
Find matches in vector (return T/F)
}
\examples{
x <- i_labelled(c(1:3,NA), labels = c("A" = 1, "B" = 2, "C" = 3), na_values = 3)
x \%lin\% "A"
}
