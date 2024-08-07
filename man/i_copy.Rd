% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/copy.R
\name{i_copy}
\alias{i_copy}
\title{copy meta information from one variable to another}
\usage{
i_copy(to, from, what = "all", ...)
}
\arguments{
\item{to}{vector}

\item{from}{vector}

\item{what}{character vector describing which labels are copied: 'all' (default), 'label', 'labels', 'na_values', 'na_range'}

\item{...}{further attributes passed to structure}
}
\value{
Returns 'to' with ilabelled attributes copied from 'from'
}
\description{
copy meta information from one variable to another
}
