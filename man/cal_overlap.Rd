% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/overlap.R
\name{cal_overlap}
\alias{cal_overlap}
\title{calculate the overlaps between each group}
\usage{
cal_overlap(x, call = rlang::caller_env())
}
\arguments{
\item{x}{list. list of items}

\item{call}{vector. for error report, see \code{\link[rlang:stack]{rlang::caller_env()}}}
}
\value{
vector of overlaps between each group
}
\description{
calculate the overlaps between each group
}
\examples{
x <- list(x = 1:10, y = 5:20)
cal_overlap(x)

}
