% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yingtools2.R
\name{fct_reordern}
\alias{fct_reordern}
\title{Reorder factor levels based on other variables.}
\usage{
fct_reordern(.f, ...)
}
\arguments{
\item{.f}{the factor to be re-ordered.}

\item{...}{variables to sort by}
}
\value{
the re-ordered factor
}
\description{
A \code{forcats}-like function which allows factor re-ordering based on sorting of multiple variables.
}
\details{
This is similar to \code{data \%>\% arrange(...) \%>\% mutate(.f=fct_inorder(.f))}. This was a feature
request in \code{forcats} but for some reason it was never added. If it is added I'll remmove this.
}
