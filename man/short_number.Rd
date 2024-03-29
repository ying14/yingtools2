% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yingtools2.R
\name{short_number}
\alias{short_number}
\title{Short number formatting}
\usage{
short_number(x, abbrev = c(K = 3, M = 6, B = 9), sig.digits = 3)
}
\arguments{
\item{x}{numeric vector to be formatted}

\item{abbrev}{named vector specifying the log base 10 cutoff values and their assigned label. Default is \code{c(K=3,M=6,B=9)}.}

\item{sig.digits}{number of signficant digits to use.}
}
\value{
character vector of formatted numbers
}
\description{
Use to abbreviate large numbers (e.g. 3450000 is '3.4M')
}
\examples{
short_number(pi*10^(-1:10))
}
