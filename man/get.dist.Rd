% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/microbiota2.R
\name{get.dist}
\alias{get.dist}
\title{Convert pairwise distance table to distance matrix}
\usage{
get.dist(pw, sample1 = sample1, sample2 = sample2, dist = dist)
}
\arguments{
\item{pw}{a data frame of pairwise distances.}

\item{sample1}{column name (bare) of 1st sample column}

\item{sample2}{column name (bare) of 2nd sample column}

\item{dist}{column name (bare) of distance column}
}
\value{
corresponding distance matrix
}
\description{
This is the inverse function of \code{\link[=get.pairwise]{get.pairwise()}}
}
\examples{
pw <- get.pairwise(dist(mtcars))
get.dist(pw)
}
