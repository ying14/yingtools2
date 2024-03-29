% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yingtools2.R
\name{declare.args}
\alias{declare.args}
\title{Declare arguments in a function}
\usage{
declare.args(..., envir_ = parent.frame())
}
\arguments{
\item{...}{either named arguments or a function by which to assign defaults.}

\item{envir_}{Environment to declare the arguments. Default is calling environment.}
}
\value{
(nothing)
}
\description{
Convenience function used when creating or modifying function code.
}
\details{
Using this is like debug trace, except more unofficial. If anything is handled as quosure, you'll probably
need to put \code{quo()} around it. Also does not handle ellipses.
}
\examples{
declare.args(var=sentences, recodes=c("the"="[THE]","[.]$"="!!"), replace.grep)
}
