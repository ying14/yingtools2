% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yingtools2.R
\name{tab}
\alias{tab}
\title{Tabulate}
\usage{
tab(var, sort = TRUE, pct = TRUE, as.char = FALSE, collapse = "\\n")
}
\arguments{
\item{var}{the vector to be tabulated}

\item{as.char}{logical specifying whether to return tabulation as a single character. Useful for summarizing data within grouping commands such as \code{ddply} or \code{group_by}/\code{summarize}}

\item{sortby.freq}{if \code{TRUE}, sorts by order}

\item{useNA}{character specifying whether to tally \code{NA} values. This is passed to \code{tabulate}}
}
\value{
Returns a data frame with tabulations.
}
\description{
Tabulates frequencies of vectors. By default, sorts by frequency.
}
\author{
Ying Taur
}
