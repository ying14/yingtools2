% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yingtools2.R
\name{pos_lookahead}
\alias{pos_lookahead}
\alias{neg_lookahead}
\alias{pos_lookbehind}
\alias{neg_lookbehind}
\title{Positive/Negative Lookahead/Lookbehind}
\usage{
pos_lookahead(base, pattern)

neg_lookahead(base, pattern)

pos_lookbehind(base, pattern)

neg_lookbehind(base, pattern)
}
\arguments{
\item{base}{regex pattern to modify}

\item{pattern}{lookahead pattern to add}
}
\description{
Convenience function for generating regular expression with lookahead/lookbehind.
}
