% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yingtools2.R
\name{copy.as.tribble}
\alias{copy.as.tribble}
\title{Copy to clipboard as tribble}
\usage{
copy.as.tribble(tbl, spaces = 1)
}
\arguments{
\item{tbl}{a data frame to be copied}

\item{spaces}{number of spaces between columns}
}
\value{
(invisibly) returns R code statement that creates the tribble data frame, and copies to clipboard.
}
\description{
Copy to clipboard as tribble
}
\examples{
copy.as.tribble(head(mtcars)) \%>\% cat()
}
