% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yingtools2.R
\name{dt}
\alias{dt}
\title{Ying's DT view}
\usage{
dt(
  data,
  fontsize = 14,
  pageLength = Inf,
  maxchars = 250,
  maxrows = 500,
  rownames = FALSE,
  escape = FALSE,
  class = "compact cell-border stripe",
  whiteSpace = "pre-wrap"
)
}
\arguments{
\item{data}{dataframe to be viewed.}

\item{fontsize}{numeric controlling font size in the table, measured in px. Default is 11.}

\item{pageLength}{number of rows to display per page (Default \code{Inf}, show all rows)}

\item{maxchars}{max number of characters before adding an ellipsis \code{...}. Default is 250.}

\item{maxrows}{numeric controlling max number of rows to display. The purpose is to prevent \code{DT} from handling excessively large data frames. Default is 1000.}

\item{rownames}{whether or not to show row names (passed directly to \code{\link[DT:datatable]{DT::datatable()}}).}

\item{escape}{whether to escape HTML entities in the table (passed directly to \code{\link[DT:datatable]{DT::datatable()}}).}

\item{class}{the CSS class(es) of the table (passed directly to \code{\link[DT:datatable]{DT::datatable()}}).}

\item{whiteSpace}{CSS property sets how white space inside an element is handled. Default is "pre-wrap".}
}
\value{
A javascript-style datatable, which displays in the Rstudio viewer.
}
\description{
Use to peruse a dataframe within RStudio. Utilizes \code{DT} package.
}
\details{
If data frame is grouped (i.e. \code{group_by} in dplyr), the rows will be sorted and shaded by group.
}
\examples{
library(dplyr)
mtcars \%>\% dt()
mtcars \%>\% group_by(cyl) \%>\% dt()
}
\author{
Ying Taur
}
