% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yingtools2.R
\name{age.years}
\alias{age.years}
\title{Age in years}
\usage{
age.years(bdate, now)
}
\arguments{
\item{bdate}{\code{Date}, vector of birthdays.}

\item{now, }{\code{Date}, vector representing the time by which to calculate age.}
}
\value{
Returns the age in years.
}
\description{
Calculates REAL age in years.
}
\details{
Some people calculate age by taking age in days and dividing by 365.25. That approach can be inaccurate
because of leap years (that's weak!).
}
\examples{
age.years(as.Date("1975-02-21"),Sys.Date())
}
\author{
Ying Taur
}
