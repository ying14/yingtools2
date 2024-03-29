% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yingtools2.R
\name{\%like\%}
\alias{\%like\%}
\alias{\%notlike\%}
\alias{\%ilike\%}
\alias{\%notilike\%}
\title{Regular Expression Operator}
\usage{
x \%like\% y

x \%notlike\% y

x \%ilike\% y

x \%notilike\% y
}
\description{
Shorthand operator for regular expression.
}
\examples{
fruit <- c("Apple", "Banana", "Pear", "Pineapple")
fruit \%like\% "A"
fruit \%notlike\% "A"
fruit \%ilike\% "A"
fruit \%notilike\% "A"
}
