% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yingtools2.R
\name{custom_full_join}
\alias{custom_full_join}
\alias{custom_inner_join}
\alias{custom_left_join}
\alias{custom_right_join}
\title{Custom Join}
\usage{
custom_full_join(
  x,
  y,
  by = NULL,
  x.only.expr = .data,
  y.only.expr = .data,
  xy.expr = .data,
  xy.conflict.expr = .x
)

custom_inner_join(x, y, by = NULL, xy.expr = .data, xy.conflict.expr = .x)

custom_left_join(
  x,
  y,
  by = NULL,
  x.only.expr = .data,
  xy.expr = .data,
  xy.conflict.expr = .x
)

custom_right_join(
  x,
  y,
  by = NULL,
  y.only.expr = .data,
  xy.expr = .data,
  xy.conflict.expr = .x
)
}
\arguments{
\item{x}{first data frame}

\item{y}{second data frame}

\item{by}{join specification similar to \code{\link[dplyr:mutate-joins]{dplyr::inner_join()}}.}

\item{x.only.expr}{expression dealing with rows in \code{x} only. \code{.data} is the tibble of x-only rows.}

\item{y.only.expr}{expression dealing with rows in \code{y} only. \code{.data} is the tibble of y-only rows.}

\item{xy.expr}{expression dealing with rows in both \code{x} and \code{y}. \code{.data} is the tibble of x and y rows.}

\item{xy.conflict.expr}{expression dealing with columns with potential conflicts between \code{x}and \code{y}. \code{.x} and \code{.y} are vectors from \code{x} and \code{y}, and \code{.col} is the name of the column.}
}
\description{
Provides great customization over how to join 2 data frames.
}
\examples{
library(dplyr)
mt1 <- mtcars \%>\% rownames_to_column("car") \%>\% mutate(car2=paste(car,"2"),car3x=paste(car,"3")) \%>\%
  slice(1:20) \%>\% select(starts_with("car"),mpg:vs)
mt2 <- mtcars \%>\% rownames_to_column("car") \%>\% mutate(car2=paste(car,"2"),car3y=paste(car,"3")) \%>\%
  slice(10:32) \%>\% select(starts_with("car"),hp:carb) \%>\%
  mutate(across(.cols=where(is.numeric),.fns=~.x + 0.00001))
mt12 <- custom_full_join(mt1,mt2,by="car")
}
