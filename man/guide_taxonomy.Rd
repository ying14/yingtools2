% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/microbiota2.R
\name{guide_taxonomy}
\alias{guide_taxonomy}
\title{Taxonomy Guide}
\usage{
guide_taxonomy(
  title = waiver(),
  override.tax.palette = NULL,
  theme = NULL,
  position = NULL,
  direction = NULL,
  override.aes = list(),
  nrow = NULL,
  ncol = NULL,
  reverse = FALSE,
  order = 0,
  ...
)
}
\arguments{
\item{override.tax.palette}{Use tax palette, overriding data}
}
\description{
Used by \code{\link[=scale_fill_taxonomy]{scale_fill_taxonomy()}} to create a custom taxonomy legend.
Adapted from \code{\link[ggplot2:guide_legend]{ggplot2::guide_legend()}}.
}
