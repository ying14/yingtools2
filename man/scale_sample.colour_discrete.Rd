% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/microbiota2.R
\name{scale_sample.colour_discrete}
\alias{scale_sample.colour_discrete}
\title{Scale for sample.colour}
\usage{
scale_sample.colour_discrete(
  ...,
  h = c(0, 360) + 15,
  c = 100,
  l = 65,
  h.start = 0,
  direction = 1,
  na.value = "grey50",
  aesthetics = "sample.colour"
)
}
\description{
In \code{\link[=geom_taxonomy]{geom_taxonomy()}}, used as default scale for aesthetic \code{sample.colour}. Same as \code{\link[ggplot2:scale_hue]{ggplot2::scale_colour_hue()}}.
}
\details{
If \code{sample.colour} is specified as an aesthetic, the layer will search for a default scale by name
(\code{ggplot2:::Layer$compute_aesthetics} > \code{ggplot2:::scales_add_defaults} > \code{ggplot2:::find_scale}).
}
