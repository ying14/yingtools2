% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/microbiota2.R
\docType{data}
\name{yt.palette3}
\alias{yt.palette3}
\title{YT Palette 3}
\format{
An object of class \code{list} of length 11.
}
\usage{
yt.palette3
}
\description{
The customary palette for Bacteria.
}
\details{
Slightly different than \link{yt.palette2}; now everything cycles through shades.
Also accomodates the name changes of Bacteroidetes to Bacteroidota,  Clostridiales to Eubacteriales, Ruminococcaceae to Oscillospiraceae

\if{html}{\out{<div class="sourceCode {r}">}}\preformatted{#| echo: false
taxlegend3 <- get.tax.legend(tax.palette = yt.palette3, fontsize = 5)
grid::grid.newpage()
grid::grid.draw(taxlegend3)
}\if{html}{\out{</div>}}
}
\keyword{datasets}
