% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/microbiota2.R
\name{get.taxonomy.colordata}
\alias{get.taxonomy.colordata}
\title{Calculate taxonomy colors}
\usage{
get.taxonomy.colordata(data, unitvar = Species, tax.palette = yt.palette3)
}
\arguments{
\item{data}{taxonomic data, can be \code{\link[phyloseq:phyloseq-class]{phyloseq}}, \code{\link[=get.otu.melt]{get.otu.melt()}} data frame, or \code{\link[=get.tax]{get.tax()}} data frame.}

\item{unitvar}{the granular column (bare unquoted) by which colors will be assigned. Default is \code{Species}.
Sometimes you might want to switch to another granular identifer, such as \code{otu}. Depending on the situation.}

\item{tax.palette}{a list of formulas used to assign colors. Each element should take the form: \verb{"<label>" = <true/false expression> ~ <color vector>}. See examples and details.}
}
\value{
a data frame with color values, where columns include \code{unit}=the distinct column ID for coloring (\code{unitvar}),
\code{name}=taxonomic label, \code{color}=assigned color. If a color was not used, it is included as a row in which \code{unit = NA}.
}
\description{
Generates taxonomy colors, suitable for plotting.
}
\details{
Used in \code{\link[=scale_fill_taxonomy]{scale_fill_taxonomy()}} to generate taxonomy colors.
}
