% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/microbiota2.R
\name{prune_unused_taxa}
\alias{prune_unused_taxa}
\title{Prune Unused Taxa from a phyloseq object}
\usage{
prune_unused_taxa(phy, verbose = FALSE)
}
\arguments{
\item{phy}{\code{\link[phyloseq:phyloseq-class]{phyloseq}} object}
}
\value{
a \code{\link[phyloseq:phyloseq-class]{phyloseq}} object, with empty taxa removed.
}
\description{
In a \code{\link[phyloseq:phyloseq-class]{phyloseq}} object, remove any taxa that are not used in the samples.
Consider using this after subsetting the samples.
}
\examples{
library(phyloseq)
physub <- cid.phy \%>\%
  subset_samples(Patient_ID=="301")
physub.clean <- prune_unused_taxa(physub)
physub
physub.clean
}
