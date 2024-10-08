% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/microbiota2.R
\name{get.phyloseq.from.melt}
\alias{get.phyloseq.from.melt}
\title{Convert melted OTU table to phyloseq object}
\usage{
get.phyloseq.from.melt(
  otu.melt,
  tax_ranks = TRUE,
  sample_vars = TRUE,
  sample_id = "sample",
  abundance_var = "numseqs",
  taxa_id = "otu"
)
}
\arguments{
\item{otu.melt}{table of taxa x sample abundances, similar to output of \code{get.otu.melt}}

\item{tax_ranks}{vector of taxonomic ranks to be included in \link[phyloseq:taxonomyTable-class]{tax_table}.
Default \code{TRUE} (include tax vars, and try to determine which ones)
\code{FALSE} no tax vars, or a character vector specifying col names to be included.}

\item{sample_vars}{whether to include sample variables in the data.
Can be \code{TRUE} (include sample vars, and try to determine which ones),
\code{FALSE} no sample vars, or a character vector specifying col names to be included.}

\item{sample_id}{sample ID variable. Default \code{"sample"}.}

\item{abundance_var}{abundance variable, used to fill \code{\link[phyloseq:otu_table-methods]{phyloseq::otu_table()}}. Default \code{"numseqs"}.}

\item{taxa_id}{taxa/OTU ID variable. Default \code{"otu"}.}
}
\value{
phyloseq object, generated from the \code{otu.melt} data.
}
\description{
Convert melted OTU table to phyloseq object
}
\examples{
library(phyloseq)
phy <- cid.phy
ranks <- rank_names(phy)
otu <- get.otu.melt(cid.phy)
phy2 <- get.phyloseq.from.melt(otu,tax_ranks=ranks)
phy
phy2
}
