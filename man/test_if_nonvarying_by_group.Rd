% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yingtools2.R
\name{test_if_nonvarying_by_group}
\alias{test_if_nonvarying_by_group}
\alias{group_suggest_additional_vars}
\alias{assert_grouping_vars}
\title{Test data for additional identifiers across groups.}
\usage{
test_if_nonvarying_by_group(
  data,
  id_vars = all_of(group_vars(data)),
  test_vars = everything(),
  verbose = FALSE
)

group_suggest_additional_vars(
  data,
  id_vars = all_of(group_vars(data)),
  test_vars = everything()
)

assert_grouping_vars(
  data,
  id_vars = all_of(group_vars(data)),
  test_vars = everything(),
  stopIfTRUE = FALSE
)
}
\arguments{
\item{data}{data to be tested (data.frame or data.table).}

\item{id_vars}{\code{\link[tidyr:tidyr_tidy_select]{tidy-select}} ID vars that define the nonvarying
groups. Default is to use the grouping variables (from \code{\link[dplyr:group_by]{dplyr::group_by()}})}

\item{test_vars}{\code{\link[tidyr:tidyr_tidy_select]{tidy-select}} variables to be tested. Default is all columns not specified in \code{id_vars}.}

\item{verbose}{whether or not to display messages about the testing results}

\item{stopIfTRUE}{Whether to raise error is test fails. Default is \code{FALSE}: issue warning only.}
}
\description{
In a data frame that can be grouped based on one or more column identifier(s), use these functions to test if
other columns do not vary within each group. In other words, it tests if additional columns can be added
to the grouping definition and would not alter the grouping structure.
This can be useful as a way to determine additional identifiers to include,
when performing reshaping operations such as \code{group_by}/\code{summarize} or \code{pivot_wider}.
\itemize{
\item \code{test_if_nonvarying_by_group()} returns testing results in the form of a named logical vector.
\item \code{group_suggest_additional_vars()} prints out the testing results and copies nonvarying vars to clipboard.
Use primarily as an aide during coding.
\item \code{assert_grouping_vars} performs testing and passes the original data frame if all tested
columns pass (are nonvarying across groups). If a column fails, a warning or error will be issued.
Use this for error checking within pipelines.
}
}
\examples{
otu <- get.otu.melt(cid.phy)

# Returns output of testing results
otu \%>\% test_if_nonvarying_by_group(id_vars=sample,
                                    test_vars=c(Sample_ID,Patient_ID,Family,Genus))

# Copies variables that passed to clipboard
otu \%>\% group_by(otu) \%>\% group_suggest_additional_vars()

# Issues warning that `test_vars` varies across `sample`
otu \%>\%
assert_grouping_vars(id_vars=sample,test_vars=c(Sample_ID,Consistency,Family,Phylum)) \%>\%
  group_by(sample,Sample_ID,Consistency,Family,Phylum) \%>\%
  summarize(totalseqs=sum(numseqs))
}
