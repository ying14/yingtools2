% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yingtools2.R
\name{geom_timeline}
\alias{geom_timeline}
\title{Plot timeline bars}
\usage{
geom_timeline(
  mapping = NULL,
  data = NULL,
  stat = "timeline",
  position = "identity",
  ...,
  merge = FALSE,
  min.gap = 1,
  row.overlap = TRUE,
  check_overlap = FALSE,
  merge.gap = 1,
  linejoin = "mitre",
  na.rm = FALSE,
  show.legend = NA,
  parse = FALSE,
  inherit.aes = TRUE
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}}. If specified and
\code{inherit.aes = TRUE} (the default), it is combined with the default mapping
at the top level of the plot. You must supply \code{mapping} if there is no plot
mapping.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{stat}{The statistical transformation to use on the data for this
layer, either as a \code{ggproto} \code{Geom} subclass or as a string naming the
stat stripped of the \code{stat_} prefix (e.g. \code{"count"} rather than
\code{"stat_count"})}

\item{position}{Position adjustment, either as a string naming the adjustment
(e.g. \code{"jitter"} to use \code{position_jitter}), or the result of a call to a
position adjustment function. Use the latter if you need to change the
settings of the adjustment.}

\item{...}{Other arguments passed on to \code{\link[ggplot2:layer]{layer()}}. These are
often aesthetics, used to set an aesthetic to a fixed value, like
\code{colour = "red"} or \code{size = 3}. They may also be parameters
to the paired geom/stat.}

\item{merge}{whether or not to merge adjacent/overlapping bars into 1 row before plotting (using \code{\link[=group_by_time]{group_by_time()}}). Default is \code{TRUE}.
Note that merging only occurs if the bars are overlapping, and have the same label and fill.}

\item{min.gap}{The allowable gap before two distinct rows are fitted on the same row, expressed as a proportion of the X-axis length. Note that this different than direct use of \code{\link[=get.row]{get.row()}}}

\item{row.overlap}{whether or not the same row value can overlap. \code{TRUE}: each row value is always one row and can overlap, FALSE: rows do not overlap and can occupy several rows if necessary}

\item{check_overlap}{If \code{TRUE}, text that overlaps previous text in the
same layer will not be plotted. \code{check_overlap} happens at draw time and in
the order of the data. Therefore data should be arranged by the label
column before calling \code{geom_text()}. Note that this argument is not
supported by \code{geom_label()}.}

\item{merge.gap}{if \code{merge=TRUE}, the maximum distance between two bars that are merged. Default is \code{1}.}

\item{linejoin}{Line join style (round, mitre, bevel).}

\item{na.rm}{If \code{FALSE}, the default, missing values are removed with
a warning. If \code{TRUE}, missing values are silently removed.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{parse}{If \code{TRUE}, the labels will be parsed into expressions and
displayed as described in \code{?plotmath}.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}
}
\description{
Plot timeline items in the form of bars, such as medication administration over time. The X-axis represents time.
}
\details{
This custom geom uses \code{\link[=get.row]{get.row()}} to arrange the timeline events into rows, where aesthetics are mapped
to the function parameters:
\itemize{
\item \code{aes(xmin=)} is mapped to \code{get.row(start=)}
\item \code{aes(xmax=)} is mapped to \code{get.row(xstop=)}
\item \code{aes(label=)} is mapped to \code{get.row(row=)}
\item \code{aes(by=)} is mapped to \code{get.row(by=)}
}

Some data prepping is done prior to plotting:
\enumerate{
\item Merge any timeline events that overlap using \code{\link[=group_by_time]{group_by_time()}} (optional step if \code{merge=TRUE})
\item Pad each event by +/- 0.45 days so that they span the length of the days they occur on, and so single day events do not have zero width.
\item Determine Y = row position for all events, using \code{\link[=get.row]{get.row()}}, using above aesthetic mappings.
\item Calculate X midpoint of each timeline event, for plotting of the label.
If X-axis transformations are used (e.g. \code{\link[=scale_x_timebar]{scale_x_timebar()}}), note that merging (#1) and padding (#2) are done
on un-transformed data, whereas \code{get.row()} (#3) and midpoint (#4) are done after transformation. This is more
seamless compared with the hassle of doing manually.
}
}
\section{Aesthetics}{

\code{geom_timeline()} understands the following aesthetics (required aesthetics are in bold):
\itemize{
\item \strong{\code{\link[=aes_position]{xmin}}}
\item \strong{\code{\link[=aes_position]{xmax}}}
\item \code{\link[=aes_colour_fill_alpha]{alpha}}
\item \code{angle}
\item \code{by}
\item \code{\link[=aes_colour_fill_alpha]{colour}}
\item \code{family}
\item \code{\link[=aes_colour_fill_alpha]{fill}}
\item \code{fontalpha}
\item \code{fontcolour}
\item \code{fontface}
\item \code{\link[=aes_group_order]{group}}
\item \code{hjust}
\item \code{label}
\item \code{lineheight}
\item \code{\link[=aes_linetype_size_shape]{linetype}}
\item \code{\link[=aes_linetype_size_shape]{linewidth}}
\item \code{\link[=aes_linetype_size_shape]{size}}
\item \code{vjust}
}
Learn more about setting these aesthetics in \code{vignette("ggplot2-specs")}.
}

\examples{
library(tidyverse)
data <- cid.meds \%>\% filter(Patient_ID=="166")
# Standard timeline plot without much cleanup
ggplot(data) + geom_timeline(aes(xmin=startday,xmax=endday,
                                 label=med.clean,by=med.class,fill=med.class),alpha=0.7)

# Make it look nice and clean with merge and check_overlap, use min.gap to share rows where possible.
ggplot(data) + geom_timeline(aes(xmin=startday,xmax=endday,
                                 label=med.clean,by=med.class,fill=med.class),alpha=0.7,
                             merge=TRUE,check_overlap=TRUE,min.gap=0.15)

# To see all events separately (even same labelled events never overlap), use row.overlap=FALSE
ggplot(data) + geom_timeline(aes(xmin=startday,xmax=endday,
                                 label=med.clean,by=med.class,fill=med.class),alpha=0.7,
                             row.overlap = FALSE)

# To fit in as few rows as possible, try removing by, and set min.gap=0 and merge=TRUE.
ggplot(data) + geom_timeline(aes(xmin=startday,xmax=endday,
                                 label=med.clean,fill=med.class),alpha=0.7,
                             merge=TRUE,min.gap=0,check_overlap=TRUE)
}
