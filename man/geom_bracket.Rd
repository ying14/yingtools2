% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yingtools2.R
\name{geom_bracket}
\alias{geom_bracket}
\title{Draw Brackets}
\usage{
geom_bracket(
  mapping = NULL,
  data = NULL,
  stat = StatBracket,
  position = "identity",
  ...,
  na.rm = FALSE,
  bracket.width = unit(0.75, "char"),
  tip = "round",
  arrow = grid::arrow(angle = 20, length = bracket.width, ends = "both", type = "closed"),
  arrow.fill = NULL,
  lineend = "butt",
  linejoin = "round",
  parse = FALSE,
  check_overlap = FALSE,
  show.legend = NA,
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

\item{stat}{The statistical transformation to use on the data for this layer.
When using a \verb{geom_*()} function to construct a layer, the \code{stat}
argument can be used the override the default coupling between geoms and
stats. The \code{stat} argument accepts the following:
\itemize{
\item A \code{Stat} ggproto subclass, for example \code{StatCount}.
\item A string naming the stat. To give the stat as a string, strip the
function name of the \code{stat_} prefix. For example, to use \code{stat_count()},
give the stat as \code{"count"}.
\item For more information and other ways to specify the stat, see the
\link[ggplot2:layer_stats]{layer stat} documentation.
}}

\item{position}{A position adjustment to use on the data for this layer. This
can be used in various ways, including to prevent overplotting and
improving the display. The \code{position} argument accepts the following:
\itemize{
\item The result of calling a position function, such as \code{position_jitter()}.
This method allows for passing extra arguments to the position.
\item A string naming the position adjustment. To give the position as a
string, strip the function name of the \code{position_} prefix. For example,
to use \code{position_jitter()}, give the position as \code{"jitter"}.
\item For more information and other ways to specify the position, see the
\link[ggplot2:layer_positions]{layer position} documentation.
}}

\item{...}{Other arguments passed on to \code{\link[ggplot2:layer]{layer()}}'s \code{params} argument. These
arguments broadly fall into one of 4 categories below. Notably, further
arguments to the \code{position} argument, or aesthetics that are required
can \emph{not} be passed through \code{...}. Unknown arguments that are not part
of the 4 categories below are ignored.
\itemize{
\item Static aesthetics that are not mapped to a scale, but are at a fixed
value and apply to the layer as a whole. For example, \code{colour = "red"}
or \code{linewidth = 3}. The geom's documentation has an \strong{Aesthetics}
section that lists the available options. The 'required' aesthetics
cannot be passed on to the \code{params}. Please note that while passing
unmapped aesthetics as vectors is technically possible, the order and
required length is not guaranteed to be parallel to the input data.
\item When constructing a layer using
a \verb{stat_*()} function, the \code{...} argument can be used to pass on
parameters to the \code{geom} part of the layer. An example of this is
\code{stat_density(geom = "area", outline.type = "both")}. The geom's
documentation lists which parameters it can accept.
\item Inversely, when constructing a layer using a
\verb{geom_*()} function, the \code{...} argument can be used to pass on parameters
to the \code{stat} part of the layer. An example of this is
\code{geom_area(stat = "density", adjust = 0.5)}. The stat's documentation
lists which parameters it can accept.
\item The \code{key_glyph} argument of \code{\link[ggplot2:layer]{layer()}} may also be passed on through
\code{...}. This can be one of the functions described as
\link[ggplot2:draw_key]{key glyphs}, to change the display of the layer in the legend.
}}

\item{na.rm}{If \code{FALSE}, the default, missing values are removed with
a warning. If \code{TRUE}, missing values are silently removed.}

\item{bracket.width}{size of bracket edges. Default is \code{unit(0.75,'char')}.}

\item{tip}{The style of bracket to use. Can be one of: \verb{"round", "bare", "square", "ibeam", "arrow", "iarrow", "curly"}. Default is \code{"round"}.}

\item{arrow}{The style of arrow to use, if \code{tip="arrow"} or \code{tip="iarrow"}. Default selection is based on \code{bracket.width}.}

\item{arrow.fill}{Fill color to use for closed arrowheads, if \code{tip="arrow"} or \code{tip="iarrow"}.}

\item{lineend}{Line end style (round, butt, square).}

\item{linejoin}{Line join style (round, mitre, bevel).}

\item{parse}{If \code{TRUE}, the labels will be parsed into expressions and
displayed as described in \code{?plotmath}.}

\item{check_overlap}{If \code{TRUE}, text that overlaps previous text in the
same layer will not be plotted. \code{check_overlap} happens at draw time and in
the order of the data. Therefore data should be arranged by the label
column before calling \code{geom_text()}. Note that this argument is not
supported by \code{geom_label()}.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{inherit.aes}{}
}
\description{
Draw brackets in a ggplot.
}
\details{
This draws brackets by using grid-drawking methods from \code{\link[=geom_text]{geom_text()}}, \code{\link[=geom_segment]{geom_segment()}}, and \code{\link[=geom_curve]{geom_curve()}}.
}
\section{Aesthetics}{

\code{geom_bracket()} understands the following aesthetics (required aesthetics are in bold):
\itemize{
\item \strong{\code{\link[=aes_position]{x}}}
\item \strong{\code{\link[=aes_position]{y}}}
\item \strong{\code{\link[=aes_position]{xend}}}
\item \strong{\code{\link[=aes_position]{yend}}}
\item \code{\link[=aes_colour_fill_alpha]{alpha}}
\item \code{angle}
\item \code{\link[=aes_colour_fill_alpha]{colour}}
\item \code{family}
\item \code{flip}
\item \code{fontface}
\item \code{fontsize}
\item \code{\link[=aes_group_order]{group}}
\item \code{hjust}
\item \code{label}
\item \code{lineheight}
\item \code{\link[=aes_linetype_size_shape]{linetype}}
\item \code{\link[=aes_linetype_size_shape]{linewidth}}
\item \code{vjust}
}
Learn more about setting these aesthetics in \code{vignette("ggplot2-specs")}.
}

\examples{
library(tidyverse)
mt <- mtcars \%>\%
  mutate(mpg.group=cut2(mpg, n.splits=3, lvls=c("Low MPG", "Med MPG", "High MPG")))
g <- ggplot(mt) +
  geom_point(aes(x=mpg, y=hp, color=mpg.group), size=3)

# whole-data brackets
g + geom_bracket(aes(x=mpg, y=350), label="MPG values") +
  geom_bracket(aes(x=35, y=hp), label="HP values", flip=TRUE)

# brackets by group
g + geom_bracket(aes(x=mpg, y=350, label=mpg.group))

# adjust the brackets with padding
g + geom_bracket(aes(x=mpg - 0.25, xend=mpg + 0.25, y=350, label=mpg.group))

# brackets by group, vary height
g + geom_bracket(aes(x=mpg, y=ave(hp,mpg.group,FUN=max)+20, yend=ave(hp,mpg.group,FUN=max)+20, label=mpg.group))

# manually specified brackets
g + geom_bracket(aes(x=18, y=225, yend=350, label="Tier 1"),flip=TRUE) +
  geom_bracket(aes(x=18, y=220, xend=25,yend=150, label="Tier 2")) +
  geom_bracket(aes(x=26,xend=35, y=150, label="Tier 3"))

# customization
g + geom_bracket(aes(x=27, y=200, yend=350, label="200+ HP"), flip=TRUE,
  linewidth=2, color="purple", fontsize=7, angle=0, hjust=0, fontface="italic", bracket.width=unit(0.5,"in"))

# various tip styles
ggplot() +
  geom_bracket(aes(x=1, xend=5, y=7, label="round"), tip="round") +
  geom_bracket(aes(x=1, xend=5, y=6, label="bare"), tip="bare") +
  geom_bracket(aes(x=1, xend=5, y=5, label="square"), tip="square") +
  geom_bracket(aes(x=1, xend=5, y=4, label="ibeam"), tip="ibeam") +
  geom_bracket(aes(x=1, xend=5, y=3, label="arrow"), tip="arrow") +
  geom_bracket(aes(x=1, xend=5, y=2, label="iarrow"), tip="iarrow") +
  geom_bracket(aes(x=1, xend=5, y=1, label="curly"), tip="curly") +
  xlim(0,6)
}
