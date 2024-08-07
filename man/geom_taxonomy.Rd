% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/microbiota2.R
\name{geom_taxonomy}
\alias{geom_taxonomy}
\title{Stacked Taxonomy Bar}
\usage{
geom_taxonomy(
  mapping = NULL,
  data = NULL,
  stat = StatTaxonomy,
  position = "stack",
  ...,
  label.pct.cutoff = 0.3,
  label.split = FALSE,
  width = 0.95,
  na.rm = FALSE,
  parse = FALSE,
  tax.palette = yt.palette3,
  drop = FALSE,
  fit.text = TRUE,
  reflow = FALSE,
  contrast = FALSE,
  check_overlap = FALSE,
  show.ribbon = FALSE,
  ribbon.alpha = 0.35,
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

\item{label.pct.cutoff}{cutoff abundance by which to label abundances. Default is 0.3. Only applies if \code{fit.text=FALSE}.}

\item{label.split}{whether to split label into two lines, using \code{\link[=str_split_equal_parts]{str_split_equal_parts()}}}

\item{width}{Bar width. By default, set to 0.95.}

\item{na.rm}{If \code{FALSE}, the default, missing values are removed with
a warning. If \code{TRUE}, missing values are silently removed.}

\item{parse}{The same argument used in \code{\link[=geom_text]{geom_text()}}. Only applies if \code{fit.text=FALSE}.}

\item{tax.palette}{a list of formulas used to assign colors. Each element should take the form: \verb{"<label>" = <true/false expression> ~ <color vector>}. See \code{\link[=scale_fill_taxonomy]{scale_fill_taxonomy()}} for examples and details.}

\item{fit.text}{whether or not the tax labels will be auto-fitted. \code{TRUE}: use \code{\link[ggfittext:geom_fit_text]{ggfittext::geom_fit_text()}} (the default); \code{FALSE}: use \code{\link[ggplot2:geom_text]{ggplot2::geom_text()}}.}

\item{reflow}{Whether or not to reflow the text. Default is \code{FALSE}. Only applies if \code{fit.text=TRUE}.}

\item{contrast}{Whether or not to vary font color based on background fill. Default is \code{FALSE}. Only applies if \code{fit.text=TRUE}.}

\item{check_overlap}{The same argument used in \code{\link[=geom_text]{geom_text()}}. This is only applicable if if \code{fit.text=FALSE}.}

\item{show.ribbon}{whether or not to display transition lines in the background. Default is \code{FALSE}.}

\item{ribbon.alpha}{alpha value of the ribbon. Default is \code{0.25}.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}
}
\description{
Creates stacked taxonomy barplots.
}
\details{
This geom is similar to \code{\link[ggplot:geom_col]{ggplot::geom_col()}}, where you can draw stacked barplots.
However there are a few additional capabilities:
\enumerate{
\item You can specify the aesthetic \code{label} to write text labels inside the bars.
The text labels can be further tweaked using aesthetics (\code{fontsize}, \code{fontcolour}, \code{fontface}, \code{fontalpha}),
or other parameters (\code{fit.text}, \code{reflow}, \code{contrast}, \code{label.split}, \code{parse}, \code{check_overlap})
\item If Y-axis is transformed, the total Y bar height will be correct.
In \code{\link[ggplot:geom_col]{ggplot::geom_col()}}, stacking of individual transformed Y values often leads to strange results.
Here, the transformed Y totals for each bar is calculated and plotted.
Fill colors occupy the barspace proportionally.
}
}
\section{Aesthetics}{

\code{geom_taxonomy()} understands the following aesthetics (required aesthetics are in bold):
\itemize{
\item \strong{\code{\link[=aes_position]{x}}}
\item \strong{\code{\link[=aes_position]{y}}}
\item \code{\link[=aes_colour_fill_alpha]{alpha}}
\item \code{angle}
\item \code{\link[=aes_colour_fill_alpha]{colour}}
\item \code{family}
\item \code{\link[=aes_colour_fill_alpha]{fill}}
\item \code{fontalpha}
\item \code{fontcolour}
\item \code{fontface}
\item \code{fontsize}
\item \code{\link[=aes_group_order]{group}}
\item \code{hjust}
\item \code{label}
\item \code{lineheight}
\item \code{\link[=aes_linetype_size_shape]{linetype}}
\item \code{\link[=aes_linetype_size_shape]{linewidth}}
\item \code{sample.colour}
\item \code{sample.linetype}
\item \code{sample.linewidth}
\item \code{vjust}
}
Learn more about setting these aesthetics in \code{vignette("ggplot2-specs")}.
}

\examples{
library(tidyverse)
otu <- cid.phy \%>\%
  get.otu.melt() \%>\%
  filter(Patient_ID=="179")

# by sample
ggplot(data=otu,aes(x=sample,y=pctseqs,fill=otu,label=Genus)) +
  geom_taxonomy()

# by day
ggplot(data=otu,aes(x=day,y=pctseqs,fill=otu,label=Genus)) +
  geom_taxonomy(width=2)

# absolute abundance
ggplot(data=otu,aes(x=day,y=numseqs,fill=otu,label=Genus)) +
  geom_taxonomy(width=2)

# absolute abundance
ggplot(data=otu,aes(x=day,y=numseqs,fill=otu,label=Genus)) +
  geom_taxonomy(width=2) +
  scale_y_continuous(trans=log_epsilon_trans(100))

# if Y is transformed, colors are distributed proportionally
# (i.e. does not get messed up)
ggplot(data=otu,aes(x=day,y=numseqs,fill=otu,label=Genus)) +
  geom_taxonomy(width=2) +
  scale_y_continuous(trans=log_epsilon_trans(100))

# show ribbon transitions
ggplot(data=otu,aes(x=day,y=pctseqs,fill=otu,label=Genus)) +
  geom_taxonomy(width=2, show.ribbon = TRUE)

# highlight a taxon of interest
ggplot(data=otu,aes(x=day,y=pctseqs,fill=otu,label=Genus,
                    color=Genus=="Blautia")) +
  geom_taxonomy(width=2,linewidth=2) +
  scale_colour_manual(values=c("TRUE"="red","FALSE"="transparent"))
}
