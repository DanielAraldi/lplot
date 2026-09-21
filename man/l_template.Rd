\name{l_template}
\alias{l_template}
\alias{l_rect}
\alias{l_text}
\title{Compose Templates, Rectangles and Text with Grid Graphics}
\description{Construct reusable graphical content with native grid geometry and
graphical parameters. The returned grobs can be placed directly in lplot scenes.}
\usage{
l_template(..., children = NULL, name = NULL, gp = NULL, vp = NULL,
  childrenvp = NULL, cl = NULL)
l_rect(x = grid::unit(0.5, "npc"), y = grid::unit(0.5, "npc"),
  width = grid::unit(1, "npc"), height = grid::unit(1, "npc"),
  just = "centre", hjust = NULL, vjust = NULL, default.units = "npc",
  name = NULL, gp = NULL, vp = NULL, ...)
l_text(label, x = grid::unit(0.5, "npc"), y = grid::unit(0.5, "npc"),
  just = "centre", hjust = NULL, vjust = NULL, rot = 0,
  check.overlap = FALSE, default.units = "npc", name = NULL,
  gp = NULL, vp = NULL, ...)
}
\arguments{
\item{...}{For \code{l_template}, child grobs in drawing order. For
\code{l_rect} and \code{l_text}, uniquely named \code{grid::gpar} parameters
that override \code{gp}, such as \code{col}, \code{fill}, \code{alpha},
\code{lwd}, \code{lty}, \code{lineend}, \code{linejoin}, \code{linemitre},
\code{fontsize}, \code{fontfamily}, \code{fontface}, \code{lineheight},
\code{cex} and \code{lex}. Values are validated by grid.}
\item{children}{Optional list or \code{grid::gList} of grobs, appended after
children supplied in \code{...}. Nested templates and native grobs are supported;
lplot scene nodes are not grobs. An empty template is allowed.}
\item{name}{Optional grob name.}
\item{gp}{\code{NULL}, a \code{grid::gpar} object or a uniquely named list of
graphical parameters. A template's parameters are inherited by children, subject
to native grid inheritance rules; explicit child settings take precedence.}
\item{vp}{Native grid viewport, viewport name or viewport path, passed through
to the grob constructor. A viewport can control group position, dimensions,
justification, rotation, clipping and coordinate scales.}
\item{childrenvp}{Optional native viewport tree for template children.}
\item{cl}{Optional additional class for a template, as in \code{grid::grobTree}.}
\item{x, y}{Numeric coordinates in \code{default.units}, or grid unit objects.
Vectorized coordinates are supported.}
\item{width, height}{Rectangle dimensions as numbers in \code{default.units}
or grid unit objects. These are not lplot layout lengths.}
\item{just}{Character or numeric justification, as in the native grid constructor.}
\item{hjust, vjust}{Optional numeric horizontal and vertical justification,
overriding the corresponding component of \code{just}.}
\item{default.units}{Units used for numeric coordinates and rectangle dimensions;
defaults to normalized parent coordinates (\code{"npc"}).}
\item{label}{Text labels, including character vectors and mathematical expressions.}
\item{rot}{Text rotation in degrees.}
\item{check.overlap}{Whether grid should suppress overlapping text labels at draw time.}
}
\details{
These constructors do not draw, open devices or change global graphics settings.
They delegate to \code{grid::grobTree}, \code{grid::rectGrob} and
\code{grid::textGrob}. Parameters and children are not mutated.

Internal coordinates follow grid: with default units, (0, 0) is the bottom-left
and (1, 1) is the top-right of the current viewport. This differs from lplot's
top-left layout origin and logical pixel lengths. Use \code{\link{l_place}}
for outer position and dimensions, including percentages and responsive lengths.
Use \code{grid::unit} for physical or native coordinates inside a grob; strings
such as \code{"8px"} or \code{"50\%"} are not grid coordinates.

\code{gp} uses grid names such as \code{col} and \code{fontsize}, not
\code{l_style} names such as \code{color} and \code{font_size}. Graphical
parameters have native grid semantics, including multiplicative alpha and scale
factors where applicable. Font availability, fills and line rendering depend
on the output device. A \code{fontface} shortcut can override a \code{font}
stored in \code{gp}, and vice versa.

Pass grobs directly to \code{\link{l_viewport}}, \code{\link{l_place}} or
\code{\link{l_save}}, or wrap them in \code{\link{l_get_element}} to attach a
semantic type, metadata or responsive styles. A generic gTree has no automatic
child bounding-box measurement: specify layout width and height for templates.
}
\value{A native \code{gTree} for \code{l_template}, a \code{rect} grob for
\code{l_rect}, or a \code{text} grob for \code{l_text}. All inherit from \code{grob}.}
\seealso{\code{grid::grobTree}, \code{grid::rectGrob}, \code{grid::textGrob},
\code{grid::gpar}, \code{\link{l_place}}, \code{\link{l_get_element}}}
\examples{
badge <- l_template(
  l_rect(fill = "white", col = "#203C43", lwd = 1),
  l_text("N", fontsize = 12, fontface = "bold"),
  gp = list(col = "#203C43")
)
scene <- l_viewport(list(
  l_place(badge, right = 12, top = 12, width = 40, height = 50)
), width = 240, height = 120)
l_render(scene)

credits <- l_text("Source: survey", x = 0, just = "left",
  gp = list(fontfamily = "sans", fontsize = 8), col = "#50666C")
l_measure(credits)
}