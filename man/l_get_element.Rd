\name{l_get_element}
\alias{l_get_element}
\alias{l_without}
\alias{l_style}
\alias{l_registry}
\alias{l_register_element}
\title{Extract, Style and Register Semantic Graphics Elements}
\description{Extract graphical components without modifying their source plot or unrelated elements.}
\usage{
l_get_element(plot, type, width = "auto", height = "auto", style = NULL,
  position = NULL, responsive = TRUE, collision = "none", priority = 0,
  registry = l_registry(), which = NULL, missing = c("error", "null"), ...)
l_without(plot, type)
l_style(object, ...)
l_registry()
l_register_element(type, can_extract = NULL, extract = NULL, measure = NULL,
  style = NULL, registry = l_registry(), extractor = NULL, measurer = NULL)
}
\arguments{
\item{plot}{Explicit source ggplot, grob, or source accepted by a custom adapter.}
\item{type}{Semantic element type; l_without also accepts a vector of types.}
\item{width, height}{Logical border-box dimensions.}
\item{style}{For extraction, a named list of element-specific overrides. For registration,
an optional function(element, style, context) returning a grob.}
\item{position}{Named list of placement properties.}
\item{responsive, collision, priority}{Node responsiveness and collision policy; see \code{\link{l_place}}.}
\item{registry}{An explicit registry value, returned by l_registry or l_register_element.}
\item{which}{Optional one-based selection when several components match.}
\item{missing}{Raise a structured error or return NULL for an absent element.}
\item{object}{An element or scene node to style.}
\item{can_extract}{Predicate function(plot, ...) accepting a source; defaults to TRUE.}
\item{extract, extractor}{Extractor function(plot, ...) returning a grob; extractor is an alias.}
\item{measure, measurer}{Optional function(element, context) returning named width/height
in logical pixels. The element contains prepared, styled content. measurer is an alias.}
\item{...}{For extraction, node properties accepted by l_place. For l_style,
named style properties such as color, font_size, font_face, font_family,
line_height, alpha, opacity, padding, margin, background and border.
Generic grobs and background elements also accept fill, line_width and line_type;
legends support legend.direction, legend.key_width and legend.key_height.
Whole plots and viewports accept only background, border, margin and padding.}
}
\details{Native types are title, subtitle, caption, tag, legend, x_axis, y_axis,
x_axis_title, y_axis_title, panel, strip, plot_background and panel_background.
Multiple matches retain their source gtable arrangement. Direct grobs also support
north_arrow, scale_bar, map_frame, credits, annotation, inset and custom.
The effective theme is captured on extraction; later global theme changes do not restyle
the element. Styles are isolated to the selected semantic component. Native axis styles
target axis text. Responsive typography is resolved per render context.
The default absence condition is lplot_missing_element. Unknown extractors and unsupported
sources have distinct lplot_error subclasses. l_without never mutates the source and
does not support removal of whole data panels. Registrations return a modified registry
without changing global state.}
\value{An \code{l_element}, a modified copy of the plot or node, NULL for permitted
absence, or an \code{l_registry}, depending on the function.}
\examples{
plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
  ggplot2::geom_point() + ggplot2::labs(title = "Efficiency")
title <- l_get_element(plot, "title", style = list(color = "blue"))
l_measure(title)
registry <- l_register_element("badge", extract = function(plot) grid::textGrob(plot))
l_get_element("Survey", "badge", registry = registry)
}
