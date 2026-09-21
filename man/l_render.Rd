\name{l_render}
\alias{l_render}
\alias{l_resolve}
\alias{l_measure}
\alias{l_as_grob}
\title{Measure, Resolve and Render Scenes}
\description{Resolve logical constraints in a transient context and draw native grid graphics.}
\usage{
l_resolve(object, width = NULL, height = NULL, dpi = 96)
l_measure(object, viewport = list(width = 800, height = 600), dpi = 96)
l_render(object, width = NULL, height = NULL, dpi = 96,
  newpage = TRUE, debug = FALSE)
l_as_grob(object, dpi = 96, debug = FALSE)
}
\arguments{
\item{object}{A scene, viewport, element, ggplot or grob.}
\item{width, height}{Optional positive root dimensions in logical pixels. Defaults to
the current viewport, or the scene reference size (800 by 600 for auto) without a device.}
\item{dpi}{Positive output DPI metadata. Logical pixels remain 1/96 inch.}
\item{viewport}{Measurement context list with width and height, optionally root_width and root_height.}
\item{newpage}{Whether to begin a new grid page before drawing.}
\item{debug}{Draw layout diagnostics including boxes, anchors and collision candidates.}
}
\details{l_resolve performs no drawing. A temporary null PDF device is opened and closed
if font measurement needs a device. l_measure reports a node's constrained size and
intrinsic dimensions; viewports use their declared or available size rather than inferring
a bounding box from absolutely positioned children. Existing viewport stacks are preserved.
l_as_grob returns a lazy grob, resolving again at draw time; grid.draw also accepts nodes
directly. l_render records a lazy scene as well, so device replay resolves automatic
dimensions against the new viewport size. Explicit width or height stays fixed on that
axis. An IDE displaying a frozen bitmap must request a redraw for this to take effect.
l_render returns a snapshot of the initial resolved layout invisibly for inspection;
that returned value is not updated by later device replays. Resolved layouts
are context-specific and should not replace the logical scene for subsequent exports.
Unresolved overflow and collisions emit structured conditions instead of silently clipping.
Automatic text wrapping and breakpoint syntax are not implemented.}
\value{l_resolve returns an \code{l_layout} with root and context fields. Each resolved
node has a box (x, y, width, height), children, intrinsic size and collision_candidates.
l_measure returns width, height, intrinsic and units. l_render invisibly returns a layout.
l_as_grob returns a lazy grid gTree.}
\examples{
scene <- l_viewport(list(l_place(grid::rectGrob(), width = "50\%", height = "50\%")))
l_resolve(scene, 256, 256)$root$children[[1]]$box
l_measure(grid::textGrob("Map title"))
grid::grid.draw(l_as_grob(scene))
}
