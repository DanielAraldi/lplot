\name{l_viewport}
\alias{l_viewport}
\alias{l_join}
\alias{l_place}
\title{Construct and Place Scene Nodes}
\description{Build immutable scenes with arbitrary positioning and local child coordinates.}
\usage{
l_viewport(plots = list(), width = "auto", height = "auto", units = "px",
  padding = 0, margin = 0, background = NULL, responsive = TRUE,
  safe_area = NULL, ...)
l_join(plots, width = "auto", height = "auto", position = NULL,
  gap = 0, background = NULL, ...)
l_place(object, x = NULL, y = NULL, width = "auto", height = "auto",
  top = NULL, right = NULL, bottom = NULL, left = NULL,
  anchor = "top-left", z_index = 0, ...)
}
\arguments{
\item{plots}{List of ggplots, grobs, extracted elements or nested viewports. A single graphics object is also accepted.}
\item{object}{A graphics object or existing scene node.}
\item{width, height}{Logical border-box dimensions. Root dimensions are offline references, not fixed render sizes.}
\item{units}{Unit for numeric viewport width and height.}
\item{padding, margin}{One to four logical lengths in top/right/bottom/left order, or a named list.}
\item{background}{Background fill color, or NULL.}
\item{responsive}{Whether collision-driven shrinking is permitted; logical lengths still resolve on every draw.}
\item{safe_area}{Insets reserved for automatically placed children.}
\item{position}{Named list of node placement properties.}
\item{gap}{Spacing for row or column flow; ignored for absolute positioning.}
\item{x, y}{Anchor coordinates, measured from the top-left.}
\item{top, right, bottom, left}{Edge insets; opposing insets derive auto dimensions.}
\item{anchor}{One of top-left, top-center, top-right, center-left, center, center-right, bottom-left, bottom-center, bottom-right.}
\item{z_index}{Drawing order; higher values draw later.}
\item{...}{Named node properties: min_width, max_width, min_height, max_height,
aspect_ratio, id, overflow (visible/hidden/inherit), collision (none/avoid/shrink/avoid-and-shrink),
priority, allow_move, candidates, border, flow (absolute/row/column/stack), metadata,
or other placement/presentation properties shown above. Unknown properties are errors.}
}
\details{Unspecified placement arguments preserve existing constraints; explicit NULL clears
an optional constraint. Percentages use the containing content box, and positive y points down.
Padding and borders are inside the declared dimensions; margins are outside.
Conflicting coordinates/insets are errors. Root margins apply only when the root is nested.
Automatic collision movement is opt-in, with fixed positions protected unless allow_move
is TRUE. Shrink also requires a minimum dimension and respects a 10 percent safety floor.
Registry-independent layout treats ordinary sibling boxes as obstacles, but excludes
whole plots and panel/background elements unless metadata$obstacle overrides that choice.
Child viewports are resolved locally; joining never flattens them into a coordinate table.}
\value{An S3 \code{l_viewport} (also \code{l_scene} and \code{l_node}), or a positioned \code{l_node}.}
\examples{
scene <- l_viewport(list(
    l_place(grid::rectGrob(), left = "10\%", top = "10\%",
      width = "80\%", height = "80\%")
), width = 400, height = 300)
l_resolve(scene, width = 800, height = 600)$root$children[[1]]$box
}
