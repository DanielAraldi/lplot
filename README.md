# lplot

A responsive scene graph for R graphics. Extract ggplot2 elements, place them
with logical constraints, and render nested compositions using native `grid`.
There is no patchwork or browser dependency.
Use `l_save()` to export scenes or plots to PNG, JPG/JPEG, SVG and WebP.

## Install and Try

From the package directory:

```sh
Rscript -e 'install.packages(c("ggplot2", "gtable", "testthat", "vdiffr", "svglite"))'
R CMD INSTALL .
```

```r
library(lplot)
library(ggplot2)

plot <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
  geom_point() + labs(title = "Vehicle efficiency", colour = "Cylinders") +
  theme_minimal()

title <- l_get_element(plot, "title", style = list(
  color = "#164E45", font_size = "clamp(10pt, 2.5vmin, 20pt)"
))
legend <- l_get_element(plot, "legend", style = list(
  background = "white", padding = "6px", border = "#DDDDDD"
))

scene <- l_viewport(list(
  l_place(l_without(plot, c("title", "legend")),
          left = 0, right = 0, top = 40, bottom = 0),
  l_place(title, x = "50%", top = 4, anchor = "top-center", z_index = 20),
  l_place(legend, right = 12, top = 60, z_index = 10)
), width = 800, height = 600, padding = 12, background = "white")

l_render(scene)
grid::grid.draw(scene)
l_resolve(scene, width = 1024, height = 768)$root$children[[2]]$box
```

An executable terrain composition is in [inst/examples/terrain.R](inst/examples/terrain.R):

```r
source(system.file("examples", "terrain.R", package = "lplot"))
scene <- terrain_scene()
l_save(scene, type = "png", dir = "exports", filename = "terrain",
  width = 1200, height = 700, dpi = 144)
```

Install the optional `ragg` dependency before using this PNG export.

## Three Cartographic Examples

[inst/examples/maps.R](inst/examples/maps.R) contains three independently callable
scenes. They use the North Carolina county polygons bundled with the optional `sf`
package, so no data download, API key or network connection is needed after installation.
Both map and terrain examples use the graphical constructors described below.

In the Positron R console, with the project root as the working directory:

```r
install.packages("sf") # Only if not already installed
pkgload::load_all(".")
source("inst/examples/maps.R")

lplot::l_render(map_scale_scene())
lplot::l_render(map_inset_scene())
lplot::l_render(map_join_scene())
```

Run the three rendering commands individually to inspect each plot:

1. `map_scale_scene()`: county map with a 200 km scale bar, a horizontal legend,
   title and subtitle, all positioned as separate lplot elements.
2. `map_inset_scene()`: enlarged central/eastern region with a 50 km scale bar,
   north arrow, title, legend and upper-left locator inset. The red rectangle in
   the inset is the exact projected extent displayed by the main map.
3. `map_join_scene()`: two independent map viewports joined side by side with
   `l_join()`. They display the `BIR74` and `BIR79` birth-count attributes from
   `sf::nc`, using identical extents and colour limits for comparison.

The main panels are extracted with `l_get_element(..., "panel")`; original title
and legend positions are therefore not duplicated. `map_sheet()` places extracted
titles, subtitles and legends, while `map_frame()` keeps the panel's aspect ratio.
Scale bars and north arrows use `l_template()`, `l_rect()` and `l_text()` to build
native grid grobs passed through `l_get_element()`.
All helpers live in the same script; sourcing it only defines functions and does
not open a graphics device or automatically draw all examples.

Coordinates use EPSG:32119 (NAD83 / North Carolina), in metres. Scale-bar width is
the requested projected distance divided by the displayed extent width, not a
fixed decorative width; it remains aligned when resized. These are projected grid
distances, not geodesic measurements. The north arrow follows geographic north
evaluated at the centre of the main map, accounting for the projection's local
orientation. The inset frame does not change the panel's coordinate proportions.

Use at least approximately 800 x 600 logical pixels for the two-map example, or
export to a device with an explicit size to avoid a narrow Plots panel:

```r
scene <- map_join_scene()
lplot::l_save(scene, type = "png", dir = "exports", filename = "joined-maps",
              width = 1200, height = 700)
```

For an installed package, replace the two loading commands with:

```r
library(lplot)
source(system.file("examples", "maps.R", package = "lplot"))
```

## Templates and Graphical Primitives

Create reusable graphical content without wrapping every style in `grid::gpar()`:

```r
badge <- l_template(
  l_rect(fill = "white", col = "#203C43", lwd = 1),
  l_text("N", fontsize = 12, fontface = "bold"),
  gp = list(col = "#203C43")
)
scene <- l_viewport(list(
  l_place(badge, right = 12, top = 12, width = 40, height = 50)
), width = 240, height = 120)
l_render(scene)
```

`l_template()` returns a native `gTree`, accepting grobs in `...` or in
`children = list(...)`. Templates can be nested and mixed with native grid grobs.
`l_rect()` and `l_text()` return native rectangle and text grobs, preserving grid
arguments such as `x`, `y`, `just`, `hjust`, `vjust`, `default.units`, `name` and
`vp`, plus rectangle dimensions or text rotation and overlap control.

All three accept `gp` as a `gpar` or named list. Templates share it with their
children using grid's inheritance rules. Rectangles and text also accept named
graphical parameters directly, overriding `gp`: `col`, `fill`, `alpha`, `lwd`,
`lty`, `fontsize`, `fontfamily`, `fontface`, `lineheight` and other `gpar` settings.
Use `vp = grid::viewport(...)` for group rotation, clipping and local coordinates.

Inside these grobs, numbers default to grid's `npc` units, with (0, 0) at the
bottom-left; `grid::unit()` is also supported. These are not lplot's top-left
pixel coordinates or percentage strings. Use `l_place()` for outer placement
and explicit template dimensions, or `l_get_element()` for semantic metadata
and responsive styles.

## Save Images

Install the codecs for the formats you need:

```r
install.packages(c("ragg", "svglite", "webp"))
```

```r
path <- l_save(scene, type = "png", dir = "exports", filename = "map",
               width = 900, height = 500, dpi = 192)
print(path)
l_save(scene, type = "jpeg", dir = "exports", filename = "map", quality = 90)
l_save(scene, type = "svg", dir = "exports", filename = "map")
l_save(scene, type = "webp", dir = "exports", filename = "map", quality = 90)
```

`plot` is an explicit ggplot, grob, element or logical scene. `type` is
case-insensitive; `jpg` and `jpeg` are aliases. `dir` is a directory, created
recursively if needed, and `filename` is a basename with an optional matching
extension. With only the required plot, defaults are PNG, the working directory,
filename `plot.png`, 800 by 600 logical pixels, 96 dpi and white background.

The helper returns an absolute path invisibly. Files are not overwritten unless
`overwrite = TRUE`; the previous file survives a rendering failure. Export uses
a temporary file and its own graphics device, then restores the caller's device.
It never saves a screenshot of the current pane or relies on its dimensions.

Width/height are logical pixels (1/96 inch), not necessarily raster file pixels:
900 by 500 at 192 dpi produces 1800 by 1000 pixels. SVG dimensions do not depend
on DPI. Raster exports allow at most 100 million pixels, with a WebP limit of
16383 pixels per axis. `quality` (0-100) controls lossy JPEG/WebP only.
PNG/SVG/WebP accept `background = "transparent"`; JPEG requires an opaque
background. Opaque scene backgrounds still cover transparent device backgrounds.
SVG preserves vector sources; raster map layers remain raster within the SVG.

Optional runtime dependencies: `ragg` for PNG/JPEG, `svglite` for SVG, and
`ragg` plus `webp` for WebP. No installation occurs automatically. Layout warnings
are not suppressed. PDF remains available through `pdf()` plus `l_render()`,
not through `l_save(type = "pdf")`.

## Layout Contract

- Coordinates start at the top-left; positive y points down.
- Numbers are logical pixels. `1px = 1/96in`; `1pt = 1/72in`.
  DPI controls raster output density, not the physical definition of a logical pixel.
- `%` uses the parent's content box; `vw`, `vh`, `vmin`, `vmax` use the root.
  Lengths also accept `mm`, `cm`, `in`, `auto`, and nested `min()`, `max()`, `clamp()`.
- Width/height describe the border box. Padding/borders are inside it; margins are outside.
  One to four edge values follow CSS order, or use a named list of sides.
- `auto` measures elements intrinsically; plots, panels and viewports fill available space.
  Opposing insets derive an `auto` dimension. Inconsistent constraints raise typed errors.
- Nine anchors align an element to `x`/`y`, or dock it when coordinates are absent.
  Insets pin edges, independently of the anchor. `x` cannot be mixed with left/right,
  nor `y` with top/bottom. Min/max and aspect ratio are resolved before placement.
- Root width/height are reference dimensions for resolution without a graphics device.
  Rendering defaults to the actual current viewport; explicit `l_render(width, height)`
  uses physical logical-pixel dimensions. Nested viewport declarations remain constraints.
- `l_join()` creates a parent, preserving child-local coordinates. Use explicit placement
  or `flow = "row"` / `"column"` with `gap`. The default is absolute positioning;
  `flow = "stack"` overlays children. Root margins only take effect when nested.
- Children draw by increasing `z_index`, with input order breaking ties.
  `overflow = "visible"` is the default; use `"hidden"` or `"inherit"` explicitly.
- `l_measure()` and `l_resolve()` do not draw or leave devices open. They use an existing
  device's font metrics, or a temporary null PDF device when no device exists.
- `l_as_grob()` returns a lazy grob that resolves again on each draw. Resolved layouts
  are inspection results for one context, never replacements for the logical scene.
- `l_render()` also records a lazy scene: when the graphics device replays it at a
  different size, automatic dimensions are recalculated. Explicit `width`/`height`
  remain fixed on their respective axes. Its returned layout describes the initial
  draw only. Frozen IDE previews still need a redraw request.

## Elements and Styles

Native extraction supports title, subtitle, caption, tag, legend, axes, axis titles,
panel, strip, plot background and panel background. The original plot is unchanged.
Use `l_without()` to remove components explicitly; removing entire data panels is
intentionally rejected. Missing elements raise `lplot_missing_element`, or return
`NULL` with `missing = "null"`. Multiple matches preserve their gtable arrangement;
use `which = 1` to select a single match.

Typography inherits the captured source theme. Supported overrides include `color`
(`colour`), `font_family`, `font_size`, `font_face`, `line_height`, `alpha`, `opacity`,
`padding`, `margin`, `background` and `border`. Background elements and generic grobs
also support `fill`, `line_width`, `line_type`. Legends additionally accept
`legend.direction`, `legend.key_width`, `legend.key_height`. Unknown styles are errors,
not silently ignored. Whole plots/viewports accept presentation properties only:
background, border, margin and padding. Borders accept a color or a list containing
`color`, `width` and `line_type`.

Custom registries are explicit, immutable values, not global registrations:

```r
registry <- l_register_element(
  "badge", can_extract = is.character,
  extract = function(plot) grid::textGrob(plot),
  measure = function(element, context) c(width = 80, height = 24)
)
badge <- l_get_element("Survey area", "badge", registry = registry)
```

Callbacks are `extract(plot, ...)`, `can_extract(plot, ...)`,
`measure(element, context)` and `style(element, style, context)`.
Measurers receive prepared content and return width/height in logical pixels.
Style callbacks receive resolved style lengths and return a grob. `extractor` and
`measurer` are aliases. Pass an existing registry when adding another type.
Direct grobs support north_arrow, scale_bar, map_frame, credits, annotation, inset
and custom types. No cowplot adapter is required for core functionality.

## Automatic Placement

Set `collision = "avoid"`, `"shrink"` or `"avoid-and-shrink"` explicitly.
Higher priorities are placed first; ties use input order. Explicit coordinates/insets
are fixed unless `allow_move = TRUE`. Shrinking additionally requires `responsive = TRUE`
and an explicit `min_width` or `min_height`; it scales content uniformly with a 10%
lower safety bound, retaining padding and borders. Candidate anchors can be supplied
using `candidates`. The attempted boxes are available in each resolved node's
`collision_candidates`.

Safe areas constrain automatic positioning. Base plots and panel/background elements
are not obstacles; other sibling boxes are. Override using
`metadata = list(obstacle = TRUE)` or `FALSE`. This is rectangle-based placement,
not analysis of the data painted inside a map. Impossible placement keeps the
preferred box and emits `lplot_collision`; overflow emits `lplot_overflow`.

`l_render(scene, debug = TRUE)` shows boxes, margins, content, intrinsic bounds,
anchors, IDs, coordinates, z-order and collision candidates.

## Validation

```sh
Rscript -e 'testthat::test_local(reporter = "summary")'
NOT_CRAN=true Rscript -e 'testthat::test_local(filter = "visual")'
R CMD build .
R CMD check --no-manual lplot_0.1.0.tar.gz
```

Tests cover lengths, extraction/theme inheritance, style isolation, constraints,
nesting/joining, four output sizes, collision policy, rendering and visual references.
Export tests decode JPEG/PNG/WebP, inspect SVG and check device restoration,
file safety and dimensions. Run `testthat::test_local(filter = "save")`;
full codec tests additionally use optional `png` and `jpeg` image readers.
Real map exports in PNG, JPG, JPEG, SVG and WebP are kept in
[tests/testthat/\_snaps/save](tests/testthat/_snaps/save). To compare fresh exports
against these references, run:

```sh
NOT_CRAN=true Rscript -e 'testthat::test_local(filter = "save", reporter = "summary")'
```

These snapshots also require `sf`. Raster checks compare decoded pixels and
dimensions; SVG checks compare its textual representation. Changed snapshots
fail instead of replacing approved references.

Visual snapshots use optional test dependencies `vdiffr` and `svglite`. There is no persisted
cross-device measurement cache; prepared content is reused by the renderer within
one resolution. Optimize further only against measured workloads.

## Deliberate Boundaries

This release does not implement breakpoint/media-query syntax, automatic text wrapping,
a browser CSS engine, or geographic calculations for scale bars/north arrows.
Supply those symbols as grobs or extraction adapters. Content that cannot fit produces
diagnostics rather than silent clipping.
Package maintainer metadata currently uses a placeholder address; replace it before publication.
