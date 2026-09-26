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

## Independent Function Examples

There is one standalone script for each of the 19 exported functions in
[inst/examples/functions/](inst/examples/functions/). Each focuses on its named
function, using `l_text()` and `l_rect()` to prepare content, `l_unit()` for native
grid dimensions and `l_render()` to draw where applicable. Every script supplies its own inputs; no shared
utilities, other example scripts, sf installation or downloads are required.

From the project root, load the development package and choose a script:

```r
pkgload::load_all(".")
source("inst/examples/functions/l_get_element.R")
```

With lplot installed, the equivalent is:

```r
source(system.file("examples", "functions", "l_get_element.R", package = "lplot"))
```

Replace the filename to try another function. Unlike the map scene constructors,
sourcing these scripts runs the example immediately. Graphics appear on the
current device; inspection examples print their results to the console. Each
script stores its main return value in `result`, even when a subsequent
`l_render()` call draws it. Run the scripts separately, in any order.

| Function               | Script                                                                                       | Demonstration                                                    |
| ---------------------- | -------------------------------------------------------------------------------------------- | ---------------------------------------------------------------- |
| `l_unit()`             | [inst/examples/functions/l_unit.R](inst/examples/functions/l_unit.R)                         | Declare and print a native grid unit in millimetres.             |
| `l_length()`           | [inst/examples/functions/l_length.R](inst/examples/functions/l_length.R)                     | Declare and print a percentage length.                           |
| `l_clamp()`            | [inst/examples/functions/l_clamp.R](inst/examples/functions/l_clamp.R)                       | Declare minimum, preferred and maximum lengths.                  |
| `l_text()`             | [inst/examples/functions/l_text.R](inst/examples/functions/l_text.R)                         | Draw text with explicit typography.                              |
| `l_rect()`             | [inst/examples/functions/l_rect.R](inst/examples/functions/l_rect.R)                         | Draw a filled rectangle with an outline.                         |
| `l_template()`         | [inst/examples/functions/l_template.R](inst/examples/functions/l_template.R)                 | Combine a rectangle and text into one reusable object.           |
| `l_style()`            | [inst/examples/functions/l_style.R](inst/examples/functions/l_style.R)                       | Style a copy of a text grob.                                     |
| `l_registry()`         | [inst/examples/functions/l_registry.R](inst/examples/functions/l_registry.R)                 | List the built-in extraction adapters.                           |
| `l_register_element()` | [inst/examples/functions/l_register_element.R](inst/examples/functions/l_register_element.R) | Register a character-to-text adapter in an independent registry. |
| `l_get_element()`      | [inst/examples/functions/l_get_element.R](inst/examples/functions/l_get_element.R)           | Extract and draw only a ggplot legend.                           |
| `l_without()`          | [inst/examples/functions/l_without.R](inst/examples/functions/l_without.R)                   | Draw a copy of a ggplot without its title and legend.            |
| `l_place()`            | [inst/examples/functions/l_place.R](inst/examples/functions/l_place.R)                       | Position and size a rectangle using percentages.                 |
| `l_viewport()`         | [inst/examples/functions/l_viewport.R](inst/examples/functions/l_viewport.R)                 | Arrange text grobs in a padded local context.                    |
| `l_join()`             | [inst/examples/functions/l_join.R](inst/examples/functions/l_join.R)                         | Join two rectangles with horizontal spacing.                     |
| `l_as_grob()`          | [inst/examples/functions/l_as_grob.R](inst/examples/functions/l_as_grob.R)                   | Create and draw a deferred grid-compatible wrapper.              |
| `l_render()`           | [inst/examples/functions/l_render.R](inst/examples/functions/l_render.R)                     | Draw a text grob and retain the returned layout.                 |
| `l_measure()`          | [inst/examples/functions/l_measure.R](inst/examples/functions/l_measure.R)                   | Print the constrained and intrinsic dimensions of text.          |
| `l_resolve()`          | [inst/examples/functions/l_resolve.R](inst/examples/functions/l_resolve.R)                   | Inspect the root and child boxes in logical pixels.              |
| `l_save()`             | [inst/examples/functions/l_save.R](inst/examples/functions/l_save.R)                         | Export a text grob and print the output path.                    |

The `l_as_grob()` and `l_template()` examples use `l_place()` to give graphical
trees an explicit rendering area rather than relying on automatic intrinsic
measurement. The `l_rect()` example also uses an explicit area to preserve its
relative geometry. `l_unit()` creates native grid units, including physical
dimensions; direct `grid::unit()` calls remain supported.

The export example requires the optional **svglite** package
(`install.packages("svglite")`). It creates a new temporary directory on every
run, writes an SVG there and prints its absolute path. Change `dir` to retain
the file outside R's temporary directory. Existing files are not overwritten.

To test all 19 examples in isolated environments from the project root:

```sh
NOT_CRAN=true Rscript -e 'testthat::test_local(filter = "function-examples", reporter = "summary", stop_on_failure = TRUE)'
```

The four registered S3 methods support `print()`, `grid::grid.draw()` and
`grid::makeContent()`; they are invoked through those generics rather than
treated as additional exported lplot functions.

## Four Progressive Map Examples

[inst/examples/](inst/examples/) also provides four examples that progress from
a minimal map to a composed cartographic report, each in its own file. All use the
North Carolina counties bundled with the optional `sf` package; no external data
download is required. `ggplot2` draws the geographic layers and `lplot` composes
the elements. The third example additionally uses `sf` for reprojection and area
calculations.

From the project root, source each example file separately and run its rendering
command:

```r
pkgload::load_all(".")

source("inst/examples/simple.R")
lplot::l_render(map_simple_scene())

source("inst/examples/template.R")
lplot::l_render(map_template_scene())

source("inst/examples/sf.R")
lplot::l_render(map_sf_scene())

source("inst/examples/complex.R")
lplot::l_render(map_complex_scene())
```

Install `sf` first with `install.packages("sf")` if needed. For an installed
`lplot`, use `library(lplot)` and
`source(system.file("examples", "simple.R", package = "lplot"))` (and so on for
the other files) instead of the first two lines above. Sourcing a script only
defines functions; it does not draw. The simple example reads the bundled data
directly with `sf` and does not load utility scripts. The other examples load
shared data (`inst/examples/data/maps.R`) and rendering helpers
(`inst/examples/utils.R`). Source `simple.R` before `template.R` or `sf.R`,
which reuse `map_simple_scene()`.

1. **Simple map:** `map_simple_scene()` keeps the complete map in ggplot2, which
   controls its coordinates and proportions. Only the title and horizontal
   legend are extracted and positioned with lplot, with colour limits and breaks
   derived from the selected data. Change `field` or supply
   modified county data when constructing a new scene; no legend labels need to
   be maintained by hand. Its position is resolved again at each device size.
2. **Custom templates:** `map_template_scene()` reuses `map_label_template()` for
   the heading and two summary labels. This helper combines `l_template()`,
   `l_rect()` and `l_text()`. Content, accent colour and background change without
   duplicating graphical construction or map placement. Templates receive explicit
   outer dimensions, while their children use native grid coordinates.
3. **Spatial processing:** `map_sf_scene()` transforms counties to EPSG:32119,
   calculates projected area in square kilometres with `st_area()`, and maps
   1974 births per 100 square kilometres. This is an area-normalized count, not a
   population birth rate. An `sf` county object in another CRS can be supplied.
4. **Complex map:** `map_complex_scene()` combines a regional map, locator inset,
   georeferenced north arrow, 50 km projected scale, legend, reusable heading,
   summary labels, top-five bar chart and source note in nested viewports.
   Indicators and ranking include the **whole count** of every county intersecting
   the displayed extent; counts are not estimated for clipped polygon fragments.

Customize the data and template without changing the layout:

```r
lplot::l_render(map_simple_scene(field = "BIR79", title = "Nascimentos | 1979"))

scene <- map_template_scene(
  field = "BIR79", title = "Nascimentos | 1979",
  accent = "#A63748", background = "#FAF1F3"
)
lplot::l_render(scene)

counties <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
lplot::l_render(map_sf_scene(counties))
```

Use at least 600 x 400 logical pixels for the first three examples and 1000 x 700
for the complex composition. Export at an explicit size when the Plots pane is
smaller (PNG requires the optional `ragg` package):

```r
lplot::l_save(map_complex_scene(), type = "png", dir = "exports",
          filename = "complex-map", width = 1200, height = 800)
```

## Three Cartographic Examples

[inst/examples/](inst/examples/) also contains three independently callable
scenes, each in its own file (`scale.R`, `inset.R`, `join.R`). They use the North
Carolina county polygons bundled with the optional `sf` package, so no data
download, API key or network connection is needed after installation. Both map
and terrain examples use the graphical constructors described below.

In the Positron R console, with the project root as the working directory:

```r
install.packages("sf") # Only if not already installed
pkgload::load_all(".")

source("inst/examples/scale.R")
lplot::l_render(map_scale_scene())

source("inst/examples/inset.R")
lplot::l_render(map_inset_scene())

source("inst/examples/join.R")
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
These shared helpers live in `inst/examples/utils.R`; sourcing an example file only
defines functions and does not open a graphics device or automatically draw all
examples.

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
source(system.file("examples", "join.R", package = "lplot"))
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
bottom-left; `l_unit()` and `grid::unit()` objects are also supported. These are not lplot's top-left
pixel coordinates or percentage strings. Use `l_place()` for outer placement
and explicit template dimensions, or `l_get_element()` for semantic metadata
and responsive styles.

## Native Grid Units

`l_unit(x, units, data = NULL)` delegates directly to `grid::unit()` and returns
the same native `unit` object. It accepts the same arguments and uses grid's
validation and recycling rules, without adding a dependency or a new unit class:

- `x`: numeric vector of values.
- `units`: grid unit names, such as `"mm"`, `"cm"`, `"inches"`, `"points"`,
  `"npc"`, `"native"`, `"lines"` or `"char"`. Unit names can also be a vector.
- `data`: optional text, expression, grob, grob path or list for units that
  require it, such as `"strwidth"`, `"strheight"`, `"grobwidth"` and `"grobheight"`.
  The default is `NULL`.

```r
spacing <- l_unit(5, "mm")
identical(spacing, grid::unit(5, "mm")) # TRUE
l_unit(c(0.25, 0.75), "npc")
l_unit(c(1, 5), c("cm", "mm"))
l_unit(1, "strwidth", data = "Survey area")

outline <- l_rect(
  width = l_unit(20, "mm"), height = l_unit(10, "mm"),
  fill = NA, col = "black"
)
label <- l_text("Survey", x = spacing, just = "left")
l_unit(1, "grobwidth", data = label)
l_unit(1, "npc") - spacing
```

Use these objects inside `l_rect()`, `l_text()`, template children and native
grid grobs or viewports. Native arithmetic, indexing and `grid::unit.c()` work
unchanged. Construction does not draw, open a device or convert to a fixed
numeric size. Grid evaluates relative units and font/grob measurements in the
applicable viewport and device; use `grid::convertUnit()`, `grid::convertWidth()`
or `grid::convertHeight()` for explicit conversions in that context.

`l_unit(0.5, "npc")` means half a native grid viewport; `l_length("50%")` is an
lplot layout length. Use `l_length()` or layout strings for `l_place()` and
`l_viewport()` constraints. Grid units do not accept CSS-like `"px"`, `"%"`,
`"vw"`, `"auto"` or `"clamp(...)"`. Invalid input produces grid errors.
The `"null"` unit has its relative-sizing meaning in `grid::grid.layout()`, not
in the lplot layout engine. Native `npc` coordinates retain a bottom-left origin.

See `?l_unit` and `?grid::unit` for help and the full native unit vocabulary,
[docs/02-API.md](docs/02-API.md) for the contract, and
[inst/examples/functions/l_unit.R](inst/examples/functions/l_unit.R) for an
independently executable example.

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

## Maintaining API Documentation

All 19 exported functions and four registered S3 methods have English roxygen2
documentation next to their definitions in [R/](R/). Each help topic includes
parameters, return values, usage details, related functions and runnable examples.

After editing these comments, regenerate the help files and namespace from the
package root (install `roxygen2` as a development tool first if needed):

```sh
Rscript -e 'roxygen2::roxygenise(".")'
```

The files in [man/](man/) and [NAMESPACE](NAMESPACE) are generated; edit the
source comments instead of modifying these outputs by hand, and include the
regenerated files when submitting changes. roxygen2 is not a runtime dependency.
Check the generated examples with `R CMD check` before publishing. Passing
documentation checks does not replace the remaining CRAN submission requirements.

## Validation

```sh
Rscript -e 'testthat::test_local(reporter = "summary")'
NOT_CRAN=true Rscript -e 'testthat::test_local(filter = "visual")'
R CMD build .
R CMD check --no-manual lplot_0.1.0.tar.gz
```

Tests cover lengths, extraction/theme inheritance, style isolation, constraints,
nesting/joining, four output sizes, collision policy, rendering and visual references.
The `templates` and `function-examples` tests also verify `l_unit()` against
`grid::unit()`, including vectors, auxiliary data, arithmetic, invalid inputs,
viewport-dependent conversion and identical rendering at two sizes.
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
