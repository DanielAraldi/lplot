\name{l_save}
\alias{l_save}
\title{Export a Plot or Scene to an Image File}
\description{Render a plot or logical scene into an independent image device and save
JPEG, PNG, SVG or WebP without changing the caller's plotting device.}
\usage{
l_save(plot, type = "png", dir = ".", filename = "plot",
  width = 800, height = 600, dpi = 96, background = "white",
  quality = 90, overwrite = FALSE)
}
\arguments{
\item{plot}{An explicit ggplot, grob, l_element or l_viewport/l_scene. Not a filename or a resolved l_layout.}
\item{type}{Case-insensitive format: jpg, jpeg, png, svg or webp. jpg and jpeg use the same encoder.}
\item{dir}{Destination directory, created recursively if necessary. Defaults to the working directory.}
\item{filename}{A basename without directory components. The selected extension is appended
if omitted. An existing extension must match type; jpg and jpeg are interchangeable.}
\item{width, height}{Positive numeric dimensions in logical pixels, default 800 by 600.
Independent of the current Plots pane and the scene's reference size.}
\item{dpi}{Positive raster density. Raster dimensions are round(width * dpi / 96)
by round(height * dpi / 96). SVG dimensions are width/96 by height/96 inches.}
\item{background}{Device background color. PNG, SVG and WebP permit transparency;
JPEG requires an opaque color. Opaque backgrounds in the scene still cover the device background.}
\item{quality}{Number from 0 to 100, used for lossy JPEG/WebP encoding; ignored for PNG/SVG.}
\item{overwrite}{Allow replacing an existing file. Defaults to FALSE.}
}
\details{Optional dependencies are ragg for PNG/JPEG, svglite for SVG, and ragg plus
webp for WebP. Missing packages produce an actionable lplot_missing_dependency condition.
The package does not install codecs automatically. SVG retains vector graphics where
the source is vector; raster layers remain raster images inside the SVG.

The function opens its own graphics device, calls l_render at the requested logical
size and restores the caller device, including on rendering errors. It writes to a
temporary file in the destination directory and renames it only after successful
rendering/encoding. Existing files are not modified by a rendering failure. Temporary
files are removed; a newly created destination directory may remain after an error.
Concurrent writers to the same destination are not coordinated. Layout warnings are
not suppressed and do not normally prevent saving; contradictory constraints still fail.

Raster exports are limited to 100 million pixels and at least one pixel per axis;
WebP also has a 16383-pixel limit per axis. Alpha is preserved for PNG/WebP when
requested; JPEG has no alpha channel. Increasing dpi increases raster density without
changing logical layout size. Font metrics can vary by graphics device.
No implicit last plot is used, and canonical scene declarations are not changed.}
\value{The normalized absolute path of the exported file, returned invisibly.}
\seealso{\code{\link{l_render}}, \code{\link{l_viewport}}}
\examples{
if (requireNamespace("ragg", quietly = TRUE)) {
  scene <- l_viewport(list(grid::textGrob("Map export")))
  file <- l_save(scene, type = "png", dir = tempdir(),
                 filename = basename(tempfile("lplot-")), width = 400, height = 200)
  stopifnot(file.exists(file))
  unlink(file)
}
}
