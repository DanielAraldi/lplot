examples_root <- system.file("examples", package = "lplot")
sys.source(file.path(examples_root, "data", "maps.R"), envir = environment())
sys.source(file.path(examples_root, "utils.R"), envir = environment())
rm(examples_root)

map_scale_scene <- function() {
  counties <- map_counties()
  extent <- map_extent(counties)
  plot <- map_source(counties, extent)
  map_sheet(plot, map_frame(plot, extent))
}
