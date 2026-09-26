examples_root <- system.file("examples", package = "lplot")
sys.source(file.path(examples_root, "data", "maps.R"), envir = environment())
sys.source(file.path(examples_root, "utils.R"), envir = environment())
rm(examples_root)

map_inset_scene <- function() {
  counties <- map_counties()
  focus <- sf::st_bbox(
    c(xmin = 580000, ymin = 130000, xmax = 820000, ymax = 290000),
    crs = sf::st_crs(counties)
  )
  plot <- map_source(counties, focus, title = "Regiao central e leste")
  frame <- map_frame(
    plot,
    focus,
    distance_m = 50000,
    overlays = list(
      map_locator(counties, focus),
      lplot::l_place(map_north_arrow(focus), right = 12, top = 12, z_index = 30)
    )
  )
  map_sheet(plot, frame)
}
