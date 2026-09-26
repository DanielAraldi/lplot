examples_root <- system.file("examples", package = "lplot")
sys.source(file.path(examples_root, "data", "maps.R"), envir = environment())
sys.source(file.path(examples_root, "utils.R"), envir = environment())
rm(examples_root)

map_sf_scene <- function(counties = map_counties()) {
  counties <- sf::st_transform(counties, 32119)
  counties$area_km2 <- as.numeric(sf::st_area(counties)) / 1e6
  counties$births_per_100_km2 <- 100 * counties$BIR74 / counties$area_km2
  base <- map_simple_scene(
    counties,
    field = "births_per_100_km2",
    title = "Densidade de nascimentos | 1974",
    legend_title = "Nascimentos por 100 km2"
  )
  lplot::l_viewport(
    list(
      base$children[[1]],
      lplot::l_place(base$children[[2]], bottom = 100),
      lplot::l_place(base$children[[3]], bottom = 36),
      lplot::l_place(
        lplot::l_text(
          "Fonte: sf::nc | Area projetada em NAD83 / North Carolina (m)",
          fontsize = 7,
          col = "#50666C"
        ),
        x = "50%",
        bottom = 0,
        anchor = "bottom-center",
        id = "credits"
      )
    ),
    width = 900,
    height = 600,
    padding = 16,
    background = "white",
    metadata = base$metadata
  )
}
