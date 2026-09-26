map_counties <- function() {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop(
      "These examples need sf. Run install.packages('sf') first.",
      call. = FALSE
    )
  }
  counties <- sf::st_read(
    system.file("shape/nc.shp", package = "sf"),
    quiet = TRUE
  )
  sf::st_transform(counties, 32119)
}
