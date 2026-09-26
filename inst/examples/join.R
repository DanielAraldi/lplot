examples_root <- system.file("examples", package = "lplot")
sys.source(file.path(examples_root, "data", "maps.R"), envir = environment())
sys.source(file.path(examples_root, "utils.R"), envir = environment())
rm(examples_root)

map_join_scene <- function() {
  counties <- map_counties()
  extent <- map_extent(counties)
  first <- map_source(
    counties,
    extent,
    field = "BIR74",
    title = "Nascimentos | BIR74"
  )
  second <- map_source(
    counties,
    extent,
    field = "BIR79",
    title = "Nascimentos | BIR79"
  )
  first_scene <- map_sheet(first, map_frame(first, extent))
  second_scene <- map_sheet(second, map_frame(second, extent))
  lplot::l_join(
    list(
      lplot::l_place(
        first_scene,
        left = "0%",
        top = "0%",
        width = "49%",
        height = "100%"
      ),
      lplot::l_place(
        second_scene,
        left = "51%",
        top = "0%",
        width = "49%",
        height = "100%"
      )
    ),
    width = 1200,
    height = 650,
    background = "#E6ECEE"
  )
}
