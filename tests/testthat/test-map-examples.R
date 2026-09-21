load_map_examples <- function() {
  environment <- new.env(parent = globalenv())
  sys.source(
    system.file("examples", "maps.R", package = "lplot"),
    envir = environment
  )
  environment
}

test_that("map examples resolve and render with a metrically aligned scale", {
  skip_if_not_installed("sf")
  examples <- load_map_examples()
  for (name in c("map_scale_scene", "map_inset_scene", "map_join_scene")) {
    scene <- examples[[name]]()
    for (size in list(c(800, 600), c(1200, 700))) {
      expect_no_warning(layout <- l_resolve(scene, size[[1]], size[[2]]))
      grDevices::pdf(NULL, width = size[[1]] / 96, height = size[[2]] / 96)
      tryCatch(
        expect_no_warning(l_render(scene)),
        finally = grDevices::dev.off()
      )
      sheets <- if (name == "map_join_scene") {
        layout$root$children
      } else {
        list(layout$root)
      }
      for (sheet in sheets) {
        frame <- sheet$children[[3]]$children[[1]]
        scale <- frame$children[[2]]
        extent <- frame$node$metadata$extent
        ratio <- as.numeric(
          (extent[["xmax"]] - extent[["xmin"]]) /
            (extent[["ymax"]] - extent[["ymin"]])
        )
        expect_equal(frame$box[["width"]] / frame$box[["height"]], ratio)
        expect_equal(
          scale$box[["width"]] / frame$box[["width"]],
          scale$node$metadata$distance_m / scale$node$metadata$extent_width_m
        )
      }
    }
  }
})

test_that("locator highlights the exact projected extent and north is georeferenced", {
  skip_if_not_installed("sf")
  examples <- load_map_examples()
  scene <- examples$map_inset_scene()
  frame <- scene$children[[3]]$children[[1]]
  extent <- frame$metadata$extent
  locator <- frame$children[[3]]
  arrow <- frame$children[[4]]
  expect_identical(locator$metadata$focus, extent)
  highlight <- locator$children[[1]]$source$layers[[2]]$data
  expect_equal(sf::st_bbox(highlight), extent)
  expect_equal(locator$left, l_length(10))
  expect_equal(locator$top, l_length(10))
  panel_source <- frame$children[[1]]$source
  ranges <- ggplot2::ggplot_build(panel_source)$layout$panel_params[[1]]
  expect_equal(
    as.numeric(ranges$x_range),
    as.numeric(extent[c("xmin", "xmax")])
  )
  expect_equal(
    as.numeric(ranges$y_range),
    as.numeric(extent[c("ymin", "ymax")])
  )
  expect_equal(sf::st_crs(extent)$epsg, 32119)
  expect_true(is.finite(arrow$metadata$angle))
  expect_lt(abs(arrow$metadata$angle), 5)
  expect_equal(arrow$type, "north_arrow")
})

test_that("joined maps use the same spatial and colour scales without flattening", {
  skip_if_not_installed("sf")
  examples <- load_map_examples()
  scene <- examples$map_join_scene()
  frames <- lapply(scene$children, function(sheet) {
    sheet$children[[3]]$children[[1]]
  })
  expect_identical(frames[[1]]$metadata$extent, frames[[2]]$metadata$extent)
  limits <- lapply(frames, function(frame) {
    frame$children[[1]]$source$scales$get_scales("fill")$limits
  })
  expect_identical(limits[[1]], limits[[2]])
  expect_equal(limits[[1]][[1]], 0)
  expect_equal(limits[[1]][[2]], 40000)
  original <- scene$children[[2]]$children
  moved <- l_place(scene$children[[2]], left = "10%", top = "50%")
  expect_identical(moved$children, original)
})

test_that("the three map examples have stable visual output", {
  skip_if_not_installed("sf")
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")
  examples <- load_map_examples()
  writer <- function(plot, file, title) {
    svglite::svglite(file, width = 1000 / 96, height = 650 / 96)
    on.exit(grDevices::dev.off())
    grid::grid.draw(plot)
  }
  for (name in c("map_scale_scene", "map_inset_scene", "map_join_scene")) {
    vdiffr::expect_doppelganger(
      name,
      l_as_grob(examples[[name]]()),
      writer = writer
    )
  }
})
