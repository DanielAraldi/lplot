load_map_examples <- function() {
  environment <- new.env(parent = globalenv())
  root <- system.file("examples", package = "lplot")
  for (file in c(
    "simple.R", "template.R", "sf.R", "complex.R",
    "scale.R", "inset.R", "join.R"
  )) {
    sys.source(file.path(root, file), envir = environment)
  }
  environment
}

test_that("simple maps work without utilities and extract only title and legend", {
  skip_if_not_installed("sf")
  examples <- new.env(parent = baseenv())
  sys.source(system.file("examples", "simple.R", package = "lplot"), envir = examples)
  expect_identical(ls(examples), "map_simple_scene")
  expect_s3_class(examples$map_simple_scene(), "l_viewport")
  counties <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  counties$custom <- counties$BIR74 * 2
  scene <- examples$map_simple_scene(counties, "custom", legend_title = "Total")
  expect_length(scene$children, 3)
  expect_identical(
    vapply(scene$children, function(child) child$id, character(1)),
    c("title", "map", "legend")
  )
  map <- scene$children[[2]]
  legend <- scene$children[[3]]
  expect_identical(map$kind, "plot")
  expect_s3_class(map$content, "ggplot")
  expect_identical(legend$source, scene$children[[1]]$source)
  expect_identical(legend$source$data, map$content$data)
  expect_null(map$content$labels$title)
  expect_identical(map$content$theme$legend.position, "none")
  expect_identical(legend$source$labels$title, "Nascimentos por condado")
  expect_identical(legend$source$theme$legend.position, "bottom")
  scale <- ggplot2::ggplot_build(legend$source)$plot$scales$get_scales("fill")
  expect_equal(scale$get_limits(), range(counties$custom))
  expect_identical(scale$name, "Total")
  original <- examples$map_simple_scene(counties)
  original_scale <- ggplot2::ggplot_build(
    original$children[[3]]$source
  )$plot$scales$get_scales("fill")
  expect_equal(original_scale$get_limits(), range(counties$BIR74))
})

test_that("simple maps let ggplot2 preserve coordinates on different devices", {
  skip_if_not_installed("sf")
  examples <- load_map_examples()
  scene <- examples$map_simple_scene()
  map <- scene$children[[2]]
  expect_s3_class(map$content$coordinates, "CoordSf")
  expect_null(map$aspect_ratio)
  for (size in list(c(600, 400), c(1000, 650))) {
    grDevices::pdf(NULL, width = size[[1]] / 96, height = size[[2]] / 96)
    tryCatch(
      {
        expect_no_warning(layout <- l_render(scene))
        children <- layout$root$children
        expect_lte(
          children[[1]]$box[["y"]] + children[[1]]$box[["height"]],
          children[[2]]$box[["y"]]
        )
        expect_lte(
          children[[2]]$box[["y"]] + children[[2]]$box[["height"]],
          children[[3]]$box[["y"]]
        )
      },
      finally = grDevices::dev.off()
    )
  }
})

test_that("map templates reuse geometry and accept content and colour overrides", {
  skip_if_not_installed("sf")
  examples <- load_map_examples()
  scene <- examples$map_template_scene(
    field = "BIR79", title = "Nascimentos | 1979",
    accent = "#A63748", background = "#FAF1F3"
  )
  templates <- lapply(scene$children[c(1, 4, 5)], function(child) child$content)
  for (template in templates) {
    expect_s3_class(template, "gTree")
    expect_identical(template$children$accent$gp$fill, "#A63748")
    expect_identical(template$children$background$gp$fill, "#FAF1F3")
    expect_identical(template$children$title$gp$col, "#A63748")
  }
  expect_identical(templates[[1]]$children$title$label, "Nascimentos | 1979")
  expect_identical(templates[[3]]$children$title$label, "100")
  expect_equal(
    scene$children[[2]]$content$data$value,
    examples$map_counties()$BIR79
  )
})

test_that("sf examples compute density from projected polygon areas", {
  skip_if_not_installed("sf")
  examples <- load_map_examples()
  counties <- examples$map_counties()
  geographic <- sf::st_transform(counties, 4326)
  scene <- examples$map_sf_scene(geographic)
  data <- scene$children[[2]]$content$data
  expect_s3_class(data, "sf")
  expect_equal(sf::st_crs(data)$epsg, 32119)
  expect_equal(data$area_km2, as.numeric(sf::st_area(counties)) / 1e6)
  expect_equal(data$value, 100 * counties$BIR74 / data$area_km2)
  expect_true(all(is.finite(data$value)))
  expect_identical(scene$metadata$field, "births_per_100_km2")
})

test_that("complex maps align geographic overlays and regional indicators", {
  skip_if_not_installed("sf")
  examples <- load_map_examples()
  scene <- examples$map_complex_scene()
  frame <- scene$children[[2]]$children[[1]]$children[[1]]
  expect_identical(frame$metadata$extent, scene$metadata$focus)
  expect_identical(frame$children[[3]]$metadata$focus, scene$metadata$focus)
  expect_equal(frame$children[[2]]$metadata$distance_m, 50000)
  expect_true(is.finite(frame$children[[4]]$metadata$angle))
  counties <- examples$map_counties()
  selected <- lengths(sf::st_intersects(counties, sf::st_as_sfc(scene$metadata$focus))) > 0
  expect_identical(scene$metadata$counties, counties$NAME[selected])
  indicators <- scene$children[[3]]$children
  expect_identical(
    indicators[[1]]$content$children$title$label,
    format(sum(counties$BIR74[selected]), big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
  )
  ranking <- indicators[[3]]$content$data
  expect_length(ranking$BIR74, 5)
  expect_equal(ranking$BIR74, head(sort(counties$BIR74[selected], decreasing = TRUE), 5))
})

test_that("new map compositions render without overlaps in their layout bands", {
  skip_if_not_installed("sf")
  examples <- load_map_examples()
  for (name in c("map_template_scene", "map_sf_scene", "map_complex_scene")) {
    scene <- examples[[name]]()
    sizes <- if (name == "map_complex_scene") {
      list(c(1000, 700), c(1400, 900))
    } else {
      list(c(600, 400), c(1000, 650))
    }
    for (size in sizes) {
      grDevices::pdf(NULL, width = size[[1]] / 96, height = size[[2]] / 96)
      tryCatch(
        {
          expect_no_warning(layout <- l_render(scene))
          children <- layout$root$children
          expect_lte(
            children[[1]]$box[["y"]] + children[[1]]$box[["height"]],
            children[[2]]$box[["y"]]
          )
          if (name == "map_complex_scene") {
            expect_lte(
              children[[2]]$box[["x"]] + children[[2]]$box[["width"]],
              children[[3]]$box[["x"]]
            )
            expect_lte(
              children[[3]]$box[["y"]] + children[[3]]$box[["height"]],
              children[[4]]$box[["y"]]
            )
            frame <- children[[2]]$children[[1]]$children[[1]]
            scale <- frame$children[[2]]
            expect_equal(
              scale$box[["width"]] / frame$box[["width"]],
              scale$node$metadata$distance_m / scale$node$metadata$extent_width_m
            )
          } else {
            expect_lte(
              children[[2]]$box[["y"]] + children[[2]]$box[["height"]],
              children[[3]]$box[["y"]]
            )
            expect_lte(
              children[[3]]$box[["y"]] + children[[3]]$box[["height"]],
              children[[4]]$box[["y"]]
            )
          }
        },
        finally = grDevices::dev.off()
      )
    }
  }
})

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

test_that("joined map credits fit compact plotting devices without overflow", {
  skip_if_not_installed("sf")
  examples <- load_map_examples()
  scene <- examples$map_join_scene()
  original <- scene
  for (size in list(c(480, 320), c(600, 400), c(640, 420), c(1200, 700))) {
    grDevices::pdf(NULL, width = size[[1]] / 96, height = size[[2]] / 96)
    tryCatch(
      {
        expect_no_warning(layout <- l_render(scene))
        for (sheet in layout$root$children) {
          credits <- sheet$children[[5]]
          legend <- sheet$children[[4]]
          available <- content_box(sheet)
          expect_gte(credits$box[["x"]], 0)
          expect_gte(credits$box[["y"]], 0)
          expect_lte(
            credits$box[["x"]] + credits$box[["width"]],
            available[["width"]]
          )
          expect_lte(
            credits$box[["y"]] + credits$box[["height"]],
            available[["height"]]
          )
          expect_lte(
            legend$box[["y"]] + legend$box[["height"]],
            credits$box[["y"]]
          )
          expect_equal(credits$node$overflow, "visible")
        }
      },
      finally = grDevices::dev.off()
    )
  }
  expect_identical(scene, original)
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

test_that("the four progressive map examples have stable visual output", {
  skip_if_not_installed("sf")
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")
  examples <- load_map_examples()
  for (name in c("map_simple_scene", "map_template_scene", "map_sf_scene", "map_complex_scene")) {
    size <- if (name == "map_complex_scene") c(1200, 800) else c(900, 600)
    writer <- function(plot, file, title) {
      svglite::svglite(file, width = size[[1]] / 96, height = size[[2]] / 96)
      on.exit(grDevices::dev.off())
      grid::grid.draw(plot)
    }
    vdiffr::expect_doppelganger(
      name,
      l_as_grob(examples[[name]]()),
      writer = writer
    )
  }
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
