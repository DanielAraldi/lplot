test_that("recorded renders resolve joined viewports on larger and smaller devices", {
  scene <- l_join(list(
    l_place(
      l_viewport(background = "red"),
      left = "2%",
      width = "50%",
      height = "100%"
    ),
    l_place(
      l_viewport(background = "blue"),
      left = "54%",
      width = "44%",
      height = "100%"
    )
  ))
  grDevices::pdf(NULL, width = 4, height = 3)
  recording <- tryCatch(
    {
      grDevices::dev.control(displaylist = "enable")
      l_render(scene)
      grDevices::recordPlot()
    },
    finally = grDevices::dev.off()
  )

  for (size in list(c(8, 3), c(2, 2))) {
    grDevices::pdf(NULL, width = size[[1]], height = size[[2]])
    tryCatch(
      {
        expect_no_warning(grDevices::replayPlot(recording))
        grid::seekViewport("root-viewport", recording = FALSE)
        expect_equal(
          grid::convertWidth(grid::unit(1, "npc"), "inches", valueOnly = TRUE),
          size[[1]]
        )
        expect_equal(
          grid::convertHeight(grid::unit(1, "npc"), "inches", valueOnly = TRUE),
          size[[2]]
        )
        grid::seekViewport("root/2-viewport", recording = FALSE)
        expect_equal(
          grid::convertWidth(grid::unit(1, "npc"), "inches", valueOnly = TRUE),
          size[[1]] * 0.44
        )
        right <- grid::deviceLoc(
          grid::unit(1, "npc"),
          grid::unit(0, "npc"),
          valueOnly = TRUE
        )$x
        expect_equal(right, size[[1]] * 0.98)
        grid::upViewport(0, recording = FALSE)
      },
      finally = grDevices::dev.off()
    )
  }
})

test_that("replay preserves explicit dimensions and resolves auto axes inside the caller", {
  scene <- l_viewport(list(l_place(
    grid::rectGrob(),
    width = "50%",
    height = "50%"
  )))
  cases <- list(
    list(arguments = list(width = 192, height = 144), expected = c(192, 144)),
    list(arguments = list(width = 192), expected = c(192, 384)),
    list(arguments = list(height = 144), expected = c(480, 144))
  )
  for (case in cases) {
    grDevices::pdf(NULL, width = 8, height = 6)
    recording <- tryCatch(
      {
        grDevices::dev.control(displaylist = "enable")
        grid::grid.newpage()
        grid::pushViewport(grid::viewport(
          name = "caller",
          width = 0.5,
          height = 0.5
        ))
        snapshot <- do.call(
          l_render,
          c(
            list(object = scene, newpage = FALSE, debug = TRUE, dpi = 144),
            case$arguments
          )
        )
        expect_equal(snapshot$context$dpi, 144)
        expect_equal(snapshot$context$width, case$arguments$width %||% 384)
        expect_equal(snapshot$context$height, case$arguments$height %||% 288)
        expect_equal(grid::current.viewport()$name, "caller")
        grid::upViewport()
        grDevices::recordPlot()
      },
      finally = grDevices::dev.off()
    )
    grDevices::pdf(NULL, width = 10, height = 8)
    tryCatch(
      {
        expect_no_warning(grDevices::replayPlot(recording))
        grid::seekViewport("root-viewport", recording = FALSE)
        expect_equal(
          grid::convertWidth(grid::unit(1, "npc"), "inches", valueOnly = TRUE) *
            96,
          case$expected[[1]]
        )
        expect_equal(
          grid::convertHeight(
            grid::unit(1, "npc"),
            "inches",
            valueOnly = TRUE
          ) *
            96,
          case$expected[[2]]
        )
        grid::upViewport(0, recording = FALSE)
      },
      finally = grDevices::dev.off()
    )
  }
})

test_that("terrain replay fills the resized device without changing child declarations", {
  examples <- new.env(parent = globalenv())
  sys.source(
    system.file("examples", "terrain.R", package = "lplot"),
    envir = examples
  )
  scene <- examples$terrain_scene()
  original <- scene
  grDevices::pdf(NULL, width = 400 / 96, height = 300 / 96)
  recording <- tryCatch(
    {
      grDevices::dev.control(displaylist = "enable")
      expect_no_warning(l_render(scene))
      grDevices::recordPlot()
    },
    finally = grDevices::dev.off()
  )
  grDevices::pdf(NULL, width = 900 / 96, height = 500 / 96)
  tryCatch(
    {
      expect_no_warning(grDevices::replayPlot(recording))
      grid::seekViewport("root-viewport", recording = FALSE)
      expect_equal(
        grid::convertWidth(grid::unit(1, "npc"), "inches", valueOnly = TRUE) *
          96,
        900
      )
      grid::seekViewport("root/2-viewport", recording = FALSE)
      right <- grid::deviceLoc(
        grid::unit(1, "npc"),
        grid::unit(0, "npc"),
        valueOnly = TRUE
      )$x *
        96
      expect_equal(right, 900 * 0.98)
      grid::upViewport(0, recording = FALSE)
    },
    finally = grDevices::dev.off()
  )
  expect_identical(scene, original)
})

test_that("terrain example fits compact plotting devices without overflow", {
  examples <- new.env(parent = globalenv())
  sys.source(
    system.file("examples", "terrain.R", package = "lplot"),
    envir = examples
  )
  scene <- examples$terrain_scene()
  for (size in list(c(400, 300), c(480, 320), c(640, 420), c(1200, 700))) {
    grDevices::pdf(NULL, width = size[[1]] / 96, height = size[[2]] / 96)
    tryCatch(
      {
        expect_no_warning(layout <- l_render(scene))
        sidebar <- layout$root$children[[2]]
        credits <- sidebar$children[[2]]
        available <- content_box(sidebar)
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
        expect_equal(credits$node$overflow, "visible")
      },
      finally = grDevices::dev.off()
    )
  }
})

test_that("rendering uses the actual device and restores viewport state", {
  grDevices::pdf(NULL, width = 8, height = 6)
  on.exit(grDevices::dev.off())
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(name = "caller", width = 0.5, height = 0.5))
  on.exit(grid::popViewport(), add = TRUE, after = FALSE)
  scene <- l_viewport(
    list(l_place(grid::rectGrob(), width = "50%", height = "50%")),
    width = 256,
    height = 256
  )
  layout <- l_render(scene, newpage = FALSE)
  expect_equal(layout$context$width, 384)
  expect_equal(layout$context$height, 288)
  expect_equal(layout$root$children[[1]]$box[["width"]], 192)
  expect_equal(grid::current.viewport()$name, "caller")
  expect_no_error(grid::grid.draw(scene))
  expect_no_error(grid::grid.draw(l_as_grob(scene)))
  expect_equal(grid::current.viewport()$name, "caller")
})

test_that("compiled drawing order, clipping, backgrounds and debug overlays are explicit", {
  top <- l_place(
    grid::rectGrob(),
    width = 40,
    height = 20,
    z_index = 9,
    id = "top"
  )
  bottom <- l_place(
    grid::rectGrob(),
    width = 40,
    height = 20,
    z_index = -1,
    id = "bottom",
    overflow = "hidden"
  )
  layout <- l_resolve(
    l_viewport(list(top, bottom), background = "white", border = "red"),
    200,
    100
  )
  tree <- compile_node(layout$root)
  expect_equal(tree$children[[2]]$childrenOrder, c("bottom", "top"))
  expect_true(tree$children[[2]]$children[[1]]$vp$clip)
  expect_gt(
    length(compile_node(layout$root, debug = TRUE)$children),
    length(tree$children)
  )
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_no_error(grid::grid.draw(compile_node(layout$root, debug = TRUE)))
})

test_that("title, legend and nested plots render without rasterizing", {
  plot <- example_plot()
  title <- l_get_element(
    plot,
    "title",
    style = list(color = "blue", font_size = "clamp(10pt, 2vmin, 18pt)")
  )
  legend <- l_get_element(
    plot,
    "legend",
    style = list(background = "white", padding = "4px")
  )
  scene <- l_viewport(
    list(
      l_without(plot, c("title", "legend")),
      l_place(title, x = "50%", top = 4, anchor = "top-center"),
      l_place(legend, right = 4, top = 40)
    ),
    padding = 8
  )
  final <- l_join(list(
    l_place(scene, width = "65%", height = "100%"),
    l_place(scene, left = "65%", width = "35%", height = "100%")
  ))
  grDevices::pdf(NULL, width = 12, height = 7)
  on.exit(grDevices::dev.off())
  expect_no_warning(l_render(final))
  expect_no_error(grid::grid.draw(l_as_grob(final)))
})
