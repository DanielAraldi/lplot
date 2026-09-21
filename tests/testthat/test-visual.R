test_that("themes, legends and nested scenes have stable visual output", {
  skip_if_not_installed("vdiffr")
  for (name in c("minimal", "bw", "void")) {
    theme <- switch(
      name,
      minimal = ggplot2::theme_minimal(),
      bw = ggplot2::theme_bw(),
      void = ggplot2::theme_void()
    )
    scene <- visual_scene(theme)
    vdiffr::expect_doppelganger(paste0("theme-", name), l_as_grob(scene))
  }
  nested <- l_join(list(
    l_place(scene, width = "60%", height = "100%"),
    l_place(scene, left = "60%", width = "40%", height = "100%")
  ))
  vdiffr::expect_doppelganger("nested-viewports", l_as_grob(nested))
})

test_that("the same scene resolves on differently sized visual viewports", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")
  scene <- visual_scene()
  for (size in list(c(256, 256), c(512, 512), c(1024, 768), c(1920, 1080))) {
    writer <- function(plot, file, title) {
      svglite::svglite(file, width = size[[1]] / 96, height = size[[2]] / 96)
      on.exit(grDevices::dev.off())
      grid::grid.draw(plot)
    }
    vdiffr::expect_doppelganger(
      paste0("responsive-full-", paste(size, collapse = "x")),
      l_as_grob(scene),
      writer = writer
    )
  }
})

test_that("custom cartographic symbols and collision fallback render natively", {
  skip_if_not_installed("vdiffr")
  arrow <- grid::grobTree(
    grid::polygonGrob(
      x = c(0.5, 0.15, 0.5, 0.85),
      y = c(0.75, 0.05, 0.25, 0.05),
      gp = grid::gpar(fill = "#263C33")
    ),
    grid::textGrob(
      "N",
      x = 0.5,
      y = 0.95,
      gp = grid::gpar(fontsize = 12, fontface = "bold")
    )
  )
  scale <- grid::grobTree(
    grid::rectGrob(
      x = 0.25,
      y = 0.3,
      width = 0.5,
      height = 0.25,
      gp = grid::gpar(fill = "black")
    ),
    grid::rectGrob(
      x = 0.75,
      y = 0.3,
      width = 0.5,
      height = 0.25,
      gp = grid::gpar(fill = "white")
    ),
    grid::textGrob(
      "0                      1 km",
      y = 0.8,
      gp = grid::gpar(fontsize = 9)
    )
  )
  scene <- l_viewport(
    list(
      l_place(
        l_get_element(arrow, "north_arrow", width = 32, height = 60),
        anchor = "top-right"
      ),
      l_place(
        l_get_element(scale, "scale_bar", width = 140, height = 30),
        anchor = "bottom-left"
      )
    ),
    padding = 20,
    background = "#F3F6F2"
  )
  vdiffr::expect_doppelganger("cartographic-symbols", l_as_grob(scene))
  scene <- collision_scene()
  vdiffr::expect_doppelganger(
    "collision-fallback-debug",
    l_as_grob(scene, debug = TRUE)
  )
  label <- l_get_element(
    grid::textGrob("A long label", gp = grid::gpar(fontsize = 28)),
    "annotation",
    collision = "shrink",
    min_width = 50,
    min_height = 10
  )
  shrink <- l_viewport(list(label), padding = 4)
  vdiffr::expect_doppelganger("permitted-shrink", function() {
    l_render(shrink, width = 120, height = 100)
  })
})
