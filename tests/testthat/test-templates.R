test_that("primitive defaults match native grid constructors", {
  caller <- grDevices::dev.cur()
  devices <- grDevices::dev.list()
  expect_identical(l_rect(name = "rect"), grid::rectGrob(name = "rect"))
  expect_identical(
    l_text("Label", name = "text"),
    grid::textGrob("Label", name = "text")
  )
  expect_identical(
    l_template(name = "template"),
    grid::grobTree(name = "template")
  )
  expect_identical(grDevices::dev.cur(), caller)
  expect_identical(grDevices::dev.list(), devices)
})

test_that("primitives retain native geometry, vectorization and text controls", {
  viewport <- grid::viewport(angle = 15, name = "local")
  rectangle_args <- list(
    x = c(2, 4), y = grid::unit(c(3, 6), "mm"),
    width = grid::unit(1, "cm"), height = c(1, 2),
    just = c("left", "bottom"), hjust = c(0, 1), vjust = 0,
    default.units = "cm", name = "rectangles", vp = viewport,
    gp = grid::gpar(fill = c("white", "red"), col = NA, alpha = 0.7)
  )
  expect_identical(
    do.call(l_rect, rectangle_args),
    do.call(grid::rectGrob, rectangle_args)
  )
  text_args <- list(
    label = expression(alpha, beta^2),
    x = c(1, 2), y = grid::unit(c(4, 8), "mm"),
    just = c("right", "top"), hjust = c(1, 0), vjust = 1,
    rot = 30, check.overlap = TRUE, default.units = "cm",
    name = "labels", vp = viewport,
    gp = grid::gpar(fontsize = c(10, 14), fontfamily = "serif", fontface = "bold")
  )
  expect_identical(
    do.call(l_text, text_args),
    do.call(grid::textGrob, text_args)
  )
})

test_that("graphical shortcuts override gp without mutating it", {
  theme <- grid::gpar(col = "red", fontface = "bold", fontsize = 9)
  original <- theme
  label <- l_text("Note", gp = theme, col = "blue", fontface = "italic")
  expect_identical(theme, original)
  expect_equal(label$gp$col, "blue")
  expect_equal(label$gp$fontsize, 9)
  expect_equal(unname(label$gp$font), 3L)
  expect_equal(
    unname(l_text("Note", gp = list(fontface = "italic"), font = 2)$gp$font),
    2L
  )
  parameters <- list(
    fill = "white", col = "black", alpha = 0.6, lwd = 2, lty = "dashed",
    lineend = "round", linejoin = "bevel", linemitre = 4, lex = 1.2
  )
  expect_identical(
    do.call(l_rect, parameters)$gp,
    do.call(grid::gpar, parameters)
  )
  parameters <- list(
    col = "blue", fontsize = 11, fontfamily = "serif", fontface = "bold.italic",
    lineheight = 1.4, cex = 0.9
  )
  expect_identical(l_text("Note", gp = parameters)$gp, do.call(grid::gpar, parameters))
  expect_identical(l_rect(gp = list(fill = "red"))$gp, grid::gpar(fill = "red"))
  expect_identical(l_template(gp = parameters)$gp, do.call(grid::gpar, parameters))
})

test_that("templates retain child order, nesting and native viewport controls", {
  background <- l_rect(name = "background", fill = "white")
  label <- l_text("N", name = "label")
  nested <- l_template(grid::segmentsGrob(name = "segment"), name = "nested")
  viewport <- grid::viewport(angle = 10)
  child_viewports <- grid::vpList(grid::viewport(name = "child"))
  template <- l_template(
    background,
    children = list(label, nested),
    name = "template", gp = list(col = "red"), vp = viewport,
    childrenvp = child_viewports, cl = "badge"
  )
  expect_identical(
    template,
    grid::grobTree(
      background, label, nested,
      name = "template",
      gp = grid::gpar(col = "red"), vp = viewport,
      childrenvp = child_viewports, cl = "badge"
    )
  )
  expect_identical(template$childrenOrder, c("background", "label", "nested"))
  expect_null(label$gp$col)
  expect_identical(
    l_template(children = grid::gList(background, label), name = "list"),
    grid::grobTree(background, label, name = "list")
  )
})

test_that("invalid children and malformed graphical parameter lists are rejected", {
  expect_error(l_template(1), class = "lplot_unsupported_source")
  expect_error(l_template(l_viewport()), class = "lplot_unsupported_source")
  expect_error(l_template(children = "label"), class = "lplot_error")
  expect_error(l_template(children = list(l_rect(), NULL)), class = "lplot_error")
  for (bad in list(
    "red", list("red"), list(col = "red", col = "blue"),
    stats::setNames(list("red"), NA_character_)
  )) {
    expect_error(l_text("Label", gp = bad), class = "lplot_error")
    expect_error(l_rect(gp = bad), class = "lplot_error")
    expect_error(l_template(gp = bad), class = "lplot_error")
  }
  expect_error(l_text(
    "Label", 0.5, 0.5, "centre", NULL, NULL, 0,
    FALSE, "npc", NULL, NULL, NULL, "red"
  ), class = "lplot_error")
  expect_error(l_rect(col = "red", col = "blue"), class = "lplot_error")
  expect_error(l_text("Label", font = 2, fontface = "italic"))
})

test_that("templates enter extraction and layout through the existing grob path", {
  template <- l_template(
    l_rect(fill = "white", col = "black"),
    l_text("N", fontsize = 12, fontface = "bold")
  )
  original <- template
  element <- l_get_element(template, "north_arrow", width = 40, height = 60)
  expect_identical(element$content, template)
  scene <- l_viewport(list(
    l_place(template, left = 10, top = 10, width = 40, height = 60),
    l_place(element, right = 10, bottom = 10)
  ), width = 200, height = 120)
  expect_s3_class(l_resolve(scene, width = 200, height = 120), "l_layout")
  expect_true(grid::is.grob(prepared(element)))
  expect_gt(l_measure(l_text("Measured", fontsize = 12))$width, 0)
  expect_identical(template, original)
})

test_that("template themes render like native grid and preserve child overrides", {
  skip_if_not_installed("ragg")
  caller <- grDevices::dev.cur()
  devices <- grDevices::dev.list()
  build <- function(template, rectangle, text) {
    template(
      rectangle(x = 0.25, width = 0.5, gp = grid::gpar(col = NA)),
      rectangle(x = 0.75, width = 0.5, gp = grid::gpar(fill = "#194E70", col = NA)),
      template(text("Theme", gp = grid::gpar(col = "white", fontface = "bold"))),
      gp = grid::gpar(fill = "#2B8E98", fontsize = 12)
    )
  }
  template <- build(l_template, l_rect, l_text)
  native <- build(grid::grobTree, grid::rectGrob, grid::textGrob)
  original <- template
  capture <- function(grob, size) {
    image <- ragg::agg_capture(width = size[[1]], height = size[[2]], res = 96)
    on.exit(grDevices::dev.off())
    scene <- l_viewport(list(l_place(
      grob,
      left = 10, right = 10, top = 10, bottom = 10
    )))
    l_render(scene)
    image()
  }
  for (size in list(c(200, 120), c(400, 240))) {
    actual <- capture(template, size)
    expect_identical(actual, capture(native, size))
    expect_true(any(tolower(actual) == "#2b8e98"))
    expect_true(any(tolower(actual) == "#194e70"))
  }
  expect_identical(template, original)
  expect_identical(grDevices::dev.cur(), caller)
  expect_identical(grDevices::dev.list(), devices)
})
