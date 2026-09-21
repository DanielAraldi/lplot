box_in <- function(node, width = 800, height = 600, ...) {
  l_resolve(l_viewport(list(node), ...), width, height)$root$children[[1L]]$box
}

test_that("anchors and opposing insets have deterministic geometry", {
  node <- l_place(
    grid::rectGrob(),
    x = "50%",
    y = "20px",
    width = 200,
    height = 30,
    anchor = "top-center"
  )
  expect_equal(box_in(node), c(x = 300, y = 20, width = 200, height = 30))
  for (anchor in anchors) {
    fraction <- anchor_fraction(anchor)
    box <- box_in(l_place(
      grid::rectGrob(),
      width = 100,
      height = 50,
      anchor = anchor
    ))
    expect_equal(unname(box[c("x", "y")]), unname(fraction * c(700, 550)))
  }
  inset <- l_place(
    grid::rectGrob(),
    left = 10,
    right = 20,
    top = 30,
    bottom = 40,
    margin = 5
  )
  expect_equal(box_in(inset), c(x = 15, y = 35, width = 760, height = 520))
  expect_error(
    box_in(l_place(inset, width = 300)),
    class = "lplot_contradictory_constraints"
  )
  expect_error(
    box_in(l_place(inset, x = 20)),
    class = "lplot_contradictory_constraints"
  )
})

test_that("constraints, aspect ratio and overflow produce useful diagnostics", {
  node <- l_place(
    grid::rectGrob(),
    width = "50%",
    height = 100,
    min_width = 450,
    max_width = 500
  )
  expect_equal(box_in(node)[["width"]], 450)
  expect_error(box_in(l_place(node, max_width = 400)), class = "lplot_error")
  expect_equal(
    box_in(l_place(grid::rectGrob(), width = 200, aspect_ratio = 2))[[
      "height"
    ]],
    100
  )
  expect_error(box_in(l_place(node, aspect_ratio = 1)), class = "lplot_error")
  expect_warning(
    box_in(l_place(grid::rectGrob(), width = 900, height = 10)),
    class = "lplot_overflow"
  )
  expect_error(
    box_in(l_place(grid::rectGrob(), width = "min(-10px, 30px)")),
    class = "lplot_error"
  )
  expect_error(box_in(l_place(node, top = "auto")), class = "lplot_error")
})

test_that("padding, border, safe area and flow are separate box concepts", {
  node <- l_place(
    grid::rectGrob(),
    width = 20,
    height = 10,
    anchor = "bottom-right"
  )
  scene <- l_viewport(
    list(node),
    padding = 10,
    border = list(width = 2),
    safe_area = 8
  )
  layout <- l_resolve(scene, 200, 100)
  expect_equal(
    layout$root$children[[1]]$box,
    c(x = 148, y = 58, width = 20, height = 10)
  )
  explicit <- l_place(node, right = 0, bottom = 0)
  expect_equal(box_in(explicit, 200, 100, safe_area = 8)[["x"]], 180)
  row <- l_viewport(
    list(l_place(node, anchor = "top-left"), node),
    flow = "row",
    gap = 5
  )
  expect_equal(l_resolve(row, 200, 100)$root$children[[2]]$box[["x"]], 25)
  column <- l_viewport(list(node, node), flow = "column", gap = 3)
  expect_equal(l_resolve(column, 200, 100)$root$children[[2]]$box[["y"]], 13)
})

test_that("responsive resizing and nesting never rewrite logical specifications", {
  element <- l_place(
    grid::rectGrob(),
    x = "50%",
    y = "50%",
    width = "10vw",
    height = 10,
    anchor = "center"
  )
  child <- l_viewport(list(element), padding = 4)
  scene <- l_join(list(l_place(
    child,
    left = "10%",
    top = "10%",
    width = "60%",
    height = "80%"
  )))
  original <- scene
  for (dimensions in list(
    c(256, 256),
    c(512, 512),
    c(1024, 768),
    c(1920, 1080)
  )) {
    layout <- l_resolve(scene, dimensions[[1]], dimensions[[2]])
    parent <- layout$root$children[[1]]
    nested <- parent$children[[1]]
    expect_equal(parent$box[["x"]], dimensions[[1]] * 0.1)
    expect_equal(nested$box[["width"]], dimensions[[1]] * 0.1)
    expect_equal(
      nested$box[["x"]] + nested$box[["width"]] / 2,
      (dimensions[[1]] * 0.6 - 8) / 2
    )
    expect_identical(scene, original)
  }
  moved <- l_join(list(l_place(
    child,
    left = "20%",
    width = "60%",
    height = "80%"
  )))
  expect_equal(
    l_resolve(scene, 800, 600)$root$children[[1]]$children[[1]]$box,
    l_resolve(moved, 800, 600)$root$children[[1]]$children[[1]]$box
  )
})

test_that("intrinsic measurement is contextual and uses custom measurers", {
  caller <- grDevices::dev.cur()
  devices <- grDevices::dev.list()
  title <- l_get_element(example_plot(), "title")
  size <- l_measure(title)
  expect_gt(size$width, 0)
  expect_gt(size$height, 0)
  registry <- l_register_element(
    "test",
    extract = function(plot) grid::rectGrob(),
    measure = function(element, context) {
      c(width = context$width / 2, height = 20)
    }
  )
  element <- l_get_element(NULL, "test", registry = registry)
  expect_equal(l_measure(element, list(width = 200, height = 100))$width, 100)
  expect_equal(l_measure(element, list(width = 400, height = 100))$width, 200)
  expect_identical(grDevices::dev.cur(), caller)
  expect_identical(grDevices::dev.list(), devices)
})

test_that("explicit layout contexts preserve an already open device and viewport", {
  for (size in list(c(5, 5), c(8, 4))) {
    grDevices::pdf(NULL, width = size[[1]], height = size[[2]])
    device <- grDevices::dev.cur()
    tryCatch(
      {
        grid::grid.newpage()
        grid::pushViewport(grid::viewport(
          name = "caller",
          width = 0.5,
          height = 0.75
        ))
        viewport <- grid::current.viewport()
        devices <- grDevices::dev.list()
        scene <- collision_scene()
        layout <- l_resolve(scene, width = 200, height = 120)
        expect_equal(
          layout$root$children[[2]]$box,
          c(x = 140, y = 45, width = 60, height = 30)
        )
        title <- l_get_element(
          example_plot(),
          "title",
          style = list(font_size = "12pt")
        )
        expect_gt(l_measure(title, list(width = 200, height = 120))$width, 0)
        bad <- l_place(grid::rectGrob(), left = 0, right = 0, width = 300)
        expect_error(
          l_measure(bad, list(width = 200, height = 120)),
          class = "lplot_contradictory_constraints"
        )
        expect_identical(grDevices::dev.cur(), device)
        expect_identical(grDevices::dev.list(), devices)
        expect_identical(grid::current.viewport(), viewport)
      },
      finally = grDevices::dev.off(device)
    )
  }
})
