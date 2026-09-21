test_that("native extractors return graphics and absence is explicit", {
  plot <- example_plot() + ggplot2::facet_wrap(~cyl) + ggplot2::theme_bw()
  for (type in native_types) {
    expect_s3_class(l_get_element(plot, type), "l_element")
    expect_true(grid::is.grob(l_get_element(plot, type)$content))
  }
  empty <- ggplot2::ggplot()
  expect_error(l_get_element(empty, "legend"), class = "lplot_missing_element")
  expect_null(l_get_element(empty, "title", missing = "null"))
  expect_error(
    l_get_element(type = "title"),
    class = "lplot_unsupported_source"
  )
  expect_error(
    l_get_element(plot, "unknown"),
    class = "lplot_missing_extractor"
  )
  panels <- l_get_element(plot, "panel")
  expect_s3_class(panels$content, "gtable")
  expect_length(panels$content$grobs, 3)
  expect_false(inherits(
    l_get_element(plot, "panel", which = 2)$content,
    "gtable"
  ))
  expect_error(l_get_element(plot, "panel", which = 9), class = "lplot_error")
})

test_that("removal and styles do not mutate sources or other elements", {
  plot <- example_plot()
  source_state <- function(plot) {
    list(
      data = plot$data,
      theme = plot$theme,
      labels = plot$labels,
      mapping = plot$mapping
    )
  }
  original <- source_state(plot)
  removed <- l_without(plot, c("title", "legend", "x_axis_title"))
  expect_identical(source_state(plot), original)
  expect_null(l_get_element(removed, "title", missing = "null"))
  expect_null(l_get_element(removed, "legend", missing = "null"))
  title <- l_get_element(
    plot,
    "title",
    style = list(color = "blue", font_size = "18pt")
  )
  text <- text_grobs(prepared(title))[[1L]]
  expect_equal(text$gp$col, "blue")
  expect_equal(text$gp$fontsize, 18)
  expect_identical(source_state(plot), original)
  for (type in c("legend", "panel", "x_axis")) {
    before <- text_grobs(l_get_element(plot, type)$content)
    after <- text_grobs(l_get_element(title$source, type)$content)
    expect_equal(
      unname(lapply(before, function(grob) grob$gp)),
      unname(lapply(after, function(grob) grob$gp))
    )
  }
  expect_error(
    l_get_element(plot, "title", style = list(fill = "red")),
    class = "lplot_error"
  )
})

test_that("themes and responsive typography are retained without device state", {
  caller <- grDevices::dev.cur()
  devices <- grDevices::dev.list()
  for (theme in list(
    ggplot2::theme_minimal(),
    ggplot2::theme_bw(),
    ggplot2::theme_void()
  )) {
    plot <- example_plot() +
      theme +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", colour = "red")
      )
    title <- l_get_element(plot, "title")
    text <- text_grobs(prepared(title))[[1L]]
    expect_equal(text$gp$col, "red")
    expect_equal(unname(text$gp$font), 2L)
    changed <- text_grobs(prepared(l_style(title, color = "blue")))[[1L]]
    expect_equal(unname(changed$gp$font), 2L)
  }
  title <- l_get_element(
    example_plot(),
    "title",
    style = list(font_size = "clamp(8pt, 2vmin, 18pt)")
  )
  original <- title
  expect_equal(text_grobs(prepared(title, 256, 256))[[1]]$gp$fontsize, 8)
  expect_equal(text_grobs(prepared(title, 1920, 1080))[[1]]$gp$fontsize, 16.2)
  expect_identical(title, original)
  expect_identical(grDevices::dev.cur(), caller)
  expect_identical(grDevices::dev.list(), devices)
})

test_that("custom registries are explicit and independent", {
  registry <- l_register_element(
    "badge",
    extractor = function(plot) grid::textGrob(plot),
    can_extract = is.character
  )
  badge <- l_get_element("Hello", "badge", registry = registry)
  expect_equal(badge$content$label, "Hello")
  expect_error(
    l_get_element("Hello", "badge"),
    class = "lplot_missing_extractor"
  )
  expect_error(
    l_get_element(1, "badge", registry = registry),
    class = "lplot_unsupported_source"
  )
  expect_true(grid::is.grob(
    l_get_element(grid::rectGrob(), "north_arrow")$content
  ))
  bad <- l_register_element("bad", extractor = function(plot) 1)
  expect_error(
    l_get_element("x", "bad", registry = bad),
    class = "lplot_invalid_extractor"
  )
})

test_that("legend spacer tracks do not stretch its intrinsic dimensions", {
  legend <- l_get_element(example_plot(), "legend")
  small <- l_measure(legend, list(width = 400, height = 300))
  large <- l_measure(legend, list(width = 800, height = 600))
  expect_equal(small$intrinsic, large$intrinsic)
  expect_lt(small$width, 200)
  expect_lt(small$height, 200)
})

test_that("presentation styles work on viewports and custom measurements see styled content", {
  viewport <- l_viewport()
  styled <- l_style(viewport, background = "white", padding = "2mm")
  expect_null(viewport$background)
  expect_equal(styled$background, "white")
  expect_error(l_style(viewport, font_size = 18), class = "lplot_error")
  registry <- l_register_element(
    "label",
    extract = function(plot) grid::textGrob(plot),
    measure = function(element, context) measure_grob(element$content, context)
  )
  element <- l_get_element("Hello", "label", registry = registry)
  expect_gt(
    l_measure(l_style(element, font_size = "30pt"))$height,
    l_measure(element)$height
  )
  old_theme <- ggplot2::theme_set(ggplot2::theme_minimal(base_size = 10))
  on.exit(ggplot2::theme_set(old_theme))
  title <- l_get_element(example_plot(), "title")
  before <- text_grobs(prepared(title))[[1]]$gp
  ggplot2::theme_set(ggplot2::theme_bw(base_size = 30))
  expect_equal(text_grobs(prepared(title))[[1]]$gp, before)
})
