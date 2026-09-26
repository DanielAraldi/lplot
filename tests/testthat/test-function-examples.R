run_function_example <- function(name) {
  environment <- new.env(parent = baseenv())
  path <- system.file("examples", "functions", paste0(name, ".R"), package = "lplot")
  grDevices::pdf(NULL, width = 8, height = 6)
  on.exit(grDevices::dev.off(), add = TRUE)
  capture.output(sys.source(path, envir = environment))
  grid::grid.force()
  environment$drawn_grobs <- grid::grid.ls(print = FALSE)$name
  environment
}

test_that("every exported function has a focused example with lplot helpers", {
  root <- system.file("examples", "functions", package = "lplot")
  paths <- list.files(root, pattern = "\\.R$", full.names = TRUE)
  public_functions <- getNamespaceExports("lplot")
  expect_setequal(tools::file_path_sans_ext(basename(paths)), public_functions)
  for (path in paths) {
    function_name <- tools::file_path_sans_ext(basename(path))
    symbols <- all.names(parse(path), functions = TRUE, unique = TRUE)
    expect_true(function_name %in% symbols, info = basename(path))
    helpers <- c("l_text", "l_rect", "l_place", "l_render")
    expect_true(
      all(intersect(symbols, public_functions) %in% c(function_name, helpers)),
      info = basename(path)
    )
    expect_false(
      any(symbols %in% c("textGrob", "rectGrob", "grid.draw", "grid.newpage")),
      info = basename(path)
    )
    if (function_name %in% c(
      "l_as_grob", "l_get_element", "l_join", "l_place", "l_rect",
      "l_render", "l_style", "l_template", "l_text", "l_viewport"
    )) {
      expect_true("l_render" %in% symbols, info = basename(path))
    }
  }
})

test_that("length examples run without shared helpers or session objects", {
  length <- run_function_example("l_length")$result
  expect_s3_class(length, "l_length")
  expect_equal(length$value, 25)
  expect_identical(length$unit, "%")

  clamp <- run_function_example("l_clamp")$result
  expect_s3_class(clamp, "l_length")
  expect_identical(clamp$kind, "clamp")
  expect_equal(resolve_length(clamp, new_layout_context(100, 100)), 80)
  expect_equal(resolve_length(clamp, new_layout_context(300, 100)), 150)
  expect_equal(resolve_length(clamp, new_layout_context(800, 100)), 240)
})

test_that("constructor examples draw lplot grobs independently", {
  expect_no_warning(text <- run_function_example("l_text")$result)
  expect_s3_class(text, "text")
  expect_identical(text$label, "Mapa de exemplo")
  expect_equal(text$gp$fontsize, 20)

  expect_no_warning(rectangle <- run_function_example("l_rect")$result)
  expect_s3_class(rectangle, "rect")
  expect_identical(rectangle$gp$fill, "#95CEC0")

  expect_no_warning(example <- run_function_example("l_template"))
  template <- example$result
  expect_s3_class(template, "gTree")
  expect_identical(template$childrenOrder, c("background", "title"))
  expect_identical(template$children$title$label, "Area de estudo")
  expect_true(all(c("background", "title") %in% example$drawn_grobs))
})

test_that("style and registry examples need no extracted elements", {
  expect_no_warning(example <- run_function_example("l_style"))
  expect_s3_class(example$result, "l_node")
  expect_identical(example$result$content, example$original)
  expect_equal(example$original$gp$fontsize, 10)
  expect_equal(example$result$style$font_size, l_length("20pt"))

  registry <- run_function_example("l_registry")$result
  expect_s3_class(registry, "l_registry")
  expect_true(all(c("title", "legend", "panel") %in% names(registry)))
  expect_true(is.function(registry$title$extract))
})

test_that("extraction and removal examples preserve their source plots", {
  expect_no_warning(extracted <- run_function_example("l_get_element"))
  expect_s3_class(extracted$result, "l_element")
  expect_identical(extracted$result$type, "legend")
  expect_identical(extracted$result$source$data, extracted$plot$data)
  expect_identical(extracted$plot$labels$colour, "Cilindros")

  expect_no_warning(removed <- run_function_example("l_without"))
  expect_s3_class(removed$result, "ggplot")
  expect_null(removed$result$labels$title)
  expect_identical(removed$result$theme$legend.position, "none")
  expect_identical(removed$result$data, removed$plot$data)
  expect_identical(removed$plot$labels$title, "Consumo dos veiculos")
  expect_false(identical(removed$plot$theme$legend.position, "none"))
})

test_that("the registration example creates an independent usable adapter", {
  registry <- run_function_example("l_register_element")$result
  expect_s3_class(registry, "l_registry")
  expect_true(registry$map_note$can_extract("Fonte: dados locais"))
  expect_false(registry$map_note$can_extract(1))
  note <- l_get_element("Fonte: dados locais", "map_note", registry = registry)
  expect_identical(note$content$label, "Fonte: dados locais")
  expect_false("map_note" %in% names(l_registry()))
})

test_that("placement and composition examples work with lplot constructors", {
  expect_no_warning(placed <- run_function_example("l_place")$result)
  expect_identical(placed$id, "positioned_rectangle")
  layout <- l_resolve(placed, width = 600, height = 400)
  expect_equal(
    layout$root$children[[1]]$box,
    c(x = 60, y = 60, width = 300, height = 80)
  )

  expect_no_warning(viewport <- run_function_example("l_viewport")$result)
  expect_s3_class(viewport, "l_viewport")
  expect_length(viewport$children, 2)
  children <- l_resolve(viewport, width = 600, height = 400)$root$children
  expect_equal(
    children[[2]]$box[["y"]],
    children[[1]]$box[["y"]] + children[[1]]$box[["height"]] + 16
  )

  expect_no_warning(joined <- run_function_example("l_join"))
  expect_s3_class(joined$result, "l_viewport")
  expect_identical(joined$result$children[[1]]$content, joined$first)
  expect_identical(joined$result$children[[2]]$content, joined$second)
  children <- l_resolve(joined$result, width = 600, height = 400)$root$children
  expect_equal(
    children[[2]]$box[["x"]],
    children[[1]]$box[["x"]] + children[[1]]$box[["width"]] + 16
  )
})

test_that("drawing examples return the documented grob and layout objects", {
  expect_no_warning(wrapped <- run_function_example("l_as_grob"))
  expect_s3_class(wrapped$result, "l_scene_grob")
  expect_true(grid::is.grob(wrapped$result))
  expect_identical(wrapped$result$scene$content, wrapped$label)
  expect_true(wrapped$label$name %in% wrapped$drawn_grobs)

  expect_no_warning(rendered <- run_function_example("l_render")$result)
  expect_s3_class(rendered, "l_layout")
  expect_equal(rendered$root$box[c("width", "height")], c(width = 768, height = 576))
})

test_that("measurement examples report sizes without shared scene setup", {
  expect_no_warning(measured <- run_function_example("l_measure")$result)
  expect_identical(measured$units, "px")
  expect_gt(measured$width, 0)
  expect_gt(measured$height, 0)

  expect_no_warning(resolved <- run_function_example("l_resolve")$result)
  expect_s3_class(resolved, "l_layout")
  expect_equal(resolved$root$box, c(x = 0, y = 0, width = 600, height = 400))
  expect_equal(
    resolved$root$children[[1]]$box,
    c(x = 0, y = 0, width = 40 * 96 / 25.4, height = 20 * 96 / 25.4)
  )
})

test_that("the export example writes an SVG to a fresh temporary directory", {
  skip_if_not_installed("svglite")
  caller <- grDevices::dev.cur()
  expect_no_warning(example <- run_function_example("l_save"))
  on.exit(unlink(example$directory, recursive = TRUE), add = TRUE)
  expect_identical(grDevices::dev.cur(), caller)
  expect_true(file.exists(example$result))
  expect_identical(dirname(example$result), normalizePath(example$directory))
  expect_identical(basename(example$result), "title.svg")
  text <- paste(readLines(example$result, warn = FALSE), collapse = "\n")
  expect_match(text, "<svg")
  expect_match(text, "Titulo exportado", fixed = TRUE)
  expect_match(text, "viewBox='0 0 450.00 300.00'", fixed = TRUE)
})
