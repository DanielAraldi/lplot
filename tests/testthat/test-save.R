read_export_image <- function(path) {
  reader <- switch(
    tolower(tools::file_ext(path)),
    png = png::readPNG,
    jpg = jpeg::readJPEG,
    jpeg = jpeg::readJPEG,
    webp = webp::read_webp
  )
  reader(path)
}

compare_export_image <- function(old, new) {
  reference <- read_export_image(old)
  actual <- read_export_image(new)
  if (!identical(dim(reference), dim(actual))) {
    return(FALSE)
  }
  difference <- abs(reference - actual)
  max(difference) <= 2 / 255 && mean(difference) <= 0.1 / 255
}

export_scene <- function() {
  l_viewport(list(
    l_place(
      grid::rectGrob(gp = grid::gpar(fill = "red", col = NA)),
      left = 0,
      top = 0,
      width = "50%",
      height = "50%"
    ),
    l_place(
      grid::rectGrob(gp = grid::gpar(fill = "blue", col = NA)),
      right = 0,
      bottom = 0,
      width = "50%",
      height = "50%"
    )
  ))
}

test_that("l_save writes decodable raster formats with correct orientation and density", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("webp")
  skip_if_not_installed("png")
  skip_if_not_installed("jpeg")
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE))
  for (type in c("PNG", "jpg", "jpeg", "webp")) {
    result <- withVisible(l_save(
      export_scene(),
      type,
      directory,
      filename = tolower(type),
      width = 120,
      height = 80,
      dpi = 192
    ))
    expect_false(result$visible)
    expect_true(file.exists(result$value))
    expect_equal(dirname(result$value), normalizePath(directory))
    reader <- switch(
      tolower(type),
      png = png::readPNG,
      jpg = jpeg::readJPEG,
      jpeg = jpeg::readJPEG,
      webp = webp::read_webp
    )
    image <- reader(result$value)
    expect_equal(dim(image)[1:2], c(160L, 240L))
    expect_gt(image[20, 20, 1], 0.9)
    expect_lt(image[20, 20, 3], 0.1)
    expect_gt(image[140, 220, 3], 0.9)
    expect_lt(image[140, 220, 1], 0.1)
    expect_true(all(image[20, 220, 1:3] > 0.9))
  }
})

test_that("SVG preserves vector content and raster backgrounds can be transparent", {
  skip_if_not_installed("svglite")
  skip_if_not_installed("ragg")
  skip_if_not_installed("png")
  skip_if_not_installed("webp")
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE))
  path <- l_save(
    export_scene(),
    "svg",
    directory,
    width = 120,
    height = 80,
    dpi = 300
  )
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(text, "<svg")
  expect_match(text, "viewBox='0 0 90.00 60.00'", fixed = TRUE)
  expect_match(text, "<rect")
  expect_false(grepl("<image", text, fixed = TRUE))
  for (type in c("png", "webp")) {
    path <- l_save(
      export_scene(),
      type,
      directory,
      width = 120,
      height = 80,
      background = "transparent"
    )
    image <- if (type == "png") png::readPNG(path) else webp::read_webp(path)
    expect_equal(dim(image), c(80L, 120L, 4L))
    expect_equal(image[10, 110, 4], 0)
    expect_equal(image[10, 10, 4], 1)
  }
})

test_that("exports preserve existing files and restore caller devices on success and failure", {
  skip_if_not_installed("ragg")
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE))
  grDevices::pdf(NULL, width = 2, height = 2)
  caller <- grDevices::dev.cur()
  on.exit(grDevices::dev.off(caller), add = TRUE)
  grid::pushViewport(grid::viewport(name = "caller"))
  original <- export_scene()
  snapshot <- original
  path <- l_save(
    original,
    "png",
    directory,
    filename = "map.png",
    width = 100,
    height = 60
  )
  digest <- unname(tools::md5sum(path))
  expect_equal(grDevices::dev.cur(), caller)
  expect_equal(grid::current.viewport()$name, "caller")
  expect_error(
    l_save(original, "png", directory, filename = "map.png"),
    class = "lplot_file_exists"
  )
  bad <- l_viewport(list(l_place(
    grid::rectGrob(),
    left = 0,
    right = 0,
    width = 900
  )))
  expect_error(
    l_save(bad, "png", directory, filename = "map.png", overwrite = TRUE),
    class = "lplot_contradictory_constraints"
  )
  expect_equal(unname(tools::md5sum(path)), digest)
  expect_equal(grDevices::dev.cur(), caller)
  expect_equal(grid::current.viewport()$name, "caller")
  expect_equal(list.files(directory, all.files = TRUE, no.. = TRUE), "map.png")
  expect_no_warning(l_save(
    original,
    "png",
    directory,
    filename = "map.png",
    width = 200,
    overwrite = TRUE
  ))
  expect_false(identical(unname(tools::md5sum(path)), digest))
  expect_identical(original, snapshot)
  grid::popViewport()
})

test_that("export validation rejects unsupported formats and ambiguous paths", {
  directory <- tempfile()
  scene <- export_scene()
  for (arguments in list(
    list(type = "pdf"),
    list(type = NA),
    list(width = 0),
    list(dpi = -1),
    list(quality = 101),
    list(filename = "../map"),
    list(filename = "map.svg"),
    list(overwrite = NA),
    list(background = "not-a-color"),
    list(type = "jpg", background = "transparent"),
    list(dir = ""),
    list(filename = "folder\\map"),
    list(width = 1e6),
    list(type = "webp", width = 16384, height = 1),
    list(width = 0.01)
  )) {
    expect_error(
      do.call(
        l_save,
        utils::modifyList(list(plot = scene, dir = directory), arguments)
      ),
      class = "lplot_error"
    )
  }
  expect_false(dir.exists(directory))
  expect_error(l_save(1), class = "lplot_unsupported_source")
})

test_that("raw ggplots, grobs and extracted elements are exportable", {
  skip_if_not_installed("ragg")
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE))
  objects <- list(
    example_plot(),
    grid::textGrob("Map"),
    l_get_element(example_plot(), "title")
  )
  for (index in seq_along(objects)) {
    expect_no_warning(
      path <- l_save(
        objects[[index]],
        "png",
        directory,
        filename = paste0("map-", index)
      )
    )
    expect_true(file.exists(path))
  }
})

test_that("directory creation, JPEG aliases and headless exports do not leak devices", {
  skip_if_not_installed("ragg")
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE))
  caller <- grDevices::dev.cur()
  devices <- grDevices::dev.list()
  destination <- file.path(directory, "nested folder")
  path <- l_save(
    export_scene(),
    "JPEG",
    destination,
    filename = "map.JPG",
    width = 100,
    height = 60
  )
  expect_true(file.exists(path))
  expect_equal(basename(path), "map.JPG")
  expect_identical(grDevices::dev.cur(), caller)
  expect_identical(grDevices::dev.list(), devices)
  expect_error(l_save(export_scene(), "png", path), class = "lplot_export")
  bad <- l_viewport(list(l_place(
    grid::rectGrob(),
    left = 0,
    right = 0,
    width = 900
  )))
  expect_error(
    l_save(bad, "png", destination, filename = "failed"),
    class = "lplot_error"
  )
  expect_identical(grDevices::dev.list(), devices)
  expect_equal(
    list.files(destination, all.files = TRUE, no.. = TRUE),
    "map.JPG"
  )
})

test_that("l_save map exports match the approved files for every format", {
  skip_on_cran()
  for (package in c("sf", "ragg", "svglite", "webp", "png", "jpeg")) {
    skip_if_not_installed(package)
  }
  examples <- new.env(parent = globalenv())
  sys.source(
    system.file("examples", "maps.R", package = "lplot"),
    envir = examples
  )
  scene <- examples$map_inset_scene()
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE))
  for (type in c("png", "jpg", "jpeg", "svg", "webp")) {
    expect_no_warning(
      path <- l_save(
        scene,
        type = type,
        dir = directory,
        filename = "map-inset",
        width = 900,
        height = 600,
        dpi = 96,
        quality = 90,
        background = "white"
      )
    )
    if (type == "svg") {
      text <- paste(readLines(path, warn = FALSE), collapse = "\n")
      expect_match(text, "<svg")
      expect_match(text, "viewBox='0 0 675.00 450.00'", fixed = TRUE)
      expect_match(text, "<polygon")
      comparator <- testthat::compare_file_text
    } else {
      image <- read_export_image(path)
      expect_equal(dim(image)[1:2], c(600L, 900L))
      expect_gt(diff(range(image[,, 1:3])), 0.5)
      comparator <- compare_export_image
    }
    expect_snapshot_file(
      path,
      name = paste0("map-inset.", type),
      compare = comparator
    )
  }
})

test_that("export snapshot comparisons reject blank, resized and changed images", {
  for (package in c("ragg", "webp", "png", "jpeg")) {
    skip_if_not_installed(package)
  }
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE))
  for (type in c("png", "jpg", "jpeg", "webp")) {
    reference <- l_save(
      export_scene(),
      type,
      directory,
      filename = "reference",
      width = 120,
      height = 80
    )
    identical_image <- l_save(
      export_scene(),
      type,
      directory,
      filename = "identical",
      width = 120,
      height = 80
    )
    blank <- l_save(
      l_viewport(),
      type,
      directory,
      filename = "blank",
      width = 120,
      height = 80
    )
    resized <- l_save(
      export_scene(),
      type,
      directory,
      filename = "resized",
      width = 100,
      height = 80
    )
    changed <- export_scene()
    changed$children[[1]] <- l_place(
      grid::rectGrob(gp = grid::gpar(fill = "green", col = NA)),
      left = 0,
      top = 0,
      width = "50%",
      height = "50%"
    )
    different_image <- l_save(
      changed,
      type,
      directory,
      filename = "changed",
      width = 120,
      height = 80
    )
    expect_true(compare_export_image(reference, identical_image))
    expect_false(compare_export_image(reference, blank))
    expect_false(compare_export_image(reference, resized))
    expect_false(compare_export_image(reference, different_image))
  }
})
