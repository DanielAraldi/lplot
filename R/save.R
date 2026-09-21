l_save <- function(
  plot,
  type = "png",
  dir = ".",
  filename = "plot",
  width = 800,
  height = 600,
  dpi = 96,
  background = "white",
  quality = 90,
  overwrite = FALSE
) {
  node <- as_l_node(plot)
  if (!is.character(type) || length(type) != 1L || is.na(type)) {
    l_abort("type must be jpg, jpeg, png, svg or webp.", property = "type")
  }
  type <- tolower(type)
  if (!type %in% c("jpg", "jpeg", "png", "svg", "webp")) {
    l_abort("type must be jpg, jpeg, png, svg or webp.", property = "type")
  }
  for (property in c("dir", "filename")) {
    value <- if (property == "dir") dir else filename
    if (
      !is.character(value) ||
        length(value) != 1L ||
        is.na(value) ||
        !nzchar(value)
    ) {
      l_abort(
        paste0(property, " must be a nonempty string."),
        property = property
      )
    }
  }
  if (filename %in% c(".", "..") || grepl("[/\\\\]", filename)) {
    l_abort(
      "filename must be a file name, without a directory path.",
      property = "filename"
    )
  }
  extension <- tolower(tools::file_ext(filename))
  aliases <- if (type %in% c("jpg", "jpeg")) c("jpg", "jpeg") else type
  if (nzchar(extension) && !extension %in% aliases) {
    l_abort(
      "filename extension does not match type; omit it or use the selected format.",
      property = "filename"
    )
  }
  if (!nzchar(extension)) {
    filename <- paste0(filename, ".", type)
  }
  width <- scalar_number(width, "width", TRUE)
  height <- scalar_number(height, "height", TRUE)
  dpi <- scalar_number(dpi, "dpi", TRUE)
  quality <- scalar_number(quality, "quality")
  if (quality < 0 || quality > 100) {
    l_abort("quality must be between 0 and 100.", property = "quality")
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    l_abort("overwrite must be TRUE or FALSE.", property = "overwrite")
  }
  if (
    !is.character(background) || length(background) != 1L || is.na(background)
  ) {
    l_abort("background must be a color string.", property = "background")
  }
  rgba <- tryCatch(
    grDevices::col2rgb(background, alpha = TRUE),
    error = function(condition) {
      l_abort("background must be a valid color.", property = "background")
    }
  )
  if (type %in% c("jpg", "jpeg") && rgba[[4L]] != 255L) {
    l_abort(
      "JPEG does not support transparency; choose an opaque background.",
      property = "background"
    )
  }
  pixel_width <- round(width * dpi / 96)
  pixel_height <- round(height * dpi / 96)
  if (
    type != "svg" &&
      (any(!is.finite(c(pixel_width, pixel_height))) ||
        min(pixel_width, pixel_height) < 1 ||
        pixel_width * pixel_height > 1e8)
  ) {
    l_abort(
      "Raster dimensions must produce at least one pixel per axis and at most 100 million pixels.",
      property = "width/height"
    )
  }
  if (type == "webp" && max(pixel_width, pixel_height) > 16383) {
    l_abort(
      "WebP supports at most 16383 pixels per axis.",
      property = "width/height"
    )
  }
  packages <- if (type == "svg") {
    "svglite"
  } else if (type == "webp") {
    c("ragg", "webp")
  } else {
    "ragg"
  }
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      l_abort(
        paste0(
          "Exporting ",
          type,
          " requires '",
          package,
          "'. Run install.packages('",
          package,
          "')."
        ),
        "missing_dependency"
      )
    }
  }
  dir <- path.expand(dir)
  if (
    !dir.exists(dir) && !dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  ) {
    l_abort("Cannot create destination directory.", "export", property = "dir")
  }
  destination <- file.path(
    normalizePath(dir, winslash = "/", mustWork = TRUE),
    filename
  )
  if (dir.exists(destination) || (file.exists(destination) && !overwrite)) {
    l_abort(
      "Destination already exists; choose another name or set overwrite = TRUE.",
      "file_exists",
      property = "filename"
    )
  }
  temporary <- tempfile(
    pattern = ".lplot-",
    tmpdir = dirname(destination),
    fileext = paste0(".", type)
  )
  caller <- grDevices::dev.cur()
  device <- NULL
  on.exit(
    {
      if (!is.null(device) && device %in% grDevices::dev.list()) {
        grDevices::dev.off(device)
      }
      if (
        caller != 1L &&
          caller %in% grDevices::dev.list() &&
          grDevices::dev.cur() != caller
      ) {
        grDevices::dev.set(caller)
      }
      unlink(temporary)
    },
    add = TRUE
  )
  capture <- NULL
  if (type == "svg") {
    svglite::svglite(
      temporary,
      width = width / 96,
      height = height / 96,
      bg = background
    )
  } else if (type == "png") {
    ragg::agg_png(
      temporary,
      width = pixel_width,
      height = pixel_height,
      res = dpi,
      background = background
    )
  } else if (type %in% c("jpg", "jpeg")) {
    ragg::agg_jpeg(
      temporary,
      width = pixel_width,
      height = pixel_height,
      res = dpi,
      background = background,
      quality = quality
    )
  } else {
    capture <- ragg::agg_capture(
      width = pixel_width,
      height = pixel_height,
      res = dpi,
      background = background
    )
  }
  device <- grDevices::dev.cur()
  l_render(node, width = width, height = height, dpi = dpi)
  bitmap <- if (!is.null(capture)) capture() else NULL
  grDevices::dev.off(device)
  device <- NULL
  if (!is.null(bitmap)) {
    colors <- grDevices::col2rgb(as.vector(t(bitmap)), alpha = TRUE)
    pixels <- array(as.raw(colors), dim = c(4L, pixel_width, pixel_height))
    webp::write_webp(pixels, target = temporary, quality = quality)
  }
  if (
    !file.exists(temporary) ||
      file.info(temporary)$size == 0 ||
      !file.rename(temporary, destination)
  ) {
    l_abort("Could not publish the exported image.", "export", property = "dir")
  }
  invisible(destination)
}
