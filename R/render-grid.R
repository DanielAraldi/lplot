box_viewport <- function(box, name, clip = "inherit") {
  grid::viewport(
    x = grid::unit(box[["x"]] / 96, "inches"),
    y = grid::unit(1, "npc") - grid::unit(box[["y"]] / 96, "inches"),
    width = grid::unit(box[["width"]] / 96, "inches"),
    height = grid::unit(box[["height"]] / 96, "inches"),
    just = c("left", "top"),
    name = name,
    clip = clip
  )
}

scale_content <- function(grob, factor) {
  if (factor == 1 || is.null(grob)) {
    return(grob)
  }
  for (property in c(
    "x",
    "y",
    "x0",
    "x1",
    "y0",
    "y1",
    "width",
    "height",
    "widths",
    "heights",
    "r"
  )) {
    value <- grob[[property]]
    if (grid::is.unit(value)) {
      relative <- grid::unitType(value) %in% c("npc", "native", "null", "snpc")
      if (any(!relative)) {
        value[!relative] <- value[!relative] * factor
      }
      grob[[property]] <- value
    }
  }
  for (property in c("fontsize", "lwd")) {
    if (!is.null(grob$gp[[property]])) {
      grob$gp[[property]] <- grob$gp[[property]] * factor
    }
  }
  if (inherits(grob, "gtable")) {
    grob$grobs <- lapply(grob$grobs, scale_content, factor = factor)
  }
  if (!is.null(grob$children)) {
    grob$children <- do.call(
      grid::gList,
      lapply(grob$children, scale_content, factor = factor)
    )
  }
  grob
}

debug_grobs <- function(resolved) {
  content <- content_box(resolved)
  fraction <- anchor_fraction(resolved$node$anchor)
  grobs <- list(
    grid::rectGrob(gp = grid::gpar(fill = NA, col = "#C62828", lty = 2)),
    grid::rectGrob(
      vp = box_viewport(content, paste0(resolved$id, "-debug-content")),
      gp = grid::gpar(fill = NA, col = "#1565C0", lty = 3)
    ),
    grid::pointsGrob(
      x = fraction[["x"]],
      y = 1 - fraction[["y"]],
      pch = 3,
      size = grid::unit(2, "mm"),
      gp = grid::gpar(col = "#C62828")
    ),
    grid::textGrob(
      sprintf(
        "%s (%.1f, %.1f) z=%g",
        resolved$id,
        resolved$box[["x"]],
        resolved$box[["y"]],
        resolved$node$z_index
      ),
      x = 0,
      y = 1,
      just = c("left", "top"),
      gp = grid::gpar(fontsize = 6, col = "#C62828")
    )
  )
  intrinsic <- c(
    x = content[["x"]],
    y = content[["y"]],
    width = resolved$intrinsic[["width"]] * resolved$scale,
    height = resolved$intrinsic[["height"]] * resolved$scale
  )
  grobs[[length(grobs) + 1L]] <- grid::rectGrob(
    vp = box_viewport(intrinsic, paste0(resolved$id, "-intrinsic")),
    gp = grid::gpar(fill = NA, col = "#2E7D32", lty = 3)
  )
  margin <- collision_box(resolved)
  margin[c("x", "y")] <- margin[c("x", "y")] - resolved$box[c("x", "y")]
  grobs[[length(grobs) + 1L]] <- grid::rectGrob(
    vp = box_viewport(margin, paste0(resolved$id, "-margin")),
    gp = grid::gpar(fill = NA, col = "#A65F00", lty = 3)
  )
  for (attempt in resolved$collision_candidates) {
    box <- attempt$box
    box[c("x", "y")] <- box[c("x", "y")] - resolved$box[c("x", "y")]
    grobs[[length(grobs) + 1L]] <- grid::rectGrob(
      vp = box_viewport(box, paste0(resolved$id, "-candidate")),
      gp = grid::gpar(fill = NA, col = "#78909C", lty = 3)
    )
  }
  grobs
}

compile_node <- function(resolved, debug = FALSE, ancestor_clips = FALSE) {
  node <- resolved$node
  clip <- if (node$overflow == "hidden") {
    "on"
  } else if (ancestor_clips || node$overflow == "inherit") {
    "inherit"
  } else {
    "off"
  }
  clips <- ancestor_clips || node$overflow == "hidden"
  children <- list()
  if (!is.null(node$background)) {
    children[[length(children) + 1L]] <- grid::rectGrob(
      gp = grid::gpar(fill = node$background, col = NA)
    )
  }
  content <- content_box(resolved)
  if (all(content[c("width", "height")] > 0)) {
    graphics <- list()
    if (!is.null(resolved$content)) {
      graphics[[1L]] <- scale_content(resolved$content, resolved$scale)
    }
    order <- order(
      vapply(resolved$children, function(child) child$node$z_index, numeric(1)),
      seq_along(resolved$children)
    )
    graphics <- c(
      graphics,
      lapply(
        resolved$children[order],
        compile_node,
        debug = debug,
        ancestor_clips = clips
      )
    )
    children[[length(children) + 1L]] <- grid::gTree(
      children = do.call(grid::gList, graphics),
      vp = box_viewport(content, paste0(resolved$id, "-content")),
      name = paste0(resolved$id, "-content")
    )
  }
  border <- resolved$border
  if (border$width > 0) {
    children[[length(children) + 1L]] <- grid::rectGrob(
      width = grid::unit(
        max(0, resolved$box[["width"]] - border$width) / 96,
        "inches"
      ),
      height = grid::unit(
        max(0, resolved$box[["height"]] - border$width) / 96,
        "inches"
      ),
      gp = grid::gpar(
        fill = NA,
        col = border$color,
        lwd = border$width,
        lty = border$line_type
      )
    )
  }
  if (debug) {
    children <- c(children, debug_grobs(resolved))
  }
  grid::gTree(
    children = do.call(grid::gList, children),
    name = resolved$id,
    vp = box_viewport(resolved$box, paste0(resolved$id, "-viewport"), clip)
  )
}

l_as_grob <- function(object, dpi = 96, debug = FALSE) {
  node <- as_l_node(object)
  scalar_number(dpi, "dpi", TRUE)
  grid::gTree(scene = node, dpi = dpi, debug = debug, cl = "l_scene_grob")
}

makeContent.l_scene_grob <- function(x) {
  layout <- l_resolve(
    x$scene,
    width = x$render_width,
    height = x$render_height,
    dpi = x$dpi
  )
  grid::setChildren(x, grid::gList(compile_node(layout$root, x$debug)))
}

l_render <- function(
  object,
  width = NULL,
  height = NULL,
  dpi = 96,
  newpage = TRUE,
  debug = FALSE
) {
  if (newpage) {
    grid::grid.newpage()
  }
  layout <- l_resolve(object, width, height, dpi)
  grob <- l_as_grob(object, dpi = dpi, debug = debug)
  grob$render_width <- width
  grob$render_height <- height
  grid::grid.draw(grob)
  invisible(layout)
}

grid.draw.l_node <- function(x, recording = TRUE) {
  grid::grid.draw(l_as_grob(x), recording = recording)
  invisible(x)
}
