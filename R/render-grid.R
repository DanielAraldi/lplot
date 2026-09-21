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

#' Wrap a scene in a grid grob with deferred layout
#'
#' Create a grid-compatible wrapper that retains a logical scene and resolves
#' it each time grid draws it. Construction does not draw or measure the scene.
#'
#' @param object An lplot scene, viewport, element, ggplot or grid grob.
#' @param dpi Positive output-density metadata; logical pixels remain 1/96 inch.
#' @param debug Logical; whether drawing should add layout diagnostic overlays
#'   for boxes, content areas, margins, intrinsic bounds, anchors and candidates.
#'
#' @details
#' This is an interoperability wrapper, not a frozen image or precomputed
#' layout. [grid::grid.draw()] invokes its grid `makeContent` method, which
#' resolves dimensions against the current viewport on each draw. Use a grid
#' viewport to control its space when embedding it in another grid composition.
#'
#' The wrapper does not implement custom `widthDetails` or `heightDetails`
#' methods. External systems that request intrinsic dimensions may therefore
#' need an explicit viewport. Use [l_measure()] or [l_resolve()] for lplot size
#' inspection; do not assume automatic sizing in every third-party grob layout.
#'
#' @returns A `l_scene_grob`, also a grid `gTree` and `grob`, storing the logical
#'   scene and drawing options. Its children are prepared at drawing time.
#' @seealso [l_render()], [grid::grid.draw()], [grid::makeContent()]
#' @examples
#' scene <- l_viewport(list(l_place(l_text("Deferred layout"), left = 10, top = 10)))
#' grob <- l_as_grob(scene)
#' grid::is.grob(grob)
#' grid::grid.newpage()
#' grid::grid.draw(grob)
#' @export
l_as_grob <- function(object, dpi = 96, debug = FALSE) {
  node <- as_l_node(object)
  scalar_number(dpi, "dpi", TRUE)
  grid::gTree(scene = node, dpi = dpi, debug = debug, cl = "l_scene_grob")
}

#' Prepare a deferred scene grob for grid drawing
#'
#' Grid calls this method when drawing an `l_scene_grob`. It resolves the
#' stored scene for the current context and replaces the wrapper's children
#' with the compiled viewport and grob tree.
#'
#' @param x An `l_scene_grob` created by [l_as_grob()].
#' @details
#' Normally use [grid::grid.draw()] instead of calling this method yourself.
#' Explicit calls through [grid::makeContent()] are useful for inspecting the
#' prepared children. The returned object retains its deferred class, so a
#' later draw can prepare it again; this is not a persistent layout cache.
#'
#' Wrappers recorded by [l_render()] may carry explicit render dimensions.
#' Those axes remain fixed; other axes resolve against the current viewport.
#' The method uses [l_resolve()] and can emit its layout warnings. It prepares
#' content but does not itself draw it or start a new page.
#'
#' @returns A copy of `x` with a compiled child grob tree. The supplied wrapper
#'   and logical scene are not mutated.
#' @seealso [l_as_grob()], [l_resolve()], [grid::makeContent()]
#' @examples
#' scene <- l_viewport(list(l_place(l_text("Prepared by grid"), left = 10, top = 10)))
#' grid::grid.newpage()
#' grob <- l_as_grob(scene)
#' prepared <- grid::makeContent(grob)
#' length(grob$children)
#' length(prepared$children)
#' grid::grid.draw(prepared)
#' @importFrom grid makeContent
#' @export
makeContent.l_scene_grob <- function(x) {
  layout <- l_resolve(
    x$scene,
    width = x$render_width,
    height = x$render_height,
    dpi = x$dpi
  )
  grid::setChildren(x, grid::gList(compile_node(layout$root, x$debug)))
}

#' Draw a logical scene on the current graphics device
#'
#' Resolve a scene and draw it with native grid graphics, optionally starting
#' a new page and showing layout diagnostics. Drawing records a deferred scene
#' so automatic dimensions can be recalculated during device replay.
#'
#' @inheritParams l_resolve
#' @inheritParams l_as_grob
#' @param newpage Logical; whether to start a new grid page before drawing.
#'   Use `FALSE` to draw inside a caller-supplied viewport on the current page.
#'
#' @details
#' With automatic dimensions, the current grid viewport determines the root
#' size. A scene's reference width and height are not a forced device size.
#' Explicit dimensions are positive logical pixels with a fixed physical size;
#' they do not resize the graphics device and can exceed its available area.
#'
#' Normal grid drawing may open the configured default device if none exists.
#' For file output with independent device management, use [l_save()]. The
#' caller's viewport stack is preserved during resolution and drawing, except
#' for the intentional page reset when `newpage = TRUE`.
#'
#' On device replay, automatic axes resolve again, while explicitly supplied
#' width or height stays fixed on that axis. An IDE showing a frozen bitmap
#' must request a redraw before this behavior is visible. The returned layout
#' describes the initial draw and is not updated by later replays.
#'
#' Layout warnings are not suppressed. Resolution occurs for inspection and
#' again during deferred drawing, so the same underlying warning can be
#' reported more than once. Correct the constraints or content rather than
#' relying on warning suppression. Automatic text wrapping is not implemented.
#'
#' @returns The initial resolved `l_layout`, invisibly. The visible side effect
#'   is drawing on the current device. Retain the logical scene for later use.
#' @seealso [l_resolve()], [l_as_grob()], [l_save()], [grid::grid.draw()]
#' @examples
#' scene <- l_viewport(list(
#'   l_place(l_rect(fill = "#95CEC0", col = NA),
#'     left = 10, top = 10, width = "50%", height = 40
#'   ),
#'   l_place(l_text("Survey"), left = 10, top = 65)
#' ))
#' layout <- l_render(scene, width = 300, height = 120)
#' layout$root$children[[1]]$box
#' l_render(scene, width = 300, height = 120, debug = TRUE)
#' @export
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

#' Draw an lplot node through the grid generic
#'
#' Provide [grid::grid.draw()] interoperability for lplot scenes and elements.
#' The node is wrapped as a deferred grob and resolved in the current viewport.
#'
#' @param x An `l_node` or subclass, such as an `l_viewport` or `l_element`.
#' @param recording Logical; passed to grid to control whether this draw is
#'   recorded on the grid display list.
#' @details
#' This method does not begin a new page. Call [grid::grid.newpage()] explicitly
#' when needed. It uses the default options of [l_as_grob()]. For explicit
#' dimensions use [l_render()]; for grid drawing with diagnostics, draw
#' `l_as_grob(x, debug = TRUE)` instead. Device replay can resolve the deferred
#' content again at the new viewport size.
#' @returns `x`, invisibly and unchanged, after drawing on the current device.
#' @seealso [l_render()], [l_as_grob()], [grid::grid.draw()]
#' @examples
#' scene <- l_viewport(list(l_place(l_text("Grid interoperability"),
#'   left = 12, top = 12
#' )))
#' grid::grid.newpage()
#' returned <- grid::grid.draw(scene)
#' identical(returned, scene)
#' @importFrom grid grid.draw
#' @export
grid.draw.l_node <- function(x, recording = TRUE) {
  grid::grid.draw(l_as_grob(x), recording = recording)
  invisible(x)
}
