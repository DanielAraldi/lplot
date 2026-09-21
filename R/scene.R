anchors <- c(
  "top-left",
  "top-center",
  "top-right",
  "center-left",
  "center",
  "center-right",
  "bottom-left",
  "bottom-center",
  "bottom-right"
)

normalize_edges <- function(value = 0) {
  sides <- c("top", "right", "bottom", "left")
  if (inherits(value, "l_length")) {
    value <- list(value)
  }
  if (!is.null(names(value))) {
    if (any(!names(value) %in% sides) || anyDuplicated(names(value))) {
      l_abort("Invalid edge names.")
    }
    edges <- stats::setNames(rep(list(l_length(0)), 4L), sides)
    for (side in names(value)) {
      edges[[side]] <- l_length(value[[side]])
    }
    return(edges)
  }
  count <- length(value)
  if (!count %in% 1:4) {
    l_abort("Edges need one to four values, or named sides.")
  }
  index <- switch(as.character(count),
    "1" = rep(1L, 4L),
    "2" = c(1L, 2L, 1L, 2L),
    "3" = c(1L, 2L, 3L, 2L),
    "4" = 1:4
  )
  stats::setNames(
    lapply(index, function(position) l_length(value[[position]])),
    sides
  )
}

new_l_node <- function(
  kind,
  content = NULL,
  type = NULL,
  width = "auto",
  height = "auto"
) {
  structure(
    list(
      id = NULL,
      id_explicit = FALSE,
      kind = kind,
      type = type,
      content = content,
      children = list(),
      width = l_length(width),
      height = l_length(height),
      x = NULL,
      y = NULL,
      top = NULL,
      right = NULL,
      bottom = NULL,
      left = NULL,
      anchor = "top-left",
      margin = normalize_edges(),
      padding = normalize_edges(),
      min_width = NULL,
      max_width = NULL,
      min_height = NULL,
      max_height = NULL,
      aspect_ratio = NULL,
      z_index = 0,
      overflow = "visible",
      responsive = TRUE,
      collision = "none",
      priority = 0,
      allow_move = FALSE,
      candidates = NULL,
      background = NULL,
      border = NULL,
      safe_area = normalize_edges(),
      flow = "absolute",
      gap = l_length(0),
      metadata = list()
    ),
    class = "l_node"
  )
}

capture_plot <- function(plot) plot + (ggplot2::theme_get() + plot$theme)

as_l_node <- function(object) {
  if (inherits(object, "l_node")) {
    return(object)
  }
  if (inherits(object, "ggplot")) {
    return(new_l_node("plot", capture_plot(object), "plot"))
  }
  if (grid::is.grob(object)) {
    return(new_l_node("element", object, "custom"))
  }
  l_abort(
    "Expected a ggplot, grob, l_element or l_viewport.",
    "unsupported_source"
  )
}

update_node <- function(node, properties) {
  lengths <- c(
    "width",
    "height",
    "x",
    "y",
    "top",
    "right",
    "bottom",
    "left",
    "min_width",
    "max_width",
    "min_height",
    "max_height",
    "gap"
  )
  allowed <- c(
    lengths,
    "id",
    "anchor",
    "margin",
    "padding",
    "safe_area",
    "aspect_ratio",
    "z_index",
    "overflow",
    "responsive",
    "collision",
    "priority",
    "allow_move",
    "candidates",
    "background",
    "border",
    "flow",
    "metadata"
  )
  if (
    length(properties) &&
      (is.null(names(properties)) ||
        any(!nzchar(names(properties))) ||
        anyDuplicated(names(properties)))
  ) {
    l_abort("Properties must have unique names.")
  }
  for (property in names(properties)) {
    if (!property %in% allowed) {
      l_abort(paste0("Unknown node property: ", property), property = property)
    }
    value <- properties[[property]]
    if (property %in% lengths && !is.null(value)) {
      value <- l_length(value)
    }
    if (property %in% c("padding", "margin", "safe_area")) {
      value <- normalize_edges(value)
    }
    if (property == "id") {
      node$id_explicit <- !is.null(value)
    }
    node[property] <- list(value)
  }
  validate_node(node)
  node
}

validate_node <- function(node) {
  choices <- list(
    anchor = anchors,
    overflow = c("visible", "hidden", "inherit"),
    collision = c("none", "avoid", "shrink", "avoid-and-shrink"),
    flow = c("absolute", "row", "column", "stack")
  )
  for (property in names(choices)) {
    value <- node[[property]]
    if (
      length(value) != 1L || is.na(value) || !value %in% choices[[property]]
    ) {
      l_abort(
        paste0("Invalid ", property, "."),
        node = node$id,
        property = property
      )
    }
  }
  for (property in c(
    "width",
    "height",
    "min_width",
    "max_width",
    "min_height",
    "max_height",
    "gap"
  )) {
    value <- node[[property]]
    if (!is.null(value) && value$kind == "value" && value$value < 0) {
      l_abort(
        paste0(property, " cannot be negative."),
        node = node$id,
        property = property
      )
    }
  }
  for (property in c("z_index", "priority")) {
    scalar_number(node[[property]], property)
  }
  if (!is.null(node$aspect_ratio)) {
    scalar_number(node$aspect_ratio, "aspect_ratio", TRUE)
  }
  for (property in c("responsive", "allow_move")) {
    if (
      !is.logical(node[[property]]) ||
        length(node[[property]]) != 1L ||
        is.na(node[[property]])
    ) {
      l_abort(paste0(property, " must be TRUE or FALSE."), property = property)
    }
  }
  if (
    !is.null(node$id) &&
      (!is.character(node$id) ||
        length(node$id) != 1L ||
        is.na(node$id) ||
        !nzchar(node$id))
  ) {
    l_abort("id must be a nonempty string.", property = "id")
  }
  if (
    !is.null(node$candidates) &&
      (!is.character(node$candidates) || any(!node$candidates %in% anchors))
  ) {
    l_abort(
      "Collision candidates must be valid anchors.",
      property = "candidates"
    )
  }
  invisible(node)
}

#' Position and constrain a graphics object in a scene
#'
#' Wrap a ggplot or grob as a scene node, or return an existing node with updated
#' logical placement properties. No drawing or device-dependent conversion
#' occurs, and the input object is not modified.
#'
#' @param object A ggplot, grid grob, extracted `l_element`, or existing lplot
#'   scene node. A ggplot's effective theme is captured when it becomes a node.
#' @param x,y Optional anchor coordinates measured from the top-left of the
#'   containing content box. Positive `y` points down. Values use [l_length()].
#' @param width,height Logical border-box dimensions accepted by [l_length()].
#'   `"auto"` uses content or available space, depending on the node type.
#' @param top,right,bottom,left Optional edge insets. Opposing insets determine
#'   an automatic dimension. Do not combine `x` with horizontal insets or `y`
#'   with vertical insets; conflicting constraints raise an error.
#' @param anchor Anchor point used for coordinates or implicit docking. One of
#'   `"top-left"`, `"top-center"`, `"top-right"`, `"center-left"`, `"center"`,
#'   `"center-right"`, `"bottom-left"`, `"bottom-center"`, `"bottom-right"`.
#' @param z_index Finite numeric drawing order. Higher values draw later;
#'   original child order breaks ties.
#' @param ... Uniquely named additional node properties, described below.
#'   Unknown properties raise an `lplot_error`.
#'
#' @details
#' Omitted placement arguments preserve an existing node's declarations.
#' Explicit `NULL` clears an optional coordinate or constraint. Bare numbers
#' are logical pixels, percentages use the parent's content box, and viewport
#' units use the root. Padding and borders are inside the box; margins are
#' outside. See [l_length()] for units and [l_viewport()] for edge shorthand.
#'
#' @section Additional properties:
#' * `min_width`, `max_width`, `min_height`, `max_height`: optional logical
#'   dimension bounds; `aspect_ratio`: positive width-to-height ratio.
#' * `id`: optional nonempty node identifier, unique within a scene.
#' * `margin`, `padding`, `safe_area`: edge lengths; the safe area constrains
#'   automatic child positioning.
#' * `background`: fill color or `NULL`; `border`: a color or a list with
#'   `color`, `width` and `line_type` entries.
#' * `overflow`: `"visible"` (default), `"hidden"` or `"inherit"`. Clipping
#'   does not repair geometry or automatically suppress overflow warnings.
#' * `flow`: `"absolute"` (default), `"row"`, `"column"` or `"stack"`;
#'   `gap`: logical spacing for row/column flow.
#' * `collision`: `"none"` (default), `"avoid"`, `"shrink"` or
#'   `"avoid-and-shrink"`; `priority`: finite numeric placement priority;
#'   `allow_move`: whether explicitly positioned nodes may be moved;
#'   `candidates`: optional character vector of alternative anchors.
#' * `responsive`: logical permission for collision-driven shrinking, not a
#'   switch that freezes percentage or viewport-relative units.
#' * `metadata`: application-specific metadata. `metadata$obstacle` overrides
#'   whether the node is treated as an obstacle during collision resolution.
#'
#' @section Collision behavior:
#' Collision handling is opt-in and based on sibling boxes, not painted map
#' features. Higher priorities are processed first. Explicit positions are
#' protected unless `allow_move = TRUE`. Shrinking requires `responsive = TRUE`
#' and a minimum dimension, with a 10 percent scale safety floor. Whole plots
#' and panel/background elements are normally excluded as obstacles. Failed
#' placement preserves a fallback and emits `lplot_collision`; an overflowing
#' box emits `lplot_overflow`. Inspect [l_resolve()] results for diagnostics.
#'
#' @returns A positioned `l_node`, preserving subclasses when `object` was
#'   already a scene node. Child-local declarations are retained.
#' @seealso [l_viewport()], [l_join()], [l_length()], [l_render()]
#' @examples
#' label <- l_place(l_text("Survey area"),
#'   x = "50%", top = 10,
#'   anchor = "top-center", id = "label"
#' )
#' scene <- l_viewport(list(label), width = 300, height = 100)
#' l_resolve(scene)$root$children[[1]]$box
#' moved <- l_place(label, x = NULL, left = 20)
#' l_resolve(l_viewport(list(moved)), width = 300, height = 100)
#' @export
l_place <- function(
  object,
  x = NULL,
  y = NULL,
  width = "auto",
  height = "auto",
  top = NULL,
  right = NULL,
  bottom = NULL,
  left = NULL,
  anchor = "top-left",
  z_index = 0,
  ...
) {
  node <- as_l_node(object)
  properties <- list(...)
  supplied <- as.list(match.call(expand.dots = FALSE))
  values <- list(
    x = x,
    y = y,
    width = width,
    height = height,
    top = top,
    right = right,
    bottom = bottom,
    left = left,
    anchor = anchor,
    z_index = z_index
  )
  for (property in intersect(names(values), names(supplied))) {
    properties[property] <- values[property]
  }
  update_node(node, properties)
}

assign_scene_ids <- function(node, path = "root") {
  if (!node$id_explicit) {
    node$id <- path
  }
  node$children <- lapply(seq_along(node$children), function(index) {
    assign_scene_ids(node$children[[index]], paste0(path, "/", index))
  })
  node
}

scene_ids <- function(node) {
  c(node$id, unlist(lapply(node$children, scene_ids), use.names = FALSE))
}

#' Create a local layout context for graphical elements
#'
#' Build a logical scene containing plots, grobs, extracted elements or nested
#' viewports. Children are resolved relative to this node's content box when
#' the scene is measured or drawn, not when it is constructed.
#'
#' @param plots List of ggplots, grid grobs or lplot nodes. A single graphics
#'   object is also accepted. The default creates an empty viewport.
#' @param width,height Logical border-box dimensions. At the root these are
#'   reference dimensions for resolution without a device, not forced render
#'   sizes. Use [l_render()] arguments to request explicit drawing dimensions.
#' @param units Unit for numeric `width` and `height`; see [l_length()]. Other
#'   numeric properties remain logical pixels.
#' @param padding,margin One to four logical edge lengths, or a named list with
#'   `top`, `right`, `bottom`, `left` entries. Padding is inside the box; margin
#'   is outside. Unnamed shorthand follows CSS order: all; vertical/horizontal;
#'   top/horizontal/bottom; or top/right/bottom/left. Missing named sides are zero.
#' @param background Optional background fill color. `NULL` leaves it unset.
#' @param responsive Logical; permits collision-driven shrinking of this node
#'   when minimum dimensions are supplied. Logical units always resolve anew.
#' @param safe_area Insets reserved for automatically positioned children,
#'   using the same shorthand as `padding`. `NULL` means zero insets.
#' @param ... Additional named node properties accepted by [l_place()], such
#'   as `id`, `border`, `flow`, `gap`, `overflow` and dimension constraints.
#'
#' @details
#' This is a scene-graph context, not a [grid::viewport()] and not a table of
#' cells. Arbitrary positioning is the default. Percentages refer to the local
#' content area after padding and borders; viewport units refer to the root.
#' `flow = "row"` or `"column"` provides sequential placement, while
#' `flow = "stack"` overlays children. Root margins matter only when nested.
#'
#' Implicit IDs are assigned from tree paths such as `root/1`. Explicit IDs must
#' be unique within the scene. Objects are copied logically; no active plot or
#' global registry is consulted. Automatic sizing does not wrap text or infer
#' a bounding box for arbitrary grid trees. See [l_place()] for collision and
#' clipping rules and [l_template()] for composition inside a single grob.
#'
#' @returns An `l_viewport`, also inheriting from `l_scene` and `l_node`.
#'   Construction does not draw; use [l_render()] or [grid::grid.draw()].
#' @seealso [l_place()], [l_join()], [l_resolve()], [l_template()]
#' @examples
#' scene <- l_viewport(list(
#'   l_place(l_rect(fill = "#95CEC0", col = NA),
#'     left = "10%", top = "10%", width = "80%", height = "80%"
#'   ),
#'   l_place(l_text("Local context"), x = "50%", y = "50%", anchor = "center")
#' ), width = 400, height = 200, padding = 10, background = "white")
#' l_resolve(scene, width = 800, height = 400)$root$children[[1]]$box
#' l_render(scene, width = 400, height = 200)
#' @export
l_viewport <- function(
  plots = list(),
  width = "auto",
  height = "auto",
  units = "px",
  padding = 0,
  margin = 0,
  background = NULL,
  responsive = TRUE,
  safe_area = NULL,
  ...
) {
  if (inherits(plots, c("l_node", "ggplot")) || grid::is.grob(plots)) {
    plots <- list(plots)
  }
  if (!is.list(plots)) {
    l_abort("plots must be a list of scene nodes.")
  }
  node <- new_l_node(
    "viewport",
    width = l_length(width, units),
    height = l_length(height, units)
  )
  node$children <- lapply(plots, as_l_node)
  node <- update_node(
    node,
    c(
      list(
        padding = padding,
        margin = margin,
        background = background,
        responsive = responsive,
        safe_area = safe_area %||% 0
      ),
      list(...)
    )
  )
  class(node) <- c("l_viewport", "l_scene", "l_node")
  node <- assign_scene_ids(node)
  if (anyDuplicated(scene_ids(node))) {
    l_abort("Node IDs must be unique within a scene.", property = "id")
  }
  node
}

#' Join graphical scenes while preserving local coordinates
#'
#' Create a parent viewport around existing graphics objects or scenes without
#' flattening their child declarations. Joining alone does not imply a grid or
#' automatic side-by-side placement.
#'
#' @inheritParams l_viewport
#' @param position Optional uniquely named list of [l_place()] node properties
#'   applied to the joined parent after construction, not to its children.
#' @param gap Logical spacing between children in row or column flow. Ignored
#'   by absolute positioning, the default flow.
#' @param ... Additional arguments forwarded to [l_viewport()], such as
#'   `padding`, `flow`, `border`, `overflow` and other node properties.
#'
#' @details
#' Position children with [l_place()] before joining, or request `flow = "row"`
#' or `flow = "column"`. Each nested scene keeps its own local coordinates and
#' is resolved in the box allocated by the parent. Moving the joined object
#' later does not rewrite those child declarations.
#'
#' @returns An `l_viewport`, also inheriting from `l_scene` and `l_node`.
#'   The input scenes are not modified.
#' @seealso [l_viewport()], [l_place()], [l_resolve()]
#' @examples
#' first <- l_viewport(list(l_place(l_text("First"), left = 8, top = 8)),
#'   background = "#DCEFE8"
#' )
#' second <- l_viewport(list(l_place(l_text("Second"), left = 8, top = 8)),
#'   background = "#EDF3F5"
#' )
#' joined <- l_join(list(
#'   l_place(first, left = "0%", width = "48%", height = "100%"),
#'   l_place(second, left = "52%", width = "48%", height = "100%")
#' ), width = 400, height = 160)
#' l_render(joined, width = 400, height = 160)
#' @export
l_join <- function(
  plots,
  width = "auto",
  height = "auto",
  position = NULL,
  gap = 0,
  background = NULL,
  ...
) {
  node <- l_viewport(
    plots,
    width = width,
    height = height,
    gap = gap,
    background = background,
    ...
  )
  if (!is.null(position)) {
    node <- update_node(node, position)
  }
  node
}

#' Print a compact summary of a scene node
#'
#' Display the node class, kind, identifier, logical dimensions and child
#' count. Unlike printing a ggplot, printing an lplot node does not draw it.
#'
#' @param x An `l_node` or subclass, such as an `l_element` or `l_viewport`.
#' @param ... Additional arguments required by the generic; currently unused.
#' @details Dimensions are printed as stored declarations, not as resolved
#'   measurements. An unassigned node ID is shown explicitly. Use [l_resolve()]
#'   to inspect calculated boxes and [l_render()] to display the scene.
#' @returns `x`, invisibly and unchanged. A summary is written to the console.
#' @seealso [l_viewport()], [l_place()], [l_resolve()]
#' @examples
#' scene <- l_viewport(list(l_place(l_text("Example"), left = 10, top = 10)),
#'   width = 300, height = 100
#' )
#' returned <- print(scene)
#' identical(returned, scene)
#' @export
print.l_node <- function(x, ...) {
  cat(
    "<",
    class(x)[[1L]],
    "> ",
    x$kind,
    " ",
    x$id %||% "(unassigned)",
    " [",
    format_length(x$width),
    " x ",
    format_length(x$height),
    "]\n",
    sep = ""
  )
  if (length(x$children)) {
    cat("Children:", length(x$children), "\n")
  }
  invisible(x)
}
