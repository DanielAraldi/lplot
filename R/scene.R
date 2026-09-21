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
  index <- switch(
    as.character(count),
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
