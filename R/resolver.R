has_coordinates <- function(node) {
  any(vapply(
    node[c("x", "y", "top", "right", "bottom", "left")],
    function(value) !is.null(value),
    logical(1)
  ))
}

resolve_property <- function(
  value,
  context,
  axis,
  node,
  property,
  fallback = NULL
) {
  if (is.null(value)) {
    return(fallback)
  }
  result <- tryCatch(
    resolve_length(value, context, axis),
    lplot_error = function(condition) {
      l_abort(condition$message, node = node$id, property = property)
    }
  )
  if (!is.finite(result)) {
    l_abort(
      paste0(property, " must resolve to a finite length."),
      node = node$id,
      property = property
    )
  }
  result
}

resolve_edges <- function(edges, context, node, property) {
  result <- vapply(
    names(edges),
    function(side) {
      resolve_property(
        edges[[side]],
        context,
        if (side %in% c("left", "right")) "x" else "y",
        node,
        property
      )
    },
    numeric(1)
  )
  if (any(result < 0)) {
    l_abort(
      paste0(property, " cannot be negative."),
      node = node$id,
      property = property
    )
  }
  result
}

resolve_border <- function(border, context, node) {
  if (is.null(border)) {
    return(list(width = 0, color = NA, line_type = 1))
  }
  if (is.character(border)) {
    border <- list(color = border)
  }
  if (
    !is.list(border) ||
      any(!names(border) %in% c("color", "colour", "width", "line_type"))
  ) {
    l_abort(
      "border must be a color or a list of color, width and line_type.",
      node = node$id,
      property = "border"
    )
  }
  width <- resolve_property(
    border$width %||% "1px",
    context,
    "x",
    node,
    "border"
  )
  if (width < 0) {
    l_abort(
      "Border width cannot be negative.",
      node = node$id,
      property = "border"
    )
  }
  list(
    width = width,
    color = border$color %||% border$colour %||% "black",
    line_type = border$line_type %||% 1
  )
}

anchor_fraction <- function(anchor) {
  index <- match(anchor, anchors) - 1L
  c(x = (index %% 3L) / 2, y = (index %/% 3L) / 2)
}

measure_content <- function(node, grob, context) {
  if (node$kind %in% c("plot", "viewport") || identical(node$type, "panel")) {
    return(c(width = context$width, height = context$height))
  }
  if (is.function(node$adapter$measure)) {
    measured_node <- node
    measured_node$content <- grob
    dimensions <- node$adapter$measure(measured_node, context)
    if (is.list(dimensions)) {
      dimensions <- unlist(dimensions[c("width", "height")])
    }
    if (is.null(names(dimensions)) && length(dimensions) == 2L) {
      names(dimensions) <- c("width", "height")
    }
    dimensions <- dimensions[c("width", "height")]
  } else {
    dimensions <- measure_grob(grob, context)
    if (inherits(grob, "gtable") && !identical(node$type, "legend")) {
      if (any(grid::unitType(grob$widths) == "null")) {
        dimensions[["width"]] <- context$width
      }
      if (any(grid::unitType(grob$heights) == "null")) {
        dimensions[["height"]] <- context$height
      }
    }
    if (identical(node$type, "x_axis")) {
      dimensions[["width"]] <- context$width
    }
    if (identical(node$type, "y_axis")) dimensions[["height"]] <- context$height
  }
  if (
    length(dimensions) != 2L ||
      any(!is.finite(dimensions)) ||
      any(dimensions < 0)
  ) {
    l_abort(
      "Measurer must return finite, nonnegative width and height in logical pixels.",
      "invalid_measurement",
      node$id
    )
  }
  dimensions
}

size_limits <- function(node, context) {
  result <- list()
  for (axis in c("width", "height")) {
    minimum <- paste0("min_", axis)
    maximum <- paste0("max_", axis)
    coordinate <- if (axis == "width") "x" else "y"
    result[[minimum]] <- resolve_property(
      node[[minimum]],
      context,
      coordinate,
      node,
      minimum,
      0
    )
    result[[maximum]] <- resolve_property(
      node[[maximum]],
      context,
      coordinate,
      node,
      maximum,
      Inf
    )
    if (result[[minimum]] < 0 || result[[maximum]] < result[[minimum]]) {
      l_abort(
        paste0("Invalid min/max constraints for ", axis, "."),
        node = node$id,
        property = axis
      )
    }
  }
  result
}

resolve_size <- function(
  node,
  intrinsic,
  context,
  margin,
  padding,
  border,
  area
) {
  dimensions <- c(width = 0, height = 0)
  limits <- size_limits(node, context)
  for (dimension in names(dimensions)) {
    coordinate <- if (dimension == "width") "x" else "y"
    sides <- if (dimension == "width") {
      c("left", "right")
    } else {
      c("top", "bottom")
    }
    insets <- lapply(sides, function(side) {
      resolve_property(node[[side]], context, coordinate, node, side)
    })
    automatic <- node[[dimension]]$kind == "auto"
    stretch <- node$kind %in%
      c("plot", "viewport") ||
      identical(node$type, "panel")
    preferred <- if (stretch) {
      area[[dimension]] - sum(margin[sides])
    } else {
      intrinsic[[dimension]] + sum(padding[sides]) + 2 * border$width
    }
    if (automatic && all(lengths(insets) > 0L)) {
      preferred <- context[[dimension]] -
        sum(unlist(insets)) -
        sum(margin[sides])
    }
    if (!automatic) {
      preferred <- resolve_property(
        node[[dimension]],
        context,
        coordinate,
        node,
        dimension
      )
    }
    if (preferred < 0) {
      l_abort(
        paste0("Negative resolved ", dimension, "."),
        node = node$id,
        property = dimension
      )
    }
    dimensions[[dimension]] <- max(
      limits[[paste0("min_", dimension)]],
      min(preferred, limits[[paste0("max_", dimension)]])
    )
  }
  if (!is.null(node$aspect_ratio)) {
    ratio <- node$aspect_ratio
    if (
      node$width$kind != "auto" &&
        node$height$kind != "auto" &&
        abs(dimensions[["width"]] - ratio * dimensions[["height"]]) > 1e-7
    ) {
      l_abort(
        "Explicit dimensions contradict aspect_ratio.",
        node = node$id,
        property = "aspect_ratio"
      )
    }
    preferred <- if (node$width$kind == "auto" && node$height$kind != "auto") {
      dimensions[["height"]] * ratio
    } else {
      dimensions[["width"]]
    }
    minimum <- max(limits$min_width, limits$min_height * ratio)
    maximum <- min(limits$max_width, limits$max_height * ratio)
    if (minimum > maximum) {
      l_abort(
        "Aspect ratio conflicts with min/max constraints.",
        node = node$id,
        property = "aspect_ratio"
      )
    }
    dimensions[["width"]] <- max(minimum, min(preferred, maximum))
    dimensions[["height"]] <- dimensions[["width"]] / ratio
  }
  dimensions
}

placement_area <- function(node, context, safe_area) {
  area <- c(x = 0, y = 0, width = context$width, height = context$height)
  if (!has_coordinates(node)) {
    area[["x"]] <- safe_area[["left"]]
    area[["y"]] <- safe_area[["top"]]
    area[["width"]] <- context$width - sum(safe_area[c("left", "right")])
    area[["height"]] <- context$height - sum(safe_area[c("top", "bottom")])
  }
  if (any(area[c("width", "height")] < 0)) {
    l_abort(
      "Safe area exceeds the containing viewport.",
      node = node$id,
      property = "safe_area"
    )
  }
  area
}

resolve_position <- function(node, dimensions, context, margin, area) {
  fraction <- anchor_fraction(node$anchor)
  position <- c(x = 0, y = 0)
  for (axis in c("x", "y")) {
    dimension <- if (axis == "x") "width" else "height"
    sides <- if (axis == "x") c("left", "right") else c("top", "bottom")
    near <- resolve_property(
      node[[sides[[1L]]]],
      context,
      axis,
      node,
      sides[[1L]]
    )
    far <- resolve_property(
      node[[sides[[2L]]]],
      context,
      axis,
      node,
      sides[[2L]]
    )
    coordinate <- resolve_property(node[[axis]], context, axis, node, axis)
    if (!is.null(coordinate) && (!is.null(near) || !is.null(far))) {
      l_abort(
        paste0(
          axis,
          " cannot be combined with ",
          paste(sides, collapse = "/"),
          "."
        ),
        "contradictory_constraints",
        node$id,
        axis
      )
    }
    extent <- dimensions[[dimension]]
    if (
      !is.null(near) &&
        !is.null(far) &&
        abs(near + far + extent + sum(margin[sides]) - context[[dimension]]) >
          1e-7
    ) {
      l_abort(
        paste0("Opposing insets contradict ", dimension, "."),
        "contradictory_constraints",
        node$id,
        dimension
      )
    }
    position[[axis]] <- if (!is.null(coordinate)) {
      coordinate -
        fraction[[axis]] * extent +
        (1 - fraction[[axis]]) * margin[[sides[[1L]]]] -
        fraction[[axis]] * margin[[sides[[2L]]]]
    } else if (!is.null(near)) {
      near + margin[[sides[[1L]]]]
    } else if (!is.null(far)) {
      context[[dimension]] - far - extent - margin[[sides[[2L]]]]
    } else {
      area[[axis]] +
        margin[[sides[[1L]]]] +
        fraction[[axis]] * (area[[dimension]] - sum(margin[sides]) - extent)
    }
  }
  c(position, dimensions)
}

resolve_node <- function(node, context, safe_area) {
  margin <- resolve_edges(node$margin, context, node, "margin")
  padding <- resolve_edges(node$padding, context, node, "padding")
  border <- resolve_border(node$border, context, node)
  area <- placement_area(node, context, safe_area)
  content <- with_grid_context(context, prepare_content(node, context))
  intrinsic <- measure_content(node, content, context)
  dimensions <- resolve_size(
    node,
    intrinsic,
    context,
    margin,
    padding,
    border,
    area
  )
  box <- resolve_position(node, dimensions, context, margin, area)
  list(
    id = node$id,
    node = node,
    box = box,
    margin = margin,
    padding = padding,
    border = border,
    intrinsic = intrinsic,
    content = content,
    children = list(),
    scale = 1,
    collision_candidates = list()
  )
}

content_box <- function(resolved) {
  padding <- resolved$padding
  border <- resolved$border$width
  c(
    x = unname(padding[["left"]] + border),
    y = unname(padding[["top"]] + border),
    width = max(
      0,
      resolved$box[["width"]] - sum(padding[c("left", "right")]) - 2 * border
    ),
    height = max(
      0,
      resolved$box[["height"]] - sum(padding[c("top", "bottom")]) - 2 * border
    )
  )
}

validate_resolved <- function(resolved, context) {
  box <- resolved$box
  margin <- resolved$margin
  tolerance <- 1e-7
  outside <- box[["x"]] - margin[["left"]] < -tolerance ||
    box[["y"]] - margin[["top"]] < -tolerance ||
    box[["x"]] + box[["width"]] + margin[["right"]] >
      context$width + tolerance ||
    box[["y"]] + box[["height"]] + margin[["bottom"]] >
      context$height + tolerance
  if (outside) {
    l_warn(
      "Box exceeds its containing viewport.",
      "overflow",
      resolved$id,
      "box"
    )
  }
  content <- content_box(resolved)
  fixed_content <- isTRUE(
    resolved$node$type %in% c(text_types, "credits", "annotation")
  )
  if (
    fixed_content &&
      resolved$node$overflow != "hidden" &&
      any(
        resolved$intrinsic * resolved$scale >
          content[c("width", "height")] + tolerance
      )
  ) {
    l_warn(
      "Intrinsic content exceeds its box; no implicit clipping or text reflow is applied.",
      "overflow",
      resolved$id,
      "content"
    )
  }
  invisible(resolved)
}

resolve_children <- function(parent, context) {
  safe_area <- resolve_edges(
    parent$node$safe_area,
    context,
    parent$node,
    "safe_area"
  )
  cursor <- 0
  gap <- resolve_property(
    parent$node$gap,
    context,
    if (parent$node$flow == "column") "y" else "x",
    parent$node,
    "gap"
  )
  if (gap < 0) {
    l_abort("gap cannot be negative.", node = parent$id, property = "gap")
  }
  children <- lapply(parent$node$children, function(child) {
    original <- child
    flow <- parent$node$flow
    automatic <- !has_coordinates(child)
    if (flow %in% c("row", "column") && automatic) {
      child$left <- l_length(
        if (flow == "row") cursor + safe_area[["left"]] else safe_area[["left"]]
      )
      child$top <- l_length(
        if (flow == "column") {
          cursor + safe_area[["top"]]
        } else {
          safe_area[["top"]]
        }
      )
    }
    result <- resolve_node(child, context, safe_area)
    result$node <- original
    if (flow %in% c("row", "column") && automatic) {
      dimension <- if (flow == "row") "width" else "height"
      sides <- if (flow == "row") c("left", "right") else c("top", "bottom")
      cursor <<- cursor +
        result$box[[dimension]] +
        sum(result$margin[sides]) +
        gap
    }
    result
  })
  if (
    any(vapply(
      children,
      function(child) child$node$collision != "none",
      logical(1)
    ))
  ) {
    children <- resolve_collisions(children, context, safe_area)
  }
  lapply(children, function(child) {
    validate_resolved(child, context)
    inner <- content_box(child)
    if (length(child$node$children)) {
      if (any(inner[c("width", "height")] <= 0)) {
        l_warn(
          "No positive content area for child nodes.",
          "overflow",
          child$id,
          "padding"
        )
      } else {
        child_context <- new_layout_context(
          inner[["width"]],
          inner[["height"]],
          context$dpi,
          context$root_width,
          context$root_height
        )
        child$children <- resolve_children(child, child_context)
      }
    }
    child
  })
}

root_context <- function(node, width, height, dpi) {
  defaults <- c(width = 800, height = 600)
  if (grDevices::dev.cur() != 1L) {
    defaults <- c(
      width = grid::convertWidth(
        grid::unit(1, "npc"),
        "inches",
        valueOnly = TRUE
      ) *
        96,
      height = grid::convertHeight(
        grid::unit(1, "npc"),
        "inches",
        valueOnly = TRUE
      ) *
        96
    )
  } else {
    reference <- new_layout_context(
      defaults[["width"]],
      defaults[["height"]],
      dpi
    )
    for (dimension in names(defaults)) {
      declaration <- node[[dimension]]
      if (declaration$kind != "auto") {
        defaults[[dimension]] <- resolve_length(
          declaration,
          reference,
          if (dimension == "width") "x" else "y"
        )
      }
    }
  }
  new_layout_context(
    width %||% defaults[["width"]],
    height %||% defaults[["height"]],
    dpi
  )
}

l_resolve <- function(object, width = NULL, height = NULL, dpi = 96) {
  node <- as_l_node(object)
  if (node$kind != "viewport") {
    node <- l_viewport(list(node), width = node$width, height = node$height)
  }
  node <- assign_scene_ids(node)
  if (anyDuplicated(scene_ids(node))) {
    l_abort("Node IDs must be unique within a scene.", property = "id")
  }
  context <- root_context(node, width, height, dpi)
  with_grid_context(context, {
    root <- list(
      id = node$id,
      node = node,
      box = c(x = 0, y = 0, width = context$width, height = context$height),
      margin = resolve_edges(node$margin, context, node, "margin"),
      padding = resolve_edges(node$padding, context, node, "padding"),
      border = resolve_border(node$border, context, node),
      content = NULL,
      scale = 1,
      intrinsic = c(width = context$width, height = context$height),
      collision_candidates = list()
    )
    inner <- content_box(root)
    if (any(inner[c("width", "height")] <= 0)) {
      l_abort("Root padding/border leaves no content area.", node = node$id)
    }
    child_context <- new_layout_context(
      inner[["width"]],
      inner[["height"]],
      dpi,
      context$width,
      context$height
    )
    root$children <- resolve_children(root, child_context)
    structure(list(root = root, context = context), class = "l_layout")
  })
}

l_measure <- function(
  object,
  viewport = list(width = 800, height = 600),
  dpi = 96
) {
  node <- as_l_node(object)
  context <- new_layout_context(
    viewport$width,
    viewport$height,
    dpi,
    viewport$root_width %||% viewport$width,
    viewport$root_height %||% viewport$height
  )
  with_grid_context(context, {
    resolved <- resolve_node(
      node,
      context,
      c(top = 0, right = 0, bottom = 0, left = 0)
    )
    list(
      width = resolved$box[["width"]],
      height = resolved$box[["height"]],
      intrinsic = resolved$intrinsic,
      units = "px"
    )
  })
}
