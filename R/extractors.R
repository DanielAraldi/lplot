native_types <- c(
  "title",
  "subtitle",
  "caption",
  "tag",
  "legend",
  "x_axis",
  "y_axis",
  "x_axis_title",
  "y_axis_title",
  "panel",
  "strip",
  "plot_background",
  "panel_background"
)

native_patterns <- c(
  title = "^title$",
  subtitle = "^subtitle$",
  caption = "^caption$",
  tag = "^tag$",
  legend = "^guide-box($|-)",
  x_axis = "^axis-[bt]($|-)",
  y_axis = "^axis-[lr]($|-)",
  x_axis_title = "^xlab-[bt]$",
  y_axis_title = "^ylab-[lr]$",
  panel = "^panel($|-)",
  strip = "^strip-",
  plot_background = "^background$",
  panel_background = "^panel($|-)"
)

find_grobs <- function(grob, pattern) {
  if (grepl(pattern, grob$name %||% "")) {
    return(list(grob))
  }
  children <- if (inherits(grob, "gtable")) {
    grob$grobs
  } else {
    as.list(grob$children)
  }
  unlist(lapply(children, find_grobs, pattern = pattern), recursive = FALSE)
}

is_empty_grob <- function(grob) inherits(grob, c("zeroGrob", "null"))

extract_native <- function(plot, type, which = NULL) {
  table <- ggplot2::ggplotGrob(plot)
  matches <- grepl(native_patterns[[type]], table$layout$name)
  matches <- matches & !vapply(table$grobs, is_empty_grob, logical(1))
  if (type == "panel_background") {
    for (index in which(matches)) {
      backgrounds <- find_grobs(table$grobs[[index]], "^panel\\.background")
      table$grobs[[index]] <- if (length(backgrounds)) {
        backgrounds[[1L]]
      } else {
        grid::nullGrob()
      }
    }
    matches <- matches & !vapply(table$grobs, is_empty_grob, logical(1))
  }
  indices <- base::which(matches)
  if (!length(indices)) {
    l_abort(
      paste0("Source has no '", type, "' element."),
      "missing_element",
      property = type
    )
  }
  if (!is.null(which)) {
    if (
      !is.numeric(which) ||
        length(which) != 1L ||
        is.na(which) ||
        which != as.integer(which) ||
        which < 1 ||
        which > length(indices)
    ) {
      l_abort("which must select an existing element.", property = "which")
    }
    return(table$grobs[[indices[[which]]]])
  }
  if (length(indices) == 1L) {
    return(table$grobs[[indices]])
  }
  table$layout <- table$layout[matches, , drop = FALSE]
  table$grobs <- table$grobs[matches]
  gtable::gtable_trim(table)
}

native_adapter <- function(type) {
  force(type)
  list(
    can_extract = function(plot, ...) {
      inherits(plot, "ggplot") || grid::is.grob(plot)
    },
    extract = function(plot, which = NULL, ...) {
      if (grid::is.grob(plot)) {
        return(plot)
      }
      extract_native(plot, type, which)
    },
    measure = NULL,
    style = NULL,
    native = TRUE
  )
}

l_registry <- function() {
  entries <- stats::setNames(lapply(native_types, native_adapter), native_types)
  generic <- list(
    can_extract = function(plot, ...) grid::is.grob(plot),
    extract = function(plot, ...) plot,
    measure = NULL,
    style = NULL,
    native = FALSE
  )
  for (type in c(
    "north_arrow",
    "scale_bar",
    "map_frame",
    "credits",
    "annotation",
    "inset",
    "custom"
  )) {
    entries[[type]] <- generic
  }
  structure(entries, class = c("l_registry", "list"))
}

l_register_element <- function(
  type,
  can_extract = NULL,
  extract = NULL,
  measure = NULL,
  style = NULL,
  registry = l_registry(),
  extractor = NULL,
  measurer = NULL
) {
  if (
    !is.character(type) || length(type) != 1L || is.na(type) || !nzchar(type)
  ) {
    l_abort("type must be a nonempty string.")
  }
  if (!inherits(registry, "l_registry")) {
    l_abort("registry must be an l_registry.")
  }
  extract <- extract %||% extractor
  measure <- measure %||% measurer
  can_extract <- can_extract %||% function(plot, ...) TRUE
  for (callback in list(extract, can_extract)) {
    if (!is.function(callback)) {
      l_abort("Extraction callbacks must be functions.")
    }
  }
  for (callback in list(measure, style)) {
    if (!is.null(callback) && !is.function(callback)) {
      l_abort("Optional callbacks must be functions.")
    }
  }
  registry[[type]] <- list(
    can_extract = can_extract,
    extract = extract,
    measure = measure,
    style = style,
    native = FALSE
  )
  registry
}

l_get_element <- function(
  plot,
  type,
  width = "auto",
  height = "auto",
  style = NULL,
  position = NULL,
  responsive = TRUE,
  collision = "none",
  priority = 0,
  registry = l_registry(),
  which = NULL,
  missing = c("error", "null"),
  ...
) {
  missing <- match.arg(missing)
  if (base::missing(plot)) {
    l_abort(
      "plot is required; there is no implicit active plot.",
      "unsupported_source"
    )
  }
  if (!inherits(registry, "l_registry")) {
    l_abort("registry must be an l_registry.")
  }
  if (!is.character(type) || length(type) != 1L || is.na(type)) {
    l_abort("type must be a string.")
  }
  adapter <- registry[[type]]
  if (is.null(adapter)) {
    l_abort(
      paste0("No extractor registered for '", type, "'."),
      "missing_extractor"
    )
  }
  if (!isTRUE(adapter$can_extract(plot))) {
    l_abort(
      paste0("Unsupported source for '", type, "'."),
      "unsupported_source"
    )
  }
  source <- if (inherits(plot, "ggplot")) capture_plot(plot) else plot
  arguments <- list(source)
  if (!is.null(which)) {
    arguments$which <- which
  }
  content <- tryCatch(
    with_grid_context(
      new_layout_context(800, 600),
      do.call(adapter$extract, arguments)
    ),
    lplot_missing_element = function(condition) {
      if (missing == "null") {
        return(NULL)
      }
      stop(condition)
    }
  )
  if (is.null(content) && missing == "null") {
    return(NULL)
  }
  if (!grid::is.grob(content)) {
    l_abort("Extractor must return a grob.", "invalid_extractor")
  }
  node <- new_l_node("element", content, type, width, height)
  node$source <- source
  node$adapter <- adapter
  node$selection <- which
  node$style <- list()
  class(node) <- c("l_element", "l_node")
  node <- update_node(
    node,
    c(
      list(responsive = responsive, collision = collision, priority = priority),
      list(...)
    )
  )
  if (!is.null(position)) {
    node <- update_node(node, position)
  }
  if (!is.null(style)) {
    node <- do.call(l_style, c(list(object = node), style))
  }
  node
}

l_without <- function(plot, type) {
  if (!inherits(plot, "ggplot")) {
    l_abort("l_without() requires a ggplot.", "unsupported_source")
  }
  if (!is.character(type) || any(!type %in% native_types)) {
    l_abort("Unknown element type in l_without().")
  }
  if ("panel" %in% type) {
    l_abort(
      "Removing data panels is not supported; compose extracted elements instead."
    )
  }
  result <- plot
  labels <- intersect(type, c("title", "subtitle", "caption", "tag"))
  if (length(labels)) {
    result <- result +
      do.call(
        ggplot2::labs,
        stats::setNames(rep(list(NULL), length(labels)), labels)
      )
  }
  properties <- list()
  if ("legend" %in% type) {
    properties$legend.position <- "none"
  }
  keys <- list(
    x_axis = c("axis.text.x", "axis.ticks.x", "axis.line.x"),
    y_axis = c("axis.text.y", "axis.ticks.y", "axis.line.y"),
    x_axis_title = "axis.title.x",
    y_axis_title = "axis.title.y",
    strip = c("strip.text", "strip.background"),
    plot_background = "plot.background",
    panel_background = "panel.background"
  )
  for (item in intersect(type, names(keys))) {
    for (key in keys[[item]]) {
      properties[[key]] <- ggplot2::element_blank()
    }
  }
  if (length(properties)) {
    result <- result + do.call(ggplot2::theme, properties)
  }
  result
}
