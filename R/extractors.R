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

#' Create an independent element-extraction registry
#'
#' Return the built-in adapters used to extract semantic components from
#' ggplots and accept native grobs. Each call creates an independent registry;
#' extending one does not affect other registries or future calls.
#'
#' @details
#' Native ggplot types are `title`, `subtitle`, `caption`, `tag`, `legend`,
#' `x_axis`, `y_axis`, `x_axis_title`, `y_axis_title`, `panel`, `strip`,
#' `plot_background` and `panel_background`.
#'
#' Direct grobs additionally support `north_arrow`, `scale_bar`, `map_frame`,
#' `credits`, `annotation`, `inset` and `custom`. These names classify supplied
#' grobs; they do not construct geographic arrows, scales or map insets.
#' Native adapters also accept direct grobs without extracting subcomponents.
#'
#' Use [l_register_element()] to add or replace an adapter, then pass the
#' returned registry explicitly to [l_get_element()]. There is no active-plot
#' lookup, global registration or required cowplot adapter.
#'
#' @returns A named list with classes `l_registry` and `list`. Each entry holds
#'   extraction, acceptance, optional measurement and optional styling callbacks,
#'   plus an internal native-adapter flag.
#' @seealso [l_register_element()], [l_get_element()]
#' @examples
#' registry <- l_registry()
#' names(registry)
#' note <- l_get_element(l_text("Survey credits"), "credits", registry = registry)
#' print(note)
#' @export
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

#' Register a custom semantic element adapter
#'
#' Add or replace an extraction adapter in a copy of an explicit registry.
#' This extends the accepted source types without modifying the layout engine
#' or installing global callbacks.
#'
#' @param type Single nonempty character identifier for the semantic element.
#'   An existing entry with this name is replaced in the returned registry.
#' @param can_extract Optional predicate called with the source as its first
#'   argument. It must return `TRUE` to accept the source. The default accepts
#'   any source and leaves validation to the extractor.
#' @param extract Required extraction function, unless `extractor` is supplied.
#'   Called with the source as its first argument and, when requested, a named
#'   `which` argument. It must return a grid grob. Accepting `...` is useful for
#'   forward-compatible adapters.
#' @param measure Optional `function(element, context)` returning named `width`
#'   and `height` values in logical pixels. `element$content` contains the
#'   prepared, styled grob. With `NULL`, grid intrinsic measurement is used.
#' @param style Optional `function(element, style, context)` returning a grob.
#'   It receives resolved style lengths in logical pixels and is responsible
#'   for applying custom styles and validating any extra property names.
#' @param registry An `l_registry` from [l_registry()] or an earlier call to
#'   this function. The original value is not modified.
#' @param extractor,measurer Aliases for `extract` and `measure`. The canonical
#'   arguments take precedence when non-`NULL`.
#'
#' @details
#' Callback contexts contain `width`, `height`, `root_width`, `root_height`
#' and `dpi`. Widths and heights are logical pixels. Graphical preparation may
#' occur more than once as measurement and rendering use different contexts;
#' callbacks should be deterministic and avoid external side effects.
#'
#' A custom style callback allows additional style names through [l_style()].
#' Logical lengths resolved by lplot still use its units, not raw grid font
#' points. Returning anything other than a grob from extraction is rejected.
#' `NULL` from extraction is accepted only when [l_get_element()] is called
#' with `missing = "null"`.
#'
#' @returns An updated `l_registry`. Always retain the return value and pass it
#'   to [l_get_element()]; the default registry is never changed globally.
#' @seealso [l_registry()], [l_get_element()], [l_measure()], [l_style()]
#' @examples
#' registry <- l_register_element(
#'   "badge",
#'   can_extract = is.character,
#'   extract = function(plot, ...) l_text(plot, fontsize = 10),
#'   measure = function(element, context) c(width = 100, height = 24)
#' )
#' badge <- l_get_element("Survey area", "badge", registry = registry)
#' l_measure(badge)
#' "badge" %in% names(l_registry())
#' @export
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

#' Extract and normalize a semantic graphical element
#'
#' Extract a component from an explicit ggplot source or wrap a supplied grob
#' as a semantic element. The source and unrelated components are preserved;
#' the returned element can be measured, restyled and positioned independently.
#'
#' @param plot Explicit source ggplot, grid grob or object accepted by a custom
#'   registry adapter. Required; no implicit current or active plot is used.
#' @param type Single character semantic type registered in `registry`. See
#'   [l_registry()] for built-in types and direct-grob categories.
#' @param width,height Logical border-box dimensions accepted by [l_length()].
#'   `"auto"` uses intrinsic content or available space, depending on the type.
#' @param style Optional named list of element-specific overrides passed to
#'   [l_style()], such as `color` and `font_size`.
#' @param position Optional named list of node properties applied after the
#'   initial properties, before style overrides; see [l_place()].
#' @param responsive Logical permission for collision-driven shrinking.
#'   This does not disable resolution of logical units on subsequent draws.
#' @param collision Collision policy: `"none"`, `"avoid"`, `"shrink"` or
#'   `"avoid-and-shrink"`. See [l_place()] for movement and shrinking conditions.
#' @param priority Finite numeric priority for collision placement. Larger
#'   values are handled first.
#' @param registry Explicit `l_registry`, normally from [l_registry()] or
#'   [l_register_element()].
#' @param which Optional positive integer selecting one nonempty component
#'   when several match. With `NULL`, native extraction preserves the matched
#'   components' gtable arrangement. Custom extractors must handle this argument
#'   if supplied.
#' @param missing `"error"` raises an error for an absent component; `"null"`
#'   returns `NULL` for permitted absence. It does not hide unsupported sources,
#'   unknown extractors or arbitrary callback errors.
#' @param ... Additional named node properties accepted by [l_place()], such
#'   as `id`, `metadata`, `padding` and dimension constraints. These arguments
#'   are not forwarded to the extraction callback.
#'
#' @details
#' For a ggplot source, the effective global and plot-specific theme is
#' captured at extraction. Later changes to the global ggplot theme do not
#' silently restyle the element. Native content may be rebuilt for the final
#' context to apply responsive typography while preserving inherited styling.
#'
#' Extraction is separate from removal. To avoid drawing a title or legend
#' twice, use [l_without()] on the base plot when composing extracted elements.
#' Direct grobs are accepted as supplied; choosing `"north_arrow"` or
#' `"scale_bar"` does not perform geographic calculations.
#'
#' @section Conditions:
#' Missing native components raise `lplot_missing_element`; unknown types
#' raise `lplot_missing_extractor`; rejected sources raise
#' `lplot_unsupported_source`; non-grob extraction results raise
#' `lplot_invalid_extractor`. These inherit from `lplot_error`.
#'
#' @returns An `l_element` inheriting from `l_node`, or `NULL` when permitted
#'   absence is requested. The node retains its source, adapter and declarations.
#' @seealso [l_without()], [l_style()], [l_register_element()], [l_place()]
#' @examples
#' plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'   ggplot2::geom_point() +
#'   ggplot2::labs(title = "Efficiency")
#' title <- l_get_element(plot, "title",
#'   style = list(color = "#194E70", font_size = "clamp(10pt, 2vmin, 18pt)")
#' )
#' l_measure(title, viewport = list(width = 600, height = 400))
#' l_get_element(ggplot2::ggplot(), "title", missing = "null")
#' credits <- l_get_element(l_text("Source: survey"), "credits",
#'   metadata = list(source = "survey")
#' )
#' print(credits)
#' @export
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

#' Remove semantic components from a copy of a ggplot
#'
#' Hide selected labels, legends, axes or backgrounds without changing the
#' original plot. Use this when separately extracted components are being
#' composed elsewhere in an lplot scene.
#'
#' @param plot A ggplot object. Grobs and lplot scene nodes are not accepted.
#' @param type Character vector of native semantic types to remove. Supported
#'   types are `title`, `subtitle`, `caption`, `tag`, `legend`, `x_axis`,
#'   `y_axis`, `x_axis_title`, `y_axis_title`, `strip`, `plot_background` and
#'   `panel_background`. Removing `panel` is deliberately unsupported.
#'
#' @details
#' Text labels are cleared with [ggplot2::labs()], legends are hidden through
#' the theme, and other components are replaced with blank theme elements.
#' Removing an axis hides its text, ticks and line; its title is a separate
#' type. Removing strips blanks strip text and backgrounds. This is not an
#' operation on the plot's data, scales or geographic extent.
#'
#' Unknown types raise an `lplot_error`. Unsupported source classes raise
#' `lplot_unsupported_source`. No extractor registry is consulted.
#'
#' @returns A modified ggplot value. The supplied `plot` is unchanged.
#' @seealso [l_get_element()], [l_viewport()], [l_place()]
#' @examples
#' plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'   ggplot2::geom_point() +
#'   ggplot2::labs(title = "Efficiency")
#' title <- l_get_element(plot, "title")
#' base <- l_without(plot, c("title", "x_axis_title"))
#' l_get_element(base, "title", missing = "null")
#' l_render(l_viewport(list(
#'   l_place(base, left = 0, right = 0, top = 40, bottom = 0),
#'   l_place(title, left = 20, top = 8)
#' )), width = 400, height = 300)
#' @export
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
