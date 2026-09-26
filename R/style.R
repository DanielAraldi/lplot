text_types <- c(
  "title",
  "subtitle",
  "caption",
  "tag",
  "x_axis",
  "y_axis",
  "x_axis_title",
  "y_axis_title",
  "strip",
  "legend"
)

#' Override an element's style without modifying its source
#'
#' Return a scene node with explicit style overrides. Unspecified properties
#' retain their source semantics, and responsive lengths are evaluated in the
#' actual measurement or drawing context.
#'
#' @param object An extracted element, grid grob, ggplot or lplot scene node.
#'   Non-node objects are first normalized as scene nodes.
#' @param ... Uniquely named style properties. Supported names depend on the
#'   element type; unknown or inapplicable names raise an `lplot_error`.
#'
#' @section Supported styles:
#' * Text elements: `color` (or `colour`, but not both), `font_family`,
#'   `font_size`, `font_face` and `line_height`.
#' * Generic grobs: the text properties plus `fill`, `line_width`, `line_type`.
#' * Plot/panel background elements: `fill`, `color`, `line_width`, `line_type`.
#' * Native legends additionally accept `legend.direction` (`"horizontal"`
#'   or `"vertical"`), `legend.key_width` and `legend.key_height`.
#' * Elements accept presentation properties `background`, `border`, `padding`,
#'   `margin`, `alpha` and `opacity`. Opacity values must be between zero and one;
#'   `opacity` takes precedence over `alpha` when both are present.
#' * Whole plots and viewports accept only `background`, `border`, `padding`
#'   and `margin`, not typography or alpha overrides.
#'
#' @details
#' `font_size`, `line_width`, `legend.key_width` and `legend.key_height` accept
#' logical lengths including `clamp()` expressions, but not `auto`. A numeric
#' `font_size` is in logical pixels; use `"12pt"` for twelve points. This
#' differs from the raw grid `fontsize` argument of [l_text()]. Edge and border
#' properties follow [l_place()] and [l_viewport()] conventions.
#'
#' Native ggplot overrides target only the selected semantic theme component.
#' Axis typography targets axis text, not tick geometry. For direct grobs,
#' applicable graphical overrides are applied recursively to descendants, so
#' this is an explicit restyling operation rather than mere group inheritance.
#' Adapter-defined style callbacks may support additional names; see
#' [l_register_element()].
#'
#' Repeated calls merge explicit overrides without mutating the source or
#' earlier nodes. Styling does not wrap text or guarantee that it fits a box.
#' Use [l_measure()] and [l_resolve()] to inspect the resulting geometry.
#'
#' @returns A styled `l_node`, retaining any element or viewport subclasses.
#'   With no overrides, returns the normalized node unchanged.
#' @seealso [l_get_element()], [l_text()], [l_measure()], [l_register_element()]
#' @examples
#' original <- l_get_element(l_text("Survey area", fontsize = 10), "annotation")
#' styled <- l_style(original,
#'   color = "#194E70",
#'   font_size = "clamp(10pt, 2vmin, 18pt)", background = "white", padding = 4
#' )
#' l_measure(original)
#' l_measure(styled, list(width = 800, height = 600))
#' viewport <- l_style(l_viewport(), background = "#EDF3F5", padding = "2mm")
#' print(viewport)
#' @export
l_style <- function(object, ...) {
  node <- as_l_node(object)
  type <- node$type %||% node$kind
  style <- list(...)
  if (!length(style)) {
    return(node)
  }
  if (
    is.null(names(style)) ||
      any(!nzchar(names(style))) ||
      anyDuplicated(names(style))
  ) {
    l_abort("Styles must be uniquely named.")
  }
  if (!is.null(style$colour)) {
    if (!is.null(style$color)) {
      l_abort("Use color or colour, not both.")
    }
    style$color <- style$colour
    style$colour <- NULL
  }
  presentation <- c(
    "background",
    "border",
    "padding",
    "margin",
    "alpha",
    "opacity"
  )
  typography <- c(
    "color",
    "font_family",
    "font_size",
    "font_face",
    "line_height"
  )
  generic <- c(typography, "fill", "line_width", "line_type")
  allowed <- if (type %in% text_types) {
    c(presentation, typography)
  } else {
    presentation
  }
  if (type == "legend") {
    allowed <- c(
      allowed,
      "legend.direction",
      "legend.key_width",
      "legend.key_height"
    )
  }
  if (!type %in% native_types || grid::is.grob(node$source)) {
    allowed <- c(presentation, generic)
  }
  if (type %in% c("plot_background", "panel_background")) {
    allowed <- c(presentation, "fill", "color", "line_width", "line_type")
  }
  if (node$kind %in% c("plot", "viewport")) {
    allowed <- c("background", "border", "padding", "margin")
  }
  if (is.function(node$adapter$style)) {
    allowed <- union(allowed, names(style))
  }
  invalid <- setdiff(names(style), allowed)
  if (length(invalid)) {
    l_abort(
      paste0(
        "Unsupported style for ",
        type,
        ": ",
        paste(invalid, collapse = ", ")
      ),
      property = "style"
    )
  }
  for (property in intersect(
    names(style),
    c("font_size", "line_width", "legend.key_width", "legend.key_height")
  )) {
    style[[property]] <- l_length(style[[property]])
    if (length_has_auto(style[[property]])) {
      l_abort(paste0(property, " cannot be auto."))
    }
  }
  for (property in intersect(names(style), c("alpha", "opacity"))) {
    value <- scalar_number(style[[property]], property)
    if (value < 0 || value > 1) {
      l_abort(paste0(property, " must be between 0 and 1."))
    }
  }
  if (
    !is.null(style$legend.direction) &&
      !style$legend.direction %in% c("horizontal", "vertical")
  ) {
    l_abort("legend.direction must be horizontal or vertical.")
  }
  for (property in intersect(
    names(style),
    c("background", "border", "padding", "margin")
  )) {
    node <- update_node(
      node,
      stats::setNames(list(style[[property]]), property)
    )
  }
  node$style <- utils::modifyList(node$style %||% list(), style)
  node
}

theme_keys <- function(type) {
  switch(type,
    title = "plot.title",
    subtitle = "plot.subtitle",
    caption = "plot.caption",
    tag = "plot.tag",
    legend = c("legend.title", "legend.text"),
    x_axis = c("axis.text.x.bottom", "axis.text.x.top"),
    y_axis = c("axis.text.y.left", "axis.text.y.right"),
    x_axis_title = c("axis.title.x.bottom", "axis.title.x.top"),
    y_axis_title = c("axis.title.y.left", "axis.title.y.right"),
    strip = c(
      "strip.text.x.top",
      "strip.text.x.bottom",
      "strip.text.y.left",
      "strip.text.y.right"
    ),
    plot_background = "plot.background",
    panel_background = "panel.background",
    character()
  )
}

resolved_style <- function(style, context) {
  for (property in names(style)) {
    if (inherits(style[[property]], "l_length")) {
      value <- resolve_length(
        style[[property]],
        context,
        if (property == "legend.key_height") "y" else "x"
      )
      if (!is.finite(value) || value < 0) {
        l_abort(
          paste0("Invalid resolved style: ", property),
          property = property
        )
      }
      style[[property]] <- value
    }
  }
  style
}

style_source <- function(source, type, style) {
  text <- list()
  mapping <- c(
    color = "colour",
    font_family = "family",
    font_face = "face",
    line_height = "lineheight"
  )
  for (property in intersect(names(style), names(mapping))) {
    text[[mapping[[property]]]] <- style[[property]]
  }
  if (!is.null(style$font_size)) {
    text$size <- style$font_size * 72 / 96
  }
  rectangle <- list()
  for (property in intersect(names(style), c("color", "fill", "line_type"))) {
    key <- switch(property,
      color = "colour",
      fill = "fill",
      line_type = "linetype"
    )
    rectangle[[key]] <- style[[property]]
  }
  if (!is.null(style$line_width)) {
    rectangle$linewidth <- style$line_width * 25.4 / 96
  }
  properties <- list()
  overrides <- if (type %in% text_types) text else rectangle
  if (length(overrides)) {
    for (key in theme_keys(type)) {
      inherited <- ggplot2::calc_element(key, source$theme)
      override <- do.call(
        if (type %in% text_types) {
          ggplot2::element_text
        } else {
          ggplot2::element_rect
        },
        overrides
      )
      single <- do.call(ggplot2::theme, stats::setNames(list(inherited), key)) +
        do.call(ggplot2::theme, stats::setNames(list(override), key))
      properties[[key]] <- single[[key]]
    }
  }
  if (!is.null(style$legend.direction)) {
    properties$legend.direction <- style$legend.direction
  }
  for (property in intersect(
    names(style),
    c("legend.key_width", "legend.key_height")
  )) {
    properties[[sub("_", ".", property, fixed = TRUE)]] <- l_unit(
      style[[property]] / 96,
      "inches"
    )
  }
  if (length(properties)) {
    source <- source + do.call(ggplot2::theme, properties)
  }
  source
}

style_grob <- function(grob, style) {
  parameters <- list()
  mapping <- c(
    color = "col",
    fill = "fill",
    font_family = "fontfamily",
    line_type = "lty",
    line_width = "lwd"
  )
  for (property in intersect(names(style), names(mapping))) {
    parameters[[mapping[[property]]]] <- style[[property]]
  }
  if (!is.null(style$font_face)) {
    parameters$fontface <- style$font_face
  }
  if (!is.null(style$font_size)) {
    parameters$fontsize <- style$font_size * 72 / 96
  }
  if (!is.null(style$line_height)) {
    parameters$lineheight <- style$line_height
  }
  alpha <- style$opacity %||% style$alpha
  if (!is.null(alpha)) {
    parameters$alpha <- alpha
  }
  if (length(parameters)) {
    existing <- as.list(grob$gp %||% grid::gpar())
    if (!is.null(parameters$fontface)) {
      existing$font <- NULL
    }
    grob$gp <- do.call(grid::gpar, utils::modifyList(existing, parameters))
  }
  if (inherits(grob, "gtable")) {
    grob$grobs <- lapply(grob$grobs, style_grob, style = style)
  }
  if (!is.null(grob$children)) {
    grob$children <- do.call(
      grid::gList,
      lapply(grob$children, style_grob, style = style)
    )
  }
  grob
}

prepare_content <- function(node, context) {
  if (node$kind == "viewport") {
    return(NULL)
  }
  if (node$kind == "plot") {
    return(ggplot2::ggplotGrob(node$content))
  }
  style <- resolved_style(node$style %||% list(), context)
  if (is.function(node$adapter$style)) {
    return(node$adapter$style(node, style, context))
  }
  if (inherits(node$source, "ggplot") && isTRUE(node$adapter$native)) {
    source <- style_source(node$source, node$type, style)
    grob <- extract_native(source, node$type, node$selection)
    return(style_grob(
      grob,
      style[intersect(names(style), c("alpha", "opacity"))]
    ))
  }
  style_grob(node$content, style)
}
