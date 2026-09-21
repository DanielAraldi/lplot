#' Declare a device-independent layout length
#'
#' Normalize a numeric value, a length string or an existing logical length.
#' The result stores a declaration rather than a device-dependent measurement;
#' it is evaluated in context by the layout resolver at render time.
#'
#' @param value A single finite number, an existing `l_length`, or a string
#'   containing a number and unit, `"auto"`, or a supported length expression.
#' @param unit Unit for numeric `value`: `"px"`, `"%"`, `"pt"`, `"mm"`, `"cm"`,
#'   `"in"`, `"vw"`, `"vh"`, `"vmin"` or `"vmax"`. Ignored when `value` is
#'   already a string or an `l_length`.
#'
#' @details
#' One logical pixel is 1/96 inch and one point is 1/72 inch. Device DPI affects
#' raster density, not the definition of these lengths. Percentages use the
#' containing content box on the relevant axis. `vw` and `vh` are percentages
#' of the root width and height; `vmin` and `vmax` use their minimum and maximum.
#'
#' Strings accept nested `min()`, `max()` and `clamp()` expressions. Parsing
#' does not evaluate R code. `auto` is not allowed inside an expression. A
#' character value such as `"24"` needs a unit; the numeric value `24` uses
#' `unit`. Negative values can describe offsets, but negative dimensions are
#' rejected by the node or resolver where they are used.
#'
#' `auto` is context-dependent: it uses intrinsic content size for ordinary
#' elements and available space for plots, panels and viewports. It is not a
#' request for text wrapping. These values are not [grid::unit()] objects.
#'
#' @returns An S3 `l_length` object containing an unresolved value or expression.
#'   An existing `l_length` is returned unchanged.
#' @seealso [l_clamp()], [l_place()], [l_resolve()]
#' @examples
#' l_length("50%")
#' l_length(2, "cm")
#' l_length("min(40mm, max(10px, 25%))")
#' scene <- l_viewport(list(
#'   l_place(l_rect(fill = "grey80"), width = l_length("50%"), height = 30)
#' ))
#' l_resolve(scene, width = 400, height = 100)$root$children[[1]]$box
#' @export
l_length <- function(value = "auto", unit = "px") {
  if (inherits(value, "l_length")) {
    return(value)
  }
  valid_units <- c(
    "px",
    "%",
    "pt",
    "mm",
    "cm",
    "in",
    "vw",
    "vh",
    "vmin",
    "vmax"
  )
  if (is.numeric(value)) {
    value <- scalar_number(value, "length")
    if (length(unit) != 1L || !unit %in% valid_units) {
      l_abort("Unsupported length unit.", property = "unit")
    }
    return(structure(
      list(kind = "value", value = value, unit = unit),
      class = "l_length"
    ))
  }
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    l_abort(
      "A length must be a number, a length string or an l_length.",
      property = "length"
    )
  }
  value <- trimws(value)
  if (identical(value, "auto")) {
    return(structure(list(kind = "auto"), class = "l_length"))
  }
  matched <- regmatches(value, regexec("^(clamp|min|max)\\((.*)\\)$", value))[[
    1L
  ]]
  if (length(matched)) {
    arguments <- split_length_arguments(matched[[3L]])
    if (
      (matched[[2L]] == "clamp" && length(arguments) != 3L) ||
        !length(arguments)
    ) {
      l_abort(
        "clamp() needs three lengths; min()/max() need at least one.",
        property = "length"
      )
    }
    arguments <- lapply(arguments, l_length)
    if (any(vapply(arguments, length_has_auto, logical(1)))) {
      l_abort("auto is not valid inside a length expression.")
    }
    return(structure(
      list(kind = matched[[2L]], arguments = arguments),
      class = "l_length"
    ))
  }
  matched <- regmatches(
    value,
    regexec(
      "^([+-]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?)(px|%|pt|mm|cm|in|vw|vh|vmin|vmax)$",
      value,
      perl = TRUE
    )
  )[[1L]]
  if (!length(matched)) {
    l_abort(paste0("Invalid length: '", value, "'."), property = "length")
  }
  l_length(as.numeric(matched[[2L]]), matched[[3L]])
}

split_length_arguments <- function(text) {
  characters <- strsplit(text, "", fixed = TRUE)[[1L]]
  depth <- 0L
  start <- 1L
  arguments <- character()
  for (index in seq_along(characters)) {
    character <- characters[[index]]
    if (character == "(") {
      depth <- depth + 1L
    }
    if (character == ")") {
      depth <- depth - 1L
    }
    if (depth < 0L) {
      l_abort("Unbalanced length expression.")
    }
    if (character == "," && depth == 0L) {
      arguments <- c(arguments, substr(text, start, index - 1L))
      start <- index + 1L
    }
  }
  if (depth != 0L) {
    l_abort("Unbalanced length expression.")
  }
  arguments <- c(arguments, substr(text, start, nchar(text)))
  if (any(!nzchar(trimws(arguments)))) {
    l_abort("Empty argument in length expression.")
  }
  arguments
}

length_has_auto <- function(length) {
  length$kind == "auto" ||
    (!is.null(length$arguments) &&
      any(vapply(length$arguments, length_has_auto, logical(1))))
}

#' Bound a responsive length between a minimum and a maximum
#'
#' Build an unresolved constraint that uses a preferred length while respecting
#' lower and upper bounds. Different units can be combined and are compared
#' only when a layout context is available.
#'
#' @param minimum,preferred,maximum Values accepted by [l_length()], except
#'   `"auto"`. Bare numbers are logical pixels. Nested expressions are allowed.
#'
#' @details
#' At resolution time, the result is the preferred value limited to the
#' resolved interval. A minimum greater than the maximum in that context raises
#' an error; construction alone cannot detect all mixed-unit contradictions.
#' This is equivalent to a `clamp()` string accepted by [l_length()].
#'
#' @returns An S3 `l_length` object with an unresolved clamp expression.
#' @seealso [l_length()], [l_style()], [l_place()]
#' @examples
#' width <- l_clamp("80px", "50%", "180px")
#' scene <- l_viewport(list(l_place(l_rect(), width = width, height = 30)))
#' l_resolve(scene, width = 200, height = 60)$root$children[[1]]$box
#' l_resolve(scene, width = 500, height = 60)$root$children[[1]]$box
#' @export
l_clamp <- function(minimum, preferred, maximum) {
  arguments <- lapply(list(minimum, preferred, maximum), l_length)
  if (any(vapply(arguments, length_has_auto, logical(1)))) {
    l_abort("clamp() does not accept auto.")
  }
  structure(list(kind = "clamp", arguments = arguments), class = "l_length")
}

format_length <- function(length) {
  if (length$kind == "auto") {
    return("auto")
  }
  if (length$kind == "value") {
    return(paste0(length$value, length$unit))
  }
  paste0(
    length$kind,
    "(",
    paste(
      vapply(length$arguments, format_length, character(1)),
      collapse = ", "
    ),
    ")"
  )
}

#' Print a logical length without resolving it
#'
#' Display the stored value or nested expression in a compact form. Printing
#' does not convert units, create a device or draw graphics.
#'
#' @param x An `l_length` object, usually created by [l_length()] or [l_clamp()].
#' @param ... Additional arguments required by the generic; currently unused.
#' @returns `x`, invisibly and unchanged. The formatted declaration is written
#'   to the console as a side effect.
#' @seealso [l_length()], [l_clamp()], [l_resolve()]
#' @examples
#' value <- l_clamp("10pt", "2vmin", "18pt")
#' returned <- print(value)
#' identical(returned, value)
#' @export
print.l_length <- function(x, ...) {
  cat("<l_length> ", format_length(x), "\n", sep = "")
  invisible(x)
}

new_layout_context <- function(
  width,
  height,
  dpi = 96,
  root_width = width,
  root_height = height
) {
  list(
    width = scalar_number(width, "width", TRUE),
    height = scalar_number(height, "height", TRUE),
    root_width = scalar_number(root_width, "root_width", TRUE),
    root_height = scalar_number(root_height, "root_height", TRUE),
    dpi = scalar_number(dpi, "dpi", TRUE)
  )
}

resolve_length <- function(value, context, axis = "x", intrinsic = NA_real_) {
  value <- l_length(value)
  if (!axis %in% c("x", "y")) {
    l_abort("axis must be x or y.")
  }
  if (value$kind == "auto") {
    return(intrinsic)
  }
  if (value$kind != "value") {
    resolved <- vapply(
      value$arguments,
      resolve_length,
      numeric(1),
      context = context,
      axis = axis
    )
    return(switch(value$kind,
      min = min(resolved),
      max = max(resolved),
      clamp = {
        if (resolved[[1L]] > resolved[[3L]]) {
          l_abort("clamp minimum exceeds maximum.", property = "length")
        }
        max(resolved[[1L]], min(resolved[[2L]], resolved[[3L]]))
      }
    ))
  }
  multiplier <- switch(value$unit,
    px = 1,
    "%" = (if (axis == "x") context$width else context$height) / 100,
    pt = 96 / 72,
    mm = 96 / 25.4,
    cm = 96 / 2.54,
    "in" = 96,
    vw = context$root_width / 100,
    vh = context$root_height / 100,
    vmin = min(context$root_width, context$root_height) / 100,
    vmax = max(context$root_width, context$root_height) / 100
  )
  value$value * multiplier
}
