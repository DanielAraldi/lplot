template_gpar <- function(gp = NULL, overrides = list()) {
  for (parameters in list(gp, overrides)) {
    if (is.null(parameters)) {
      next
    }
    if (
      !is.list(parameters) ||
        (length(parameters) &&
          (is.null(names(parameters)) ||
            anyNA(names(parameters)) ||
            any(!nzchar(names(parameters))) ||
            anyDuplicated(names(parameters))))
    ) {
      l_abort("Graphical parameters must be a gpar or a uniquely named list.",
        property = "gp"
      )
    }
  }
  if (!inherits(gp, "gpar")) {
    gp <- do.call(grid::gpar, gp %||% list())
  }
  overrides <- do.call(grid::gpar, overrides)
  gp[names(overrides)] <- overrides
  gp
}

l_template <- function(
  ...,
  children = NULL,
  name = NULL,
  gp = NULL,
  vp = NULL,
  childrenvp = NULL,
  cl = NULL
) {
  if (!is.null(children) && !is.list(children)) {
    l_abort("children must be a list of grobs.", property = "children")
  }
  children <- c(list(...), children)
  if (!all(vapply(children, grid::is.grob, logical(1)))) {
    l_abort("Template children must be grobs.", "unsupported_source",
      property = "children"
    )
  }
  if (!is.null(gp)) {
    gp <- template_gpar(gp)
  }
  do.call(
    grid::grobTree,
    c(
      unname(children),
      list(name = name, gp = gp, vp = vp, childrenvp = childrenvp, cl = cl)
    )
  )
}

l_rect <- function(
  x = grid::unit(0.5, "npc"),
  y = grid::unit(0.5, "npc"),
  width = grid::unit(1, "npc"),
  height = grid::unit(1, "npc"),
  just = "centre",
  hjust = NULL,
  vjust = NULL,
  default.units = "npc",
  name = NULL,
  gp = NULL,
  vp = NULL,
  ...
) {
  grid::rectGrob(
    x = x, y = y, width = width, height = height,
    just = just, hjust = hjust, vjust = vjust, default.units = default.units,
    name = name, gp = template_gpar(gp, list(...)), vp = vp
  )
}

l_text <- function(
  label,
  x = grid::unit(0.5, "npc"),
  y = grid::unit(0.5, "npc"),
  just = "centre",
  hjust = NULL,
  vjust = NULL,
  rot = 0,
  check.overlap = FALSE,
  default.units = "npc",
  name = NULL,
  gp = NULL,
  vp = NULL,
  ...
) {
  grid::textGrob(
    label = label, x = x, y = y, just = just, hjust = hjust, vjust = vjust,
    rot = rot, check.overlap = check.overlap, default.units = default.units,
    name = name, gp = template_gpar(gp, list(...)), vp = vp
  )
}
