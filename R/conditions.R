l_abort <- function(
  message,
  subclass = "invalid",
  node = NULL,
  property = NULL
) {
  if (!is.null(node)) {
    message <- paste0("Node '", node, "': ", message)
  }
  stop(structure(
    list(
      message = message,
      call = NULL,
      node = node,
      property = property
    ),
    class = c(paste0("lplot_", subclass), "lplot_error", "error", "condition")
  ))
}

l_warn <- function(message, subclass, node = NULL, property = NULL) {
  if (!is.null(node)) {
    message <- paste0("Node '", node, "': ", message)
  }
  warning(structure(
    list(
      message = message,
      call = NULL,
      node = node,
      property = property
    ),
    class = c(
      paste0("lplot_", subclass),
      "lplot_warning",
      "warning",
      "condition"
    )
  ))
}

scalar_number <- function(value, property, positive = FALSE) {
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value) ||
      (positive && value <= 0)
  ) {
    l_abort(
      paste0(
        property,
        " must be a finite ",
        if (positive) "positive " else "",
        "number."
      ),
      property = property
    )
  }
  as.numeric(value)
}

`%||%` <- function(value, fallback) if (is.null(value)) fallback else value
