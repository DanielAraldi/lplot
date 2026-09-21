with_grid_context <- function(context, code) {
  opened <- grDevices::dev.cur() == 1L
  if (opened) {
    grDevices::pdf(
      file = NULL,
      width = context$width / 96,
      height = context$height / 96
    )
    on.exit(grDevices::dev.off(), add = TRUE)
  }
  grid::pushViewport(grid::viewport(
    width = grid::unit(context$width / 96, "inches"),
    height = grid::unit(context$height / 96, "inches")
  ))
  on.exit(grid::popViewport(), add = TRUE, after = FALSE)
  force(code)
}

measure_grob <- function(grob, context) {
  with_grid_context(context, {
    c(
      width = grid::convertWidth(
        grid::grobWidth(grob),
        "inches",
        valueOnly = TRUE
      ) *
        96,
      height = grid::convertHeight(
        grid::grobHeight(grob),
        "inches",
        valueOnly = TRUE
      ) *
        96
    )
  })
}
