test_that("placement returns copies and preserves unspecified constraints", {
  original <- l_viewport(width = 256, height = 128)
  placed <- l_place(original, left = "10%", top = "2mm")
  expect_null(original$left)
  expect_identical(placed$width, original$width)
  expect_identical(placed$left, l_length("10%"))
  expect_identical(l_place(placed, left = NULL)$left, NULL)
  expect_identical(l_place(placed, width = "auto")$width, l_length("auto"))
})

test_that("joining retains child-local declarations", {
  child <- l_viewport(list(l_place(
    grid::rectGrob(),
    x = "50%",
    y = "50%",
    anchor = "center"
  )))
  joined <- l_join(list(
    l_place(child, width = "60%"),
    l_place(child, left = "60%", width = "40%")
  ))
  expect_identical(joined$children[[1]]$children[[1]]$x, child$children[[1]]$x)
  expect_identical(joined$children[[2]]$children[[1]]$y, child$children[[1]]$y)
  expect_false(anyDuplicated(scene_ids(joined)) > 0)
  expect_identical(
    joined,
    l_join(list(
      l_place(child, width = "60%"),
      l_place(child, left = "60%", width = "40%")
    ))
  )
})

test_that("box declarations validate input and normalize edges", {
  expect_identical(
    normalize_edges(c("2px", "4px")),
    normalize_edges(list(
      top = "2px",
      bottom = "2px",
      left = "4px",
      right = "4px"
    ))
  )
  expect_identical(l_viewport(width = 2, units = "cm")$width, l_length("2cm"))
  for (properties in list(
    list(anchor = "north"),
    list(width = -1),
    list(priority = Inf),
    list(overflow = "silent"),
    list(unknown = 1),
    list(responsive = NA)
  )) {
    expect_error(
      do.call(l_place, c(list(grid::rectGrob()), properties)),
      class = "lplot_error"
    )
  }
  expect_error(l_viewport(list(1)), class = "lplot_unsupported_source")
})
