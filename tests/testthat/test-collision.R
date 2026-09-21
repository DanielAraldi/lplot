test_that("candidate order is deterministic and independent of drawing order", {
  scene <- collision_scene()
  original <- scene
  first <- l_resolve(scene, width = 200, height = 120)
  second <- l_resolve(scene, width = 200, height = 120)
  moved <- first$root$children[[2]]
  expect_equal(moved$box, c(x = 140, y = 45, width = 60, height = 30))
  expect_equal(
    moved$collision_candidates,
    second$root$children[[2]]$collision_candidates
  )
  expect_false(boxes_overlap(first$root$children[[1]]$box, moved$box))
  expect_identical(scene, original)
  scene$children[[2]] <- l_place(scene$children[[2]], z_index = -100)
  expect_equal(
    l_resolve(scene, width = 200, height = 120)$root$children[[2]]$box,
    moved$box
  )
})

test_that("fixed placement wins unless displacement is explicitly enabled", {
  scene <- collision_scene()
  scene$children[[2]] <- l_place(scene$children[[2]], top = 0, right = 0)
  expect_warning(
    layout <- l_resolve(scene, width = 200, height = 120),
    class = "lplot_collision"
  )
  expect_equal(layout$root$children[[2]]$box[["y"]], 0)
  scene$children[[2]] <- l_place(scene$children[[2]], allow_move = TRUE)
  expect_equal(
    l_resolve(scene, width = 200, height = 120)$root$children[[2]]$box[["y"]],
    45
  )
  scene <- collision_scene("none")
  expect_no_warning(l_resolve(scene, width = 200, height = 120))
})

test_that("higher priority resists displacement and plot backgrounds are not obstacles", {
  node <- l_place(
    grid::rectGrob(),
    width = 40,
    height = 20,
    collision = "avoid",
    anchor = "top-right"
  )
  scene <- l_viewport(list(example_plot(), node, l_place(node, priority = 20)))
  layout <- l_resolve(scene, 400, 300)
  expect_equal(layout$root$children[[3]]$box[["y"]], 0)
  expect_gt(layout$root$children[[2]]$box[["y"]], 0)
})

test_that("shrink requires responsive permission and respects minimum dimensions", {
  node <- l_place(
    grid::rectGrob(),
    width = 180,
    height = 60,
    collision = "shrink",
    min_width = 80,
    min_height = 20
  )
  scene <- l_viewport(list(node), safe_area = 10)
  layout <- l_resolve(scene, 120, 100)
  result <- layout$root$children[[1]]
  expect_lte(result$box[["width"]], 100)
  expect_gte(result$box[["width"]], 80)
  expect_lt(result$scale, 1)
  node <- l_place(node, responsive = FALSE)
  conditions <- list()
  unchanged <- withCallingHandlers(
    l_resolve(l_viewport(list(node)), 120, 100),
    warning = function(condition) {
      conditions[[length(conditions) + 1L]] <<- class(condition)
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(vapply(
    conditions,
    function(classes) "lplot_collision" %in% classes,
    logical(1)
  )))
  expect_equal(unchanged$root$children[[1]]$box[["width"]], 180)
  expect_equal(unchanged$root$children[[1]]$scale, 1)
})
