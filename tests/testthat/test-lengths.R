test_that("logical lengths resolve without mutation", {
  length <- l_length("50%")
  original <- length
  expect_equal(resolve_length(length, new_layout_context(800, 600)), 400)
  expect_equal(resolve_length(length, new_layout_context(200, 100)), 100)
  expect_identical(length, original)
  context <- new_layout_context(200, 100, root_width = 800, root_height = 600)
  expect_equal(resolve_length("10vw", context), 80)
  expect_equal(resolve_length("10vh", context), 60)
  expect_equal(resolve_length("10vmin", context), 60)
  expect_equal(resolve_length("10vmax", context), 80)
  expect_equal(resolve_length("50%", context, "y"), 50)
})

test_that("physical lengths use 96 logical pixels per inch independently of dpi", {
  for (dpi in c(72, 96, 300)) {
    context <- new_layout_context(800, 600, dpi)
    for (length in c("1in", "25.4mm", "2.54cm", "72pt", "96px")) {
      expect_equal(resolve_length(length, context), 96)
    }
  }
})

test_that("expressions are parsed safely and resolved recursively", {
  context <- new_layout_context(800, 600)
  expect_equal(resolve_length("clamp(120px, 35%, 360px)", context), 280)
  expect_equal(resolve_length("min(90%, max(20px, 600px))", context), 600)
  expect_equal(resolve_length(l_clamp("10px", "200%", "300px"), context), 300)
  expect_true(is.na(resolve_length("auto", context)))
  expect_equal(resolve_length("auto", context, intrinsic = 42), 42)
  for (invalid in c(
    "12em",
    "12",
    "min()",
    "clamp(1px, 2px)",
    "min(auto, 2px)",
    "min(2px,,3px)",
    "system('echo no')",
    "min(2px, max(3px, 4px)"
  )) {
    expect_error(l_length(invalid), class = "lplot_error")
  }
  expect_error(
    resolve_length("clamp(10px, 2px, 1px)", context),
    class = "lplot_error"
  )
  expect_error(l_length(Inf), class = "lplot_error")
  expect_error(new_layout_context(-1, 20), class = "lplot_error")
})
