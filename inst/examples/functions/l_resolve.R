rectangle <- lplot::l_rect(
  width = lplot::l_unit(40, "mm"),
  height = lplot::l_unit(20, "mm")
)

result <- lplot::l_resolve(rectangle, width = 600, height = 400)
print(result$root$box)
print(result$root$children[[1]]$box)
