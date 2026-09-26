first <- lplot::l_rect(
  width = lplot::l_unit(55, "mm"), height = lplot::l_unit(35, "mm"),
  fill = "#95CEC0", col = NA
)
second <- lplot::l_rect(
  width = lplot::l_unit(55, "mm"), height = lplot::l_unit(35, "mm"),
  fill = "#194E70", col = NA
)

result <- lplot::l_join(
  list(first, second),
  flow = "row",
  gap = 16,
  padding = 16,
  background = "white"
)

lplot::l_render(result)
