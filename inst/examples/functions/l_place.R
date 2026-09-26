rectangle <- lplot::l_rect(fill = "#95CEC0", col = "#194E70")

result <- lplot::l_place(
  rectangle,
  left = "10%",
  top = "15%",
  width = "50%",
  height = 80,
  id = "positioned_rectangle"
)

lplot::l_render(result)
