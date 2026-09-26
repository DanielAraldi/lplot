original <- lplot::l_text("Titulo do mapa", fontsize = 10)

result <- lplot::l_style(
  original,
  color = "#194E70",
  font_size = "20pt",
  font_face = "bold",
  background = "#EEF3CF",
  padding = 12
)

lplot::l_render(result)
