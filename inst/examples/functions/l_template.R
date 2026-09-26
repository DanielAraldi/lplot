result <- lplot::l_template(
  lplot::l_rect(
    width = 0.6, height = 0.3,
    fill = "#EEF3CF", col = "#194E70",
    name = "background"
  ),
  lplot::l_text("Area de estudo", name = "title"),
  gp = list(col = "#194E70", fontsize = 18, fontface = "bold"),
  name = "map_label"
)

lplot::l_render(lplot::l_place(result, width = "100%", height = "100%"))
