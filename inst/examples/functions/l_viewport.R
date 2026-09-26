title <- lplot::l_text("Area de estudo", fontsize = 18)
credits <- lplot::l_text("Fonte: dados locais", fontsize = 10)

result <- lplot::l_viewport(
  list(title, credits),
  padding = 24,
  flow = "column",
  gap = 16,
  background = "#EEF3CF"
)

lplot::l_render(result)
