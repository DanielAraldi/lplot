label <- lplot::l_text("Titulo exportado", fontsize = 18)
directory <- tempfile("lplot-save-")

result <- lplot::l_save(
  label,
  type = "svg",
  dir = directory,
  filename = "title",
  width = 600,
  height = 400
)

print(result)
