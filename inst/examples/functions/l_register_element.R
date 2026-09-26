result <- lplot::l_register_element(
  "map_note",
  can_extract = is.character,
  extract = function(source, ...) {
    lplot::l_text(source, fontsize = 12, col = "#194E70")
  }
)

print(names(result))
