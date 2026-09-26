label <- lplot::l_text("Conteudo convertido em grob", fontsize = 18)

result <- lplot::l_as_grob(label)

lplot::l_render(lplot::l_place(result, width = "100%", height = "100%"))
