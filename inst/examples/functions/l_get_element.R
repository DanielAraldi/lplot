plot <- ggplot2::ggplot(
  datasets::mtcars,
  ggplot2::aes(wt, mpg, colour = factor(cyl))
) +
  ggplot2::geom_point() +
  ggplot2::labs(title = "Consumo dos veiculos", colour = "Cilindros") +
  ggplot2::theme_minimal()

result <- lplot::l_get_element(plot, "legend")

lplot::l_render(result)
