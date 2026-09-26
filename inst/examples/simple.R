map_simple_scene <- function(
  counties = sf::st_read(
    system.file("shape/nc.shp", package = "sf"),
    quiet = TRUE
  ),
  field = "BIR74",
  title = "Nascimentos por condado",
  legend_title = "Nascimentos"
) {
  counties$value <- counties[[field]]

  plot <- ggplot2::ggplot(counties) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = value),
      colour = "white", linewidth = 0.3
    ) +
    ggplot2::scale_fill_gradient(
      low = "#EEF3CF", high = "#194E70", name = legend_title
    ) +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::labs(title = title) +
    ggplot2::theme_void(base_size = 10) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", colour = "#203C43"),
      legend.position = "bottom"
    )

  title <- lplot::l_get_element(plot, "title")
  legend <- lplot::l_get_element(plot, "legend")
  map <- plot +
    ggplot2::labs(title = NULL) +
    ggplot2::theme(legend.position = "none")

  lplot::l_viewport(
    list(
      lplot::l_place(
        title,
        left = 0,
        top = 0,
        id = "title"
      ),
      lplot::l_place(
        map,
        left = 0,
        right = 0,
        top = 52,
        bottom = 64,
        id = "map"
      ),
      lplot::l_place(
        legend,
        x = "50%",
        bottom = 0,
        anchor = "bottom-center",
        id = "legend"
      )
    ),
    width = 900,
    height = 600,
    padding = 16,
    background = "white",
    metadata = list(field = field)
  )
}
