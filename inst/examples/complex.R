examples_root <- system.file("examples", package = "lplot")
sys.source(file.path(examples_root, "data", "maps.R"), envir = environment())
sys.source(file.path(examples_root, "utils.R"), envir = environment())
rm(examples_root)

map_complex_scene <- function() {
  counties <- map_counties()
  focus <- sf::st_bbox(
    c(xmin = 580000, ymin = 130000, xmax = 820000, ymax = 290000),
    crs = sf::st_crs(counties)
  )
  selected <- lengths(sf::st_intersects(counties, sf::st_as_sfc(focus))) > 0
  region <- sf::st_drop_geometry(counties[selected, ])
  ranking <- head(region[order(-region$BIR74), ], 5)
  ranking$NAME <- factor(ranking$NAME, levels = rev(ranking$NAME))
  chart_mapping <- do.call(
    ggplot2::aes,
    list(x = quote(BIR74), y = quote(NAME))
  )
  chart <- ggplot2::ggplot(ranking, chart_mapping) +
    ggplot2::geom_col(fill = "#197C80", width = 0.65) +
    ggplot2::scale_x_continuous(
      labels = function(values) paste(values / 1000, "mil")
    ) +
    ggplot2::labs(
      title = "Maiores totais",
      subtitle = "Condados que intersectam o recorte",
      x = "Nascimentos em 1974",
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 9) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", colour = "#203C43"),
      plot.subtitle = ggplot2::element_text(size = 8),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(4, 8, 4, 0)
    )
  plot <- map_source(counties, focus)
  frame <- map_frame(
    plot,
    focus,
    distance_m = 50000,
    overlays = list(
      map_locator(counties, focus),
      lplot::l_place(map_north_arrow(focus), right = 12, top = 12, z_index = 30)
    )
  )
  map <- lplot::l_viewport(list(
    lplot::l_place(
      lplot::l_viewport(list(frame)),
      left = 0,
      right = 0,
      top = 0,
      bottom = 68,
      id = "frame"
    ),
    lplot::l_place(
      lplot::l_get_element(plot, "legend"),
      x = "50%",
      bottom = 0,
      anchor = "bottom-center",
      id = "legend"
    )
  ))
  indicators <- lplot::l_viewport(list(
    lplot::l_place(
      map_label_template(
        format(
          sum(region$BIR74),
          big.mark = ".",
          decimal.mark = ",",
          scientific = FALSE,
          trim = TRUE
        ),
        "Nascimentos nos condados do recorte"
      ),
      left = 0,
      right = 0,
      top = 0,
      height = 64,
      id = "births"
    ),
    lplot::l_place(
      map_label_template(
        as.character(nrow(region)),
        "Condados que intersectam o recorte",
        accent = "#A63748",
        background = "#FAF1F3"
      ),
      left = 0,
      right = 0,
      top = 80,
      height = 64,
      id = "counties"
    ),
    lplot::l_place(
      chart,
      left = 0,
      right = 0,
      top = 168,
      bottom = 0,
      id = "ranking"
    )
  ))
  lplot::l_viewport(
    list(
      lplot::l_place(
        map_label_template(
          "Nascimentos | recorte regional",
          "Carolina do Norte, 1974 | Regiao central e leste"
        ),
        left = 0,
        right = 0,
        top = 0,
        height = 76,
        id = "header"
      ),
      lplot::l_place(
        map,
        left = 0,
        width = "66%",
        top = 100,
        bottom = 68,
        id = "map"
      ),
      lplot::l_place(
        indicators,
        left = "69%",
        right = 0,
        top = 100,
        bottom = 68,
        id = "indicators"
      ),
      lplot::l_place(
        lplot::l_text(
          paste(
            "Fonte: sf::nc | NAD83 / North Carolina (m) | Escala em distancia projetada",
            "Indicadores incluem o total de cada condado que intersecta o recorte.",
            sep = "\n"
          ),
          fontsize = 8,
          col = "#50666C"
        ),
        left = 0,
        bottom = 0,
        id = "credits"
      )
    ),
    width = 1200,
    height = 800,
    padding = 24,
    background = "white",
    metadata = list(focus = focus, counties = region$NAME)
  )
}
