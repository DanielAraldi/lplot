map_extent <- function(counties) {
  bounds <- sf::st_bbox(counties)
  padding <- c(
    diff(bounds[c("xmin", "xmax")]),
    diff(bounds[c("ymin", "ymax")])
  ) *
    0.04
  bounds[c("xmin", "ymin")] <- bounds[c("xmin", "ymin")] - padding
  bounds[c("xmax", "ymax")] <- bounds[c("xmax", "ymax")] + padding
  bounds
}

map_label_template <- function(
  title,
  subtitle,
  accent = "#197C80",
  background = "#F0F6F5"
) {
  lplot::l_template(
    lplot::l_rect(fill = background, col = NA, name = "background"),
    lplot::l_rect(
      x = 0,
      just = "left",
      width = grid::unit(4, "pt"),
      fill = accent,
      col = NA,
      name = "accent"
    ),
    lplot::l_text(
      title,
      x = grid::unit(14, "pt"),
      y = 0.65,
      just = "left",
      fontsize = 12,
      fontface = "bold",
      col = accent,
      name = "title"
    ),
    lplot::l_text(
      subtitle,
      x = grid::unit(14, "pt"),
      y = 0.28,
      just = "left",
      fontsize = 8,
      col = "#50666C",
      name = "subtitle"
    )
  )
}

map_source <- function(
  counties,
  extent,
  field = "BIR74",
  title = "Carolina do Norte"
) {
  counties$births <- counties[[field]]
  mapping <- do.call(ggplot2::aes, list(fill = quote(births)))
  ggplot2::ggplot(counties) +
    ggplot2::geom_sf(mapping, colour = "#FFFFFF", linewidth = 0.3) +
    ggplot2::scale_fill_gradientn(
      colours = c("#EEF3CF", "#95CEC0", "#2B8E98", "#194E70"),
      limits = c(
        0,
        ceiling(max(c(counties$BIR74, counties$BIR79)) / 10000) * 10000
      ),
      breaks = c(0, 20000, 40000),
      labels = c("0", "20 mil", "40 mil"),
      name = "Nascimentos",
      guide = ggplot2::guide_colourbar(
        direction = "horizontal",
        title.position = "top",
        barwidth = grid::unit(42, "mm"),
        barheight = grid::unit(2.5, "mm")
      )
    ) +
    ggplot2::coord_sf(
      crs = sf::st_crs(counties),
      default_crs = sf::st_crs(counties),
      datum = NA,
      xlim = as.numeric(extent[c("xmin", "xmax")]),
      ylim = as.numeric(extent[c("ymin", "ymax")]),
      expand = FALSE
    ) +
    ggplot2::labs(
      title = title,
      subtitle = paste("Nascimentos por condado |", field)
    ) +
    ggplot2::theme_void(base_size = 10) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", colour = "#203C43"),
      plot.subtitle = ggplot2::element_text(colour = "#50666C"),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 8),
      legend.title = ggplot2::element_text(size = 9),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      panel.background = ggplot2::element_rect(fill = "#EDF3F5", colour = NA),
      plot.margin = ggplot2::margin(0, 0, 0, 0)
    )
}

map_scale_bar <- function(distance_m, extent) {
  width_m <- as.numeric(extent[["xmax"]] - extent[["xmin"]])
  scale <- lplot::l_template(
    lplot::l_rect(
      x = 0.25,
      y = 0.3,
      width = 0.5,
      height = 0.25,
      fill = "#203C43",
      col = "#203C43"
    ),
    lplot::l_rect(
      x = 0.75,
      y = 0.3,
      width = 0.5,
      height = 0.25,
      fill = "white",
      col = "#203C43"
    ),
    lplot::l_text(
      "0",
      x = 0,
      y = 0.8,
      just = "left",
      fontsize = 8
    ),
    lplot::l_text(
      paste(distance_m / 1000, "km"),
      x = 1,
      y = 0.8,
      just = "right",
      fontsize = 8
    )
  )
  lplot::l_get_element(
    scale,
    "scale_bar",
    width = lplot::l_length(100 * distance_m / width_m, "%"),
    height = 30,
    metadata = list(distance_m = distance_m, extent_width_m = width_m)
  )
}

map_frame <- function(plot, extent, distance_m = 200000, overlays = list()) {
  ratio <- as.numeric(
    (extent[["xmax"]] - extent[["xmin"]]) /
      (extent[["ymax"]] - extent[["ymin"]])
  )
  panel <- lplot::l_get_element(plot, "panel")
  frame <- lplot::l_viewport(
    c(
      list(
        lplot::l_place(panel, width = "100%", height = "100%"),
        lplot::l_place(
          map_scale_bar(distance_m, extent),
          left = 12,
          bottom = 8,
          z_index = 10
        )
      ),
      overlays
    ),
    metadata = list(extent = extent)
  )
  lplot::l_place(
    frame,
    x = "50%",
    y = "50%",
    anchor = "center",
    width = "100%",
    max_height = "100%",
    aspect_ratio = ratio
  )
}

map_sheet <- function(plot, frame, heading = "Carolina do Norte") {
  title <- lplot::l_get_element(
    plot,
    "title",
    style = list(font_size = "clamp(12pt, 2.5vmin, 20pt)")
  )
  subtitle <- lplot::l_get_element(
    plot,
    "subtitle",
    style = list(font_size = "clamp(8pt, 1.4vmin, 10pt)")
  )
  legend <- lplot::l_get_element(plot, "legend")
  credit <- lplot::l_get_element(
    lplot::l_text(
      "Fonte: NAD83 / NC (m)\nElaboração: Daniel Sansão Araldi",
      fontsize = 7,
      col = "#50666C"
    ),
    "credits"
  )
  lplot::l_viewport(
    list(
      lplot::l_place(title, left = 0, top = 0, z_index = 20),
      lplot::l_place(subtitle, left = 0, top = 34, z_index = 20),
      lplot::l_place(
        lplot::l_viewport(list(frame)),
        left = 0,
        right = 0,
        top = 68,
        bottom = 98
      ),
      lplot::l_place(legend, x = "50%", bottom = 28, anchor = "bottom-center"),
      lplot::l_place(credit, x = "50%", bottom = 4, anchor = "bottom-center")
    ),
    width = 1000,
    height = 650,
    padding = 16,
    background = "#FFFFFF",
    metadata = list(heading = heading)
  )
}

map_north_arrow <- function(extent) {
  center <- sf::st_sfc(
    sf::st_point(c(
      mean(as.numeric(extent[c("xmin", "xmax")])),
      mean(as.numeric(extent[c("ymin", "ymax")]))
    )),
    crs = sf::st_crs(extent)
  )
  lonlat <- sf::st_coordinates(sf::st_transform(center, 4326))[1, ]
  north <- sf::st_sfc(sf::st_point(unname(lonlat + c(0, 0.05))), crs = 4326)
  direction <- as.numeric(
    sf::st_coordinates(sf::st_transform(north, sf::st_crs(extent))) -
      sf::st_coordinates(center)
  )
  angle <- atan2(direction[[2]], direction[[1]]) * 180 / pi - 90
  arrow <- lplot::l_template(
    grid::segmentsGrob(
      x0 = 0.5,
      x1 = 0.5,
      y0 = 0.14,
      y1 = 0.72,
      arrow = grid::arrow(length = grid::unit(3, "mm"), type = "closed"),
      gp = grid::gpar(col = "#203C43", fill = "#203C43", lwd = 1.5)
    ),
    lplot::l_text(
      "N",
      x = 0.5,
      y = 0.92,
      fontsize = 11,
      fontface = "bold"
    ),
    vp = grid::viewport(angle = angle)
  )
  lplot::l_get_element(
    arrow,
    "north_arrow",
    width = 40,
    height = 68,
    metadata = list(angle = angle, reference = lonlat)
  )
}

map_locator <- function(counties, focus) {
  extent <- map_extent(counties)
  highlight <- sf::st_as_sfc(focus)
  plot <- ggplot2::ggplot(counties) +
    ggplot2::geom_sf(fill = "#D7E2E3", colour = "white", linewidth = 0.15) +
    ggplot2::geom_sf(
      data = highlight,
      fill = "#D84B3933",
      colour = "#C63F30",
      linewidth = 0.8
    ) +
    ggplot2::coord_sf(
      crs = sf::st_crs(counties),
      datum = NA,
      expand = FALSE,
      xlim = as.numeric(extent[c("xmin", "xmax")]),
      ylim = as.numeric(extent[c("ymin", "ymax")])
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.border = ggplot2::element_rect(
        fill = NA,
        colour = "#718A90",
        linewidth = 0.4
      )
    )
  ratio <- as.numeric(
    (extent[["xmax"]] - extent[["xmin"]]) /
      (extent[["ymax"]] - extent[["ymin"]])
  )
  panel <- lplot::l_get_element(plot, "panel")
  lplot::l_place(
    lplot::l_viewport(
      list(panel),
      background = "white",
      metadata = list(focus = focus, extent = extent)
    ),
    left = 10,
    top = 10,
    width = "34%",
    aspect_ratio = ratio,
    z_index = 30
  )
}
