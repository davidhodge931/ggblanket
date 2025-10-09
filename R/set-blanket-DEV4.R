# Fixed set_blanket function - colour inherits from fill by default
set_blanket <- function(
    #base
  theme = NULL,
  #geom
  fill = "steelblue",
  # fill = ifelse(is_panel_dark(), ocean, blue),
  shape = 21,
  linewidth = 0.66,
  borderwidth = 0.25,
  size = 1.5,
  stroke = 0.5,
  #palette
  fill_palette = NULL,
  shape_palette = scales::pal_manual(c(21, 24, 22, 23, 25)),
  linetype_palette = scales::pal_manual(1:6)
) {

  #base
  base_theme <- if (!rlang::is_null(theme)) theme else ggplot2::theme_get()

  #palette
  print(length(fill_palette))

  if (rlang::is_null(fill_palette)) {
    fill_palette_d <- scales::pal_hue()
    fill_palette_c <- direction_contrast(scales::pal_viridis(option = "mako", begin = 0.1, end = 0.9))
  }
  else if (purrr::is_list(fill_palette)) {
    if (length(fill_palette) == 1) {
      fill_palette_d <- unlist(fill_palette)
      fill_palette_c <- direction_contrast(scales::pal_viridis(option = "mako", begin = 0.1, end = 0.9))
    }
    else if (length(fill_palette) == 2) {
      fill_palette_d <- fill_palette[[1]]
      fill_palette_c <- fill_palette[[2]]
    }
  }
  else if (purrr::is_vector(fill_palette)) {
    fill_palette_d <- unlist(fill_palette)
    fill_palette_c <- direction_contrast(scales::pal_viridis(option = "mako", begin = 0.1, end = 0.9))
  }

  # Colour palette inherits from fill palette with no transformation
  colour_palette_d <- fill_palette_d
  colour_palette_c <- fill_palette_c

  #set theme
  ggplot2::set_theme(
    base_theme +
      ggplot2::theme(
        geom = ggplot2::element_geom(
          fill = fill,
          colour = fill,
          pointshape = shape,
          linewidth = linewidth,
          borderwidth = borderwidth,
          pointsize = size,
        ),
        geom.point = ggplot2::element_geom(borderwidth = stroke),
        geom.jitter = ggplot2::element_geom(borderwidth = stroke),
        geom.count = ggplot2::element_geom(borderwidth = stroke),
        geom.qq = ggplot2::element_geom(borderwidth = stroke),
        geom.dotplot = ggplot2::element_geom(borderwidth = stroke),
        geom.pointrange = ggplot2::element_geom(borderwidth = stroke),
        palette.colour.discrete = colour_palette_d,
        palette.fill.discrete = fill_palette_d,
        palette.colour.continuous = colour_palette_c,
        palette.fill.continuous = fill_palette_c,
        palette.shape.discrete = shape_palette,
        palette.linetype.discrete = linetype_palette
      )
  )
}
