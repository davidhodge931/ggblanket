# Fixed set_blanket function - colour inherits from fill (no transformation in theme)
set_blanket <- function(
    #theme
  theme = theme_lighter(),
  polish = polish_modern,
  #geom
  fill = flexoki::flexoki$blue["blue400"],
  shape = 21, #NEED THIS
  ###
  linewidth = 0.66,
  borderwidth = 0.25, # maybe bordered_linewidth
  stroke = 0.5,
  ###
  size = 1.5,
  #scales
  fill_palette_discrete = scales::pal_hue(),
  fill_palette_continuous = scales::pal_gradient_n(direction_contrast(viridis::mako(n = 256))),
  shape_palette_discrete = scales::pal_manual(c(21, 24, 22, 23, 25)),
  linetype_palette_discrete = scales::pal_manual(1:6),
  #geom and scales
  bordered_colour = NULL
) {
  #base
  base_theme <- if (!rlang::is_null(theme)) theme else ggplot2::theme_get()

  # Set default bordered_colour if not provided
  if (rlang::is_null(bordered_colour)) {
    bordered_colour <- \(x) if (is_panel_dark()) blend_screen(x) else blend_multiply(x)
  }

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
        geom.pointrange = ggplot2::element_geom(borderwidth = stroke),
        geom.dotplot = ggplot2::element_geom(borderwidth = stroke),
        palette.colour.discrete = fill_palette_discrete,
        palette.fill.discrete = fill_palette_discrete,
        palette.colour.continuous = fill_palette_continuous,
        palette.fill.continuous = fill_palette_continuous,
        palette.shape.discrete = shape_palette_discrete,
        palette.linetype.discrete = linetype_palette_discrete
      )
  )

  #options
  set_polish(polish = polish)
  set_bordered_colour(bordered_colour = bordered_colour)
}
