# Fixed set_blanket function - colour inherits from fill by default
set_blanket <- function(
    #base
  theme = NULL,
  #geom
  fill = ifelse(is_panel_dark(), ocean, blue),
  shape = 21,
  linewidth = 0.66,
  size = 1.5,
  stroke = 0.5,
  #palette
  fill_palette = NULL,
  shape_palette = scales::pal_manual(c(21, 24, 22, 23, 25)),
  linetype_palette = scales::pal_manual(1:6),
  #border
  border_geoms = NULL,
  border_linewidth = 0.25
) {

  #base
  base_theme <- if (!rlang::is_null(theme)) theme else ggplot2::theme_get()

  #geom - get all geom names
  all_geoms <- ls(pattern = "^Geom", envir = asNamespace("ggplot2"))
  all_geoms <- tolower(sub("^Geom", "", all_geoms))
  all_geoms <- all_geoms[all_geoms != ""]  # Remove empty string

  #border
  if (rlang::is_null(border_geoms)) {
    # Geoms with filled areas that benefit from thinner borders
    border_polygons <- c(
      "area", "bar", "bin2d", "boxplot", "col", "contour_filled",
      "crossbar", "density", "density2d", "density2d_filled", "dotplot",
      "hex", "map", "polygon", "raster", "rect", "ribbon", "sf",
      "tile", "violin"
    )

    border_points <- if (shape %in% 21:25) {
      c("point", "jitter", "count", "qq", "pointrange", "dotplot")
    } else {
      character(0)
    }

    border_geoms <- c(border_polygons, border_points)
  }

  # Apply geom defaults to all geoms, with conditional linewidth
  # Colour now inherits from fill with no transformation
  geom_theme <- rlang::set_names(
    lapply(all_geoms, \(geom) {
      # Use border_linewidth for border geoms, otherwise use default linewidth
      lw <- if (geom %in% border_geoms) border_linewidth else linewidth

      ggplot2::element_geom(
        fill = fill,
        colour = fill,  # Inherit from fill with no transformation
        shape = shape,
        linewidth = lw,
        borderwidth = lw,
        size = size,
        stroke = stroke,
      )
    }),
    paste0("geom.", all_geoms)
  )

  #palette
  if (rlang::is_null(fill_palette)) {
    fill_palette_d <- scales::pal_hue()
    fill_palette_c <- direction_contrast(scales::pal_viridis(option = "mako", begin = 0.1, end = 0.9))
  }
  else if (length(fill_palette) == 1) {
    fill_palette_d <- unlist(fill_palette)
    fill_palette_c <- direction_contrast(scales::pal_viridis(option = "mako", begin = 0.1, end = 0.9))
  }
  else if (length(fill_palette) == 2) {
    fill_palette_d <- fill_palette[[1]]
    fill_palette_c <- fill_palette[[2]]
  }

  # Colour palette inherits from fill palette with no transformation
  colour_palette_d <- fill_palette_d
  colour_palette_c <- fill_palette_c

  #set theme
  ggplot2::set_theme(
    base_theme +
      ggplot2::theme(
        palette.colour.discrete = colour_palette_d,
        palette.fill.discrete = fill_palette_d,
        palette.colour.continuous = colour_palette_c,
        palette.fill.continuous = fill_palette_c,
        palette.shape.discrete = shape_palette,
        palette.linetype.discrete = linetype_palette,
        !!!geom_theme
      )
  )

  invisible(NULL)
}
