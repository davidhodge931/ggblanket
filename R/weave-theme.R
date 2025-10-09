# set_blanket <- function(
#   #base
#   theme = NULL,
#   #geom
#   fill = ifelse(is_panel_dark(), ocean, blue),
#   shape = 21,
#   linewidth = 0.66,
#   size = 1.5,
#   stroke = 0.5,
#   #palette
#   fill_palette = NULL,
#   shape_palette = c(21, 24, 22, 23, 25),
#   linetype_palette = 1:6,
#   #border
#   border_geoms = NULL,
#   border_linewidth = 0.25,
#   border_pop = \(x) ifelse(is_panel_dark(), scales::col_lighter(x), scales::col_darker(x))
# ) {
#   #base
#   if (!rlang::is_null(theme)) {
#     ggplot2::set_theme(theme = theme)
#   }
#   #geom
#   ggplot2::update_theme(
#     geom = ggplot2::element_geom(
#       fill = fill,
#       colour = border_pop(fill),
#       shape = shape,
#       linewidth = linewidth,
#       borderwidth = linewidth,
#       size = size,
#       stroke = stroke,
#     )
#   )
#   #palette
#   if (rlang::is_null(fill_palette)) {
#     fill_palette_d <- scales::pal_hue()
#     fill_palette_c <- direction_contrast(scales::pal_viridis(option = "mako", begin = 0.1, end = 0.9))
#   }
#   else if (length(fill_palette) == 1) {
#     fill_palette_d <- unlist(fill_palette)
#     fill_palette_c <- direction_contrast(scales::pal_viridis(option = "mako", begin = 0.1, end = 0.9))
#   }
#   else if (length(fill_palette) == 2) {
#     fill_palette_d <- fill_palette[[1]]
#     fill_palette_c <- fill_palette[[2]]
#   }
#
#   colour_palette_d <- border_pop(fill_palette_d)
#   colour_palette_c <- border_pop(fill_palette_c)
#
#   ggplot2::update_theme(
#     palette.colour.discrete = fill_palette_d,
#     palette.fill.discrete = fill_palette_d,
#     palette.colour.continuous = fill_palette_c,
#     palette.fill.continuous = fill_palette_c,
#     palette.shape.discrete = shape_palette,
#     palette.shape.linetype = linetype_palette
#   )
#
#   #border
#   if (rlang::is_null(border_geoms)) {
#     border_geoms <- c("area", "bin2d", "bar", "boxplot", "col", "contour_filled",
#                       "crossbar", "density", "density2d_filled", "hex", "map",
#                       "polygon", "raster", "rect", "ribbon", "sf", "tile", "violin")
#   }
#
#   border_theme <- rlang::set_names(
#     lapply(border_geoms, \(geom) {
#       ggplot2::element_geom(linewidth = border_linewidth, borderwidth = border_linewidth)
#     }),
#     paste0("geom.", border_geoms)
#   )
#
#   ggplot2::update_theme(!!!border_theme)
# }
