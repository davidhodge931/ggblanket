#' Set the mode and geom_defaults
#'
#' @param mode A `*_mode_*` theme set globally for when mode = NULL. E.g. [light_mode_t()], [grey_mode_r()], or [dark_mode_r()].
#' @param base_colour
#' @param base_fill
#' @param base_alpha
#' @param base_colour
#' @param base_linewidth
#' @param base_size
#' @param annotation_colour
#' @param annotation_linewidth
#' @param annotation_linewidth
#'
#' @return A globally set mode.
#' @export
set_gg <- function(
  mode = light_mode_r(),
  base_colour = blue,
  base_fill = blue,
  base_alpha = 0.9,
  base_linewidth = 0.66,
  base_size = 1.5,
  annotation_colour = "#121B24",
  annotation_linewidth = 0.33
  ) {

  set_mode(mode)

  ggplot2::update_geom_defaults("hline", ggplot2::aes(colour = annotation_colour, linewidth = annotation_linewidth))
  ggplot2::update_geom_defaults("vline", ggplot2::aes(colour = annotation_colour, linewidth = annotation_linewidth))
  ggplot2::update_geom_defaults("abline", ggplot2::aes(colour = annotation_colour, linewidth = annotation_colour))
  ggplot2::update_geom_defaults("curve", ggplot2::aes(colour = annotation_colour, linewidth = annotation_linewidth))
  ggplot2::update_geom_defaults("text", ggplot2::aes(colour = annotation_colour))
  ggplot2::update_geom_defaults("label", ggplot2::aes(colour = annotation_colour, fill = annotation_colour, alpha = 0.1))

  ggplot2::update_geom_defaults("area", ggplot2::aes(colour = base_colour, fill = base_fill, alpha = base_alpha, linewidth = base_linewidth))
  ggplot2::update_geom_defaults("bar", ggplot2::aes(colour = base_colour, fill = base_fill, alpha = base_alpha, linewidth = base_linewidth))
  ggplot2::update_geom_defaults("boxplot", ggplot2::aes(colour = base_colour, fill = base_fill, alpha = base_alpha * 0.5, linewidth = base_linewidth))
  ggplot2::update_geom_defaults("col", ggplot2::aes(colour = base_colour, fill = base_fill, alpha = base_alpha, linewidth = base_linewidth))
  ggplot2::update_geom_defaults("contour", ggplot2::aes(colour = blue, linewidth = base_linewidth))
  ggplot2::update_geom_defaults("contour_filled", ggplot2::aes(fill = blue, linewidth = base_linewidth))
  ggplot2::update_geom_defaults("crossbar", ggplot2::aes(colour = base_colour, fill = base_fill, alpha = base_alpha * 0.5, linewidth = base_linewidth))
  ggplot2::update_geom_defaults("density", ggplot2::aes(colour = base_colour, fill = base_fill, alpha = base_alpha * 0.5, linewidth = base_linewidth))
  ggplot2::update_geom_defaults("density2d", ggplot2::aes(colour = blue, linewidth = base_linewidth))
  ggplot2::update_geom_defaults("density_2d_filled", ggplot2::aes(fill = blue, linewidth = base_linewidth))
  ggplot2::update_geom_defaults("errorbar", ggplot2::aes(colour = blue, width = 0.2, linewidth = base_linewidth))
  ggplot2::update_geom_defaults("function", ggplot2::aes(colour = blue, linewidth = base_linewidth))
  ggplot2::update_geom_defaults("hex", ggplot2::aes(fill = blue))
  ggplot2::update_geom_defaults("line", ggplot2::aes(colour = blue, linewidth = base_linewidth))
  ggplot2::update_geom_defaults("linerange", ggplot2::aes(colour = blue, linewidth = base_linewidth))
  ggplot2::update_geom_defaults("path", ggplot2::aes(colour = blue, linewidth = base_linewidth))
  ggplot2::update_geom_defaults("point", ggplot2::aes(colour = blue, fill = blue, size = base_size))
  ggplot2::update_geom_defaults("pointrange", ggplot2::aes(colour = base_colour, fill = base_fill, linewidth = base_linewidth, size = base_size * 0.25))
  ggplot2::update_geom_defaults("polygon", ggplot2::aes(colour = base_colour, fill = base_fill, alpha = base_alpha, linewidth = base_linewidth))
  ggplot2::update_geom_defaults("quantile", ggplot2::aes(colour = blue, linewidth = base_linewidth))
  ggplot2::update_geom_defaults("raster", ggplot2::aes(fill = blue))
  ggplot2::update_geom_defaults("rect", ggplot2::aes(colour = base_colour, fill = base_fill, alpha = base_alpha, linewidth = base_linewidth))
  ggplot2::update_geom_defaults("ribbon", ggplot2::aes(colour = base_colour, fill = base_fill, alpha = base_alpha * 0.5, linewidth = base_linewidth))
  ggplot2::update_geom_defaults("rug", ggplot2::aes(colour = blue, linewidth = base_linewidth))
  ggplot2::update_geom_defaults("segment", ggplot2::aes(colour = blue, linewidth = base_linewidth))
  ggplot2::update_geom_defaults("sf", ggplot2::aes(colour = base_colour, fill = base_fill, alpha = base_alpha, linewidth = base_linewidth))
  ggplot2::update_geom_defaults("smooth", ggplot2::aes(colour = base_colour, fill = base_fill, alpha = base_alpha * 0.5, linewidth = base_linewidth))
  ggplot2::update_geom_defaults("spoke", ggplot2::aes(colour = blue, linewidth = base_linewidth))
  ggplot2::update_geom_defaults("step", ggplot2::aes(colour = blue, linewidth = base_linewidth))
  ggplot2::update_geom_defaults("violin", ggplot2::aes(colour = base_colour, fill = base_fill, alpha = base_alpha, linewidth = base_linewidth))
  #to add and adjust once ggplot makes GeomBin2d
  ggplot2::update_geom_defaults("tile", ggplot2::aes(colour = NA, fill = blue, alpha = base_alpha, linewidth = base_linewidth))
}

