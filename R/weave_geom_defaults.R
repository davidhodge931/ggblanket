#' Update a series of geom defaults
#'
#' @description Update a series of geom defaults for when these are unspecified
#'
#' @param col_pal A hex colour for the exterior outlines and interior fill for `gg_*(..., col = NULL, ...)`. Defaults to blue. Does not affect annotation type geoms (i.e. hline, vline, abline, curve, text and label).
#' @param annotate_pal A hex colour for annotation (e.g. `darkness[2]`)
#'
#' @return Globally updated geom defaults
#' @export
weave_geom_defaults <- function(col_pal = blue, annotate_pal = lightness[2]) {

  ggplot2::update_geom_defaults("hline", ggplot2::aes(colour = annotate_pal, linewidth = 0.33))
  ggplot2::update_geom_defaults("vline", ggplot2::aes(colour = annotate_pal, linewidth = 0.33))
  ggplot2::update_geom_defaults("abline", ggplot2::aes(colour = annotate_pal, linewidth = 0.33))
  ggplot2::update_geom_defaults("curve", ggplot2::aes(colour = annotate_pal, linewidth = 0.33))
  ggplot2::update_geom_defaults("text", ggplot2::aes(colour = annotate_pal))
  ggplot2::update_geom_defaults("label", ggplot2::aes(colour = annotate_pal, fill = annotate_pal, alpha = 0.05))

  ggplot2::update_geom_defaults("area", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9, linewidth = 0.66))
  ggplot2::update_geom_defaults("bar", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9, linewidth = 0.66))
  ggplot2::update_geom_defaults("boxplot", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9 * 0.67, linewidth = 0.66))
  ggplot2::update_geom_defaults("col", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9, linewidth = 0.66))
  ggplot2::update_geom_defaults("contour", ggplot2::aes(colour = col_pal, linewidth = 0.66))
  ggplot2::update_geom_defaults("contour_filled", ggplot2::aes(fill = col_pal, linewidth = 0.66))
  ggplot2::update_geom_defaults("crossbar", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9 * 0.67, linewidth = 0.66))
  ggplot2::update_geom_defaults("density", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9 * 0.67, linewidth = 0.66))
  ggplot2::update_geom_defaults("density2d", ggplot2::aes(colour = col_pal, linewidth = 0.66))
  ggplot2::update_geom_defaults("density_2d_filled", ggplot2::aes(fill = col_pal, linewidth = 0.66))
  ggplot2::update_geom_defaults("errorbar", ggplot2::aes(colour = col_pal, linewidth = 0.66))
  ggplot2::update_geom_defaults("function", ggplot2::aes(colour = col_pal, linewidth = 0.66))
  ggplot2::update_geom_defaults("hex", ggplot2::aes(fill = col_pal))
  ggplot2::update_geom_defaults("line", ggplot2::aes(colour = col_pal, linewidth = 0.66))
  ggplot2::update_geom_defaults("linerange", ggplot2::aes(colour = col_pal, linewidth = 0.66))
  ggplot2::update_geom_defaults("path", ggplot2::aes(colour = col_pal, linewidth = 0.66))
  ggplot2::update_geom_defaults("point", ggplot2::aes(colour = col_pal, fill = col_pal, size = 1.5))
  ggplot2::update_geom_defaults("pointrange", ggplot2::aes(colour = col_pal, fill = col_pal, linewidth = 0.66, size = 1.5 * 0.25))
  ggplot2::update_geom_defaults("polygon", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9, linewidth = 0.66))
  ggplot2::update_geom_defaults("quantile", ggplot2::aes(colour = col_pal, linewidth = 0.66))
  ggplot2::update_geom_defaults("raster", ggplot2::aes(fill = col_pal))
  ggplot2::update_geom_defaults("rect", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9, linewidth = 0.66))
  ggplot2::update_geom_defaults("ribbon", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9 * 0.67, linewidth = 0.66))
  ggplot2::update_geom_defaults("rug", ggplot2::aes(colour = col_pal, linewidth = 0.66))
  ggplot2::update_geom_defaults("segment", ggplot2::aes(colour = col_pal, linewidth = 0.66))
  ggplot2::update_geom_defaults("sf", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9, linewidth = 0.66))
  ggplot2::update_geom_defaults("smooth", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9 * 0.67, linewidth = 0.66))
  ggplot2::update_geom_defaults("spoke", ggplot2::aes(colour = col_pal, linewidth = 0.66))
  ggplot2::update_geom_defaults("step", ggplot2::aes(colour = col_pal, linewidth = 0.66))
  ggplot2::update_geom_defaults("violin", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9, linewidth = 0.66))
  #to add and adjust once ggplot makes GeomBin2d
  ggplot2::update_geom_defaults("tile", ggplot2::aes(colour = NA, fill = col_pal, alpha = 0.9, linewidth = 0.66))
}
