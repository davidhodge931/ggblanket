#' #' Setup the ggblanket style
#' #'
#' #' @description A set-up function, which sets the mode and updates ggplot2 geom defaults.
#' #'
#' #' @param mode A `*_mode_*` theme set globally for when mode = NULL. E.g. [light_mode_t()], [grey_mode_r()], or [dark_mode_r()].
#' #' @param geom_defaults A list of ggplot2::update_geom_defaults. E.g. `geom_defaults_light` or `geom_defaults_dark`.
#' #' @param ... Provided only to support trailing commas.
#' #'
#' #' @return A globally set mode.
#' #' @export
#' blankify <- function(
#'   mode = light_mode_r(),
#'   geom_defaults = geom_defaults_light(),
#'   ...
#'   ) {
#'
#'   set_mode(mode)
#'
#'   geom_defaults
#' }
#'
#' #' Updated geom defaults for a light (or grey) mode
#' #'
#' #' @param col_pal A hex colour for the exterior outlines and interior fill for `gg_*(..., col = NULL, ...)`. Defaults to blue. Does not affect annotation type geoms (i.e. hline, vline, abline, curve, text and label).
#' #'
#' #' @return Updated geom defaults
#' #' @export
#' geom_defaults_light <- function(col_pal = blue) {
#'
#'   ggplot2::update_geom_defaults("hline", ggplot2::aes(colour = "#121B24", linewidth = 0.33))
#'   ggplot2::update_geom_defaults("vline", ggplot2::aes(colour = "#121B24", linewidth = 0.33))
#'   ggplot2::update_geom_defaults("abline", ggplot2::aes(colour = "#121B24", linewidth = 0.33))
#'   ggplot2::update_geom_defaults("curve", ggplot2::aes(colour = "#121B24", linewidth = 0.33))
#'   ggplot2::update_geom_defaults("text", ggplot2::aes(colour = "#121B24"))
#'   ggplot2::update_geom_defaults("label", ggplot2::aes(colour = "#121B24", fill = "#121B24", alpha = 0.05))
#'
#'   ggplot2::update_geom_defaults("area", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("bar", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("boxplot", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9 * 0.67, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("col", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("contour", ggplot2::aes(colour = col_pal, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("contour_filled", ggplot2::aes(fill = col_pal, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("crossbar", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9 * 0.67, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("density", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9 * 0.67, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("density2d", ggplot2::aes(colour = col_pal, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("density_2d_filled", ggplot2::aes(fill = col_pal, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("errorbar", ggplot2::aes(colour = col_pal, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("function", ggplot2::aes(colour = col_pal, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("hex", ggplot2::aes(fill = col_pal))
#'   ggplot2::update_geom_defaults("line", ggplot2::aes(colour = col_pal, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("linerange", ggplot2::aes(colour = col_pal, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("path", ggplot2::aes(colour = col_pal, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("point", ggplot2::aes(colour = col_pal, fill = col_pal, size = 1.5))
#'   ggplot2::update_geom_defaults("pointrange", ggplot2::aes(colour = col_pal, fill = col_pal, linewidth = 0.66, size = 1.5 * 0.25))
#'   ggplot2::update_geom_defaults("polygon", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("quantile", ggplot2::aes(colour = col_pal, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("raster", ggplot2::aes(fill = col_pal))
#'   ggplot2::update_geom_defaults("rect", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("ribbon", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9 * 0.67, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("rug", ggplot2::aes(colour = col_pal, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("segment", ggplot2::aes(colour = col_pal, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("sf", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("smooth", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9 * 0.67, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("spoke", ggplot2::aes(colour = col_pal, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("step", ggplot2::aes(colour = col_pal, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("violin", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9, linewidth = 0.66))
#'   #to add and adjust once ggplot makes GeomBin2d
#'   ggplot2::update_geom_defaults("tile", ggplot2::aes(colour = NA, fill = col_pal, alpha = 0.9, linewidth = 0.66))
#' }
#'
#' #' Updated geom defaults for a dark mode
#' #'
#' #' @param col_pal A hex colour for the exterior outlines and interior fill for `gg_*(..., col = NULL, ...)`. Defaults to blue. Does not affect annotation type geoms (i.e. hline, vline, abline, curve, text and label).
#' #'
#' #' @return Updated geom defaults
#' #' @export
#' geom_defaults_dark <- function(col_pal = blue) {
#'
#'   ggplot2::update_geom_defaults("hline", ggplot2::aes(colour = "#C8D7DF", linewidth = 0.33))
#'   ggplot2::update_geom_defaults("vline", ggplot2::aes(colour = "#C8D7DF", linewidth = 0.33))
#'   ggplot2::update_geom_defaults("abline", ggplot2::aes(colour = "#C8D7DF", linewidth = 0.33))
#'   ggplot2::update_geom_defaults("curve", ggplot2::aes(colour = "#C8D7DF", linewidth = 0.33))
#'   ggplot2::update_geom_defaults("text", ggplot2::aes(colour = "#C8D7DF"))
#'   ggplot2::update_geom_defaults("label", ggplot2::aes(colour = "#C8D7DF", fill = "#C8D7DF", alpha = 0.05))
#'
#'   ggplot2::update_geom_defaults("area", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("bar", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("boxplot", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9 * 0.67, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("col", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("contour", ggplot2::aes(colour = col_pal, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("contour_filled", ggplot2::aes(fill = col_pal, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("crossbar", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9 * 0.67, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("density", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9 * 0.67, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("density2d", ggplot2::aes(colour = col_pal, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("density_2d_filled", ggplot2::aes(fill = col_pal, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("errorbar", ggplot2::aes(colour = col_pal, width = 0.2, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("function", ggplot2::aes(colour = col_pal, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("hex", ggplot2::aes(fill = col_pal))
#'   ggplot2::update_geom_defaults("line", ggplot2::aes(colour = col_pal, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("linerange", ggplot2::aes(colour = col_pal, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("path", ggplot2::aes(colour = col_pal, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("point", ggplot2::aes(colour = col_pal, fill = col_pal, size = 1.5))
#'   ggplot2::update_geom_defaults("pointrange", ggplot2::aes(colour = col_pal, fill = col_pal, linewidth = 0.66, size = 1.5 * 0.25))
#'   ggplot2::update_geom_defaults("polygon", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("quantile", ggplot2::aes(colour = col_pal, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("raster", ggplot2::aes(fill = col_pal))
#'   ggplot2::update_geom_defaults("rect", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("ribbon", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9 * 0.67, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("rug", ggplot2::aes(colour = col_pal, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("segment", ggplot2::aes(colour = col_pal, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("sf", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("smooth", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9 * 0.67, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("spoke", ggplot2::aes(colour = col_pal, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("step", ggplot2::aes(colour = col_pal, linewidth = 0.66))
#'   ggplot2::update_geom_defaults("violin", ggplot2::aes(colour = col_pal, fill = col_pal, alpha = 0.9, linewidth = 0.66))
#'   #to add and adjust once ggplot makes GeomBin2d
#'   ggplot2::update_geom_defaults("tile", ggplot2::aes(colour = NA, fill = col_pal, alpha = 0.9, linewidth = 0.66))
#' }
#'
