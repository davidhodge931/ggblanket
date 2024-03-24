#' #' Set the mode and geom_defaults
#' #'
#' #' @param mode A `*_mode_*` theme set globally for when mode = NULL. E.g. [light_mode_t()], [grey_mode_r()], or [dark_mode_r()].
#' #' @param geom_defaults TRUE or FALSE of whether to update geom defaults to ggblanket defaults. Defaults to TRUE.
#' #'
#' #' @return A globally set mode.
#' #' @export
#' set_gg <- function(
#'     mode = light_mode_r(),
#'     geom_defaults = TRUE
#' ) {
#'
#'   set_mode(mode)
#'
#'   if (isTRUE(geom_defaults)) {
#'     ggplot2::update_geom_defaults("hline", ggplot2::aes(colour = "#121B24", linewidth = 0.33))
#'     ggplot2::update_geom_defaults("vline", ggplot2::aes(colour = "#121B24", linewidth = 0.33))
#'     ggplot2::update_geom_defaults("curve", ggplot2::aes(colour = "#121B24", linewidth = 0.33))
#'     ggplot2::update_geom_defaults("abline", ggplot2::aes(colour = blue, fill = blue, linewidth = 0.66))
#'     ggplot2::update_geom_defaults("area", ggplot2::aes(colour = blue, fill = blue, alpha = 0.9, linewidth = 0.66))
#'     ggplot2::update_geom_defaults("bar", ggplot2::aes(colour = blue, fill = blue, alpha = 0.9, linewidth = 0.66))
#'     ggplot2::update_geom_defaults("boxplot", ggplot2::aes(colour = blue, fill = blue, alpha = 0.5, linewidth = 0.66))
#'     ggplot2::update_geom_defaults("col", ggplot2::aes(colour = blue, fill = blue, alpha = 0.9, linewidth = 0.66))
#'     ggplot2::update_geom_defaults("contour", ggplot2::aes(colour = blue, linewidth = 0.66))
#'     ggplot2::update_geom_defaults("contour_filled", ggplot2::aes(fill = blue, linewidth = 0.66))
#'     ggplot2::update_geom_defaults("crossbar", ggplot2::aes(colour = blue, fill = blue, alpha = 0.5, linewidth = 0.66))
#'     ggplot2::update_geom_defaults("density", ggplot2::aes(colour = blue, fill = blue, alpha = 0.5, linewidth = 0.66))
#'     ggplot2::update_geom_defaults("density2d", ggplot2::aes(colour = blue, linewidth = 0.66))
#'     ggplot2::update_geom_defaults("density_2d_filled", ggplot2::aes(fill = blue, linewidth = 0.66))
#'     ggplot2::update_geom_defaults("errorbar", ggplot2::aes(colour = blue, width = 0.2, linewidth = 0.66))
#'     ggplot2::update_geom_defaults("function", ggplot2::aes(colour = blue, linewidth = 0.66))
#'     ggplot2::update_geom_defaults("hex", ggplot2::aes(fill = blue))
#'     ggplot2::update_geom_defaults("label", ggplot2::aes(colour = blue, fill = blue, alpha = 0.1))
#'     ggplot2::update_geom_defaults("line", ggplot2::aes(colour = blue, linewidth = 0.66))
#'     ggplot2::update_geom_defaults("linerange", ggplot2::aes(colour = blue, linewidth = 0.66))
#'     ggplot2::update_geom_defaults("path", ggplot2::aes(colour = blue, linewidth = 0.66))
#'     ggplot2::update_geom_defaults("point", ggplot2::aes(colour = blue, fill = blue))
#'     ggplot2::update_geom_defaults("pointrange", ggplot2::aes(colour = blue, fill = blue, linewidth = 0.66, size = 0.2))
#'     ggplot2::update_geom_defaults("polygon", ggplot2::aes(colour = blue, fill = blue, alpha = 0.9, linewidth = 0.66))
#'     ggplot2::update_geom_defaults("quantile", ggplot2::aes(colour = blue, linewidth = 0.66))
#'     ggplot2::update_geom_defaults("raster", ggplot2::aes(fill = blue))
#'     ggplot2::update_geom_defaults("rect", ggplot2::aes(colour = blue, fill = blue, alpha = 0.9, linewidth = 0.66))
#'     ggplot2::update_geom_defaults("ribbon", ggplot2::aes(colour = blue, fill = blue, alpha = 0.4, linewidth = 0.66))
#'     ggplot2::update_geom_defaults("rug", ggplot2::aes(colour = blue, linewidth = 0.66))
#'     ggplot2::update_geom_defaults("segment", ggplot2::aes(colour = blue, linewidth = 0.66))
#'     ggplot2::update_geom_defaults("sf", ggplot2::aes(colour = blue, fill = blue, alpha = 0.9, linewidth = 0.66))
#'     ggplot2::update_geom_defaults("smooth", ggplot2::aes(colour = blue, fill = blue, linewidth = 0.66))
#'     ggplot2::update_geom_defaults("spoke", ggplot2::aes(colour = blue, linewidth = 0.66))
#'     ggplot2::update_geom_defaults("step", ggplot2::aes(colour = blue, linewidth = 0.66))
#'     ggplot2::update_geom_defaults("text", ggplot2::aes(colour = blue, fill = blue))
#'     ggplot2::update_geom_defaults("violin", ggplot2::aes(colour = blue, fill = blue, alpha = 0.9, linewidth = 0.66))
#'     #to add and adjust once ggplot makes GeomBin2d
#'     ggplot2::update_geom_defaults("tile", ggplot2::aes(colour = NA, fill = blue, alpha = 0.9, linewidth = 0.66))
#'   }
#' }
