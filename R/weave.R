#' Get the default mode
#'
#' @description Get the current globally set theme that is added to the `mode` argument where NULL in the the `gg_*` functions.
#' Note [ggplot2::theme_get()] sets globally a new theme that is `+`-ed on as a layer to the `gg_*` functions.
#'
#' @noRd
get_mode <- function() {
  if (!identical(theme_env$theme_current, ggplot2::theme_get())) {
    theme_env$mode_current <- ggplot2::theme_get()
    theme_env$theme_current <- ggplot2::theme_get()
    theme <- theme_env$theme_current
  } else {
    theme <- theme_env$mode_current
  }
  theme
}

#' Set the default mode
#'
#' @description Set the default `mode` for `gg_*` functions.
#'
#' @param new A new `*_mode_*` theme (e.g. [dark_mode_r()]).
#'
#' @return A globally set mode
#' @export
#'
#' @examples
#' weave_mode(dark_mode_r())
#'
weave_mode <- function(new = light_mode_r()) {
  mode_old <- theme_env$mode_current
  theme_env$mode_current <- new
  theme_env$theme_current <- ggplot2::theme_get()
  invisible(mode_old)
}

# internal
theme_env <- new.env(parent = emptyenv())
theme_env$mode_current <- ggplot2::theme_grey()
theme_env$theme_current <- ggplot2::theme_grey()

#' Update a series of geom defaults
#'
#' @description Update a series of geom defaults.
#'
#' @param colour A hex colour for geoms. Defaults to `blue`. Also used for fill.
#'
#' @return Globally updated geom defaults
#' @export
weave_geom_defaults <- function(colour = blue) {

  ggplot2::update_geom_defaults("area", ggplot2::aes(colour = colour, fill = colour, alpha = 0.9, linewidth = 0.66))
  ggplot2::update_geom_defaults("bar", ggplot2::aes(colour = colour, fill = colour, alpha = 0.9, linewidth = 0.66))
  ggplot2::update_geom_defaults("boxplot", ggplot2::aes(colour = colour, fill = colour, alpha = 0.9 * 0.67, linewidth = 0.66))
  ggplot2::update_geom_defaults("col", ggplot2::aes(colour = colour, fill = colour, alpha = 0.9, linewidth = 0.66))
  ggplot2::update_geom_defaults("contour", ggplot2::aes(colour = colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("contour_filled", ggplot2::aes(fill = colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("crossbar", ggplot2::aes(colour = colour, fill = colour, alpha = 0.9 * 0.67, linewidth = 0.66))
  ggplot2::update_geom_defaults("density", ggplot2::aes(colour = colour, fill = colour, alpha = 0.9 * 0.67, linewidth = 0.66))
  ggplot2::update_geom_defaults("density2d", ggplot2::aes(colour = colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("density_2d_filled", ggplot2::aes(fill = colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("errorbar", ggplot2::aes(colour = colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("function", ggplot2::aes(colour = colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("hex", ggplot2::aes(fill = colour))
  ggplot2::update_geom_defaults("line", ggplot2::aes(colour = colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("linerange", ggplot2::aes(colour = colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("path", ggplot2::aes(colour = colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("point", ggplot2::aes(colour = colour, fill = colour, size = 1.5))
  ggplot2::update_geom_defaults("pointrange", ggplot2::aes(colour = colour, fill = colour, linewidth = 0.66, size = 1.5 * 0.25))
  ggplot2::update_geom_defaults("polygon", ggplot2::aes(colour = colour, fill = colour, alpha = 0.9, linewidth = 0.66))
  ggplot2::update_geom_defaults("quantile", ggplot2::aes(colour = colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("raster", ggplot2::aes(fill = colour))
  ggplot2::update_geom_defaults("rect", ggplot2::aes(colour = colour, fill = colour, alpha = 0.9, linewidth = 0.66))
  ggplot2::update_geom_defaults("ribbon", ggplot2::aes(colour = colour, fill = colour, alpha = 0.9 * 0.67, linewidth = 0.66))
  ggplot2::update_geom_defaults("rug", ggplot2::aes(colour = colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("segment", ggplot2::aes(colour = colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("sf", ggplot2::aes(colour = colour, fill = colour, alpha = 0.9, linewidth = 0.66))
  ggplot2::update_geom_defaults("smooth", ggplot2::aes(colour = colour, fill = colour, alpha = 0.9 * 0.67, linewidth = 0.66))
  ggplot2::update_geom_defaults("spoke", ggplot2::aes(colour = colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("step", ggplot2::aes(colour = colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("violin", ggplot2::aes(colour = colour, fill = colour, alpha = 0.9, linewidth = 0.66))
  #to add and adjust once ggplot makes GeomBin2d
  ggplot2::update_geom_defaults("tile", ggplot2::aes(colour = NA, fill = colour, alpha = 0.9, linewidth = 0.66))
}

#' Update a series of annotation defaults
#'
#' @description Update a series of geom defaults for annotation (i.e. `*_hline`, `*_vline`, `*_abline`, `*_curve`, `*_text` and `*_label`).
#'
#' @param colour A hex colour for annotation. Defaults to `lightness[2]`.
#'
#' @return Globally updated annotation defaults
#' @export
#'
#' @examples
#' weave_annotate_defaults(darkness[2])
#'
weave_annotate_defaults <- function(colour = lightness[2]) {
  ggplot2::update_geom_defaults("hline", ggplot2::aes(colour = colour, linewidth = 0.33))
  ggplot2::update_geom_defaults("vline", ggplot2::aes(colour = colour, linewidth = 0.33))
  ggplot2::update_geom_defaults("abline", ggplot2::aes(colour = colour, linewidth = 0.33))
  ggplot2::update_geom_defaults("curve", ggplot2::aes(colour = colour, linewidth = 0.33))
  ggplot2::update_geom_defaults("text", ggplot2::aes(colour = colour))
  ggplot2::update_geom_defaults("label", ggplot2::aes(colour = colour, fill = colour, alpha = 0.05))
}
