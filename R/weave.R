# internal
ggblanket_global <- new.env(parent = emptyenv())
ggblanket_global$mode_current <- light_mode_r()
ggblanket_global$palette_d_current <- c(teal, orange, navy, pink)
ggblanket_global$palette_c_current <- viridisLite::mako(n = 9)

#' Get the default mode
#'
#' @description Get the current globally set theme that is added to the `mode` argument where NULL in the the `gg_*` functions.
#' Note [ggplot2::theme_get()] sets globally a new theme that is `+`-ed on as a layer to the `gg_*` functions.
#'
#' @noRd
get_mode <- function() {
  ggblanket_global$mode_current
}

#' Set the default mode
#'
#' @description Set the default `mode` for `gg_*` functions.
#'
#' @param mode A new `*_mode_*` theme (e.g. [dark_mode_r()]).
#'
#' @noRd
weave_mode <- function(mode = light_mode_r()) {
  old <- ggblanket_global$mode_current
  ggblanket_global$mode_current <- mode
  invisible(old)
}

#' Update a series of geom defaults
#'
#' @description Update a series of geom defaults.
#'
#' @param colour A hex colour. Defaults to #357ba2 (i.e. `blue`). The fill inherits.
#'
#' @noRd
weave_colour <- function(colour = "#357ba2") {

  ggplot2::update_geom_defaults("area", ggplot2::aes(colour = !!colour, fill = !!colour, alpha = 0.9, linewidth = 0.66))
  ggplot2::update_geom_defaults("bar", ggplot2::aes(colour = !!colour, fill = !!colour, alpha = 0.9, linewidth = 0.66))
  ggplot2::update_geom_defaults("boxplot", ggplot2::aes(colour = !!colour, fill = !!colour, alpha = 0.9 * 0.67, linewidth = 0.66))
  ggplot2::update_geom_defaults("col", ggplot2::aes(colour = !!colour, fill = !!colour, alpha = 0.9, linewidth = 0.66))
  ggplot2::update_geom_defaults("contour", ggplot2::aes(colour = !!colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("contour_filled", ggplot2::aes(fill = !!colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("crossbar", ggplot2::aes(colour = !!colour, fill = !!colour, alpha = 0.9 * 0.67, linewidth = 0.66))
  ggplot2::update_geom_defaults("density", ggplot2::aes(colour = !!colour, fill = !!colour, alpha = 0.9 * 0.67, linewidth = 0.66))
  ggplot2::update_geom_defaults("density2d", ggplot2::aes(colour = !!colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("density_2d_filled", ggplot2::aes(fill = !!colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("errorbar", ggplot2::aes(colour = !!colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("function", ggplot2::aes(colour = !!colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("hex", ggplot2::aes(fill = !!colour))
  ggplot2::update_geom_defaults("line", ggplot2::aes(colour = !!colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("linerange", ggplot2::aes(colour = !!colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("path", ggplot2::aes(colour = !!colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("point", ggplot2::aes(colour = !!colour, fill = !!colour, size = 1.5))
  ggplot2::update_geom_defaults("pointrange", ggplot2::aes(colour = !!colour, fill = !!colour, linewidth = 0.66, size = 1.5 * 0.25))
  ggplot2::update_geom_defaults("polygon", ggplot2::aes(colour = !!colour, fill = !!colour, alpha = 0.9, linewidth = 0.66))
  ggplot2::update_geom_defaults("quantile", ggplot2::aes(colour = !!colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("raster", ggplot2::aes(fill = !!colour))
  ggplot2::update_geom_defaults("rect", ggplot2::aes(colour = !!colour, fill = !!colour, alpha = 0.9, linewidth = 0.66))
  ggplot2::update_geom_defaults("ribbon", ggplot2::aes(colour = !!colour, fill = !!colour, alpha = 0.9 * 0.67, linewidth = 0.66))
  ggplot2::update_geom_defaults("rug", ggplot2::aes(colour = !!colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("segment", ggplot2::aes(colour = !!colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("sf", ggplot2::aes(colour = !!colour, fill = !!colour, alpha = 0.9, linewidth = 0.66))
  ggplot2::update_geom_defaults("smooth", ggplot2::aes(colour = !!colour, fill = !!colour, alpha = 0.9 * 0.67, linewidth = 0.66))
  ggplot2::update_geom_defaults("spoke", ggplot2::aes(colour = !!colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("step", ggplot2::aes(colour = !!colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("violin", ggplot2::aes(colour = !!colour, fill = !!colour, alpha = 0.9, linewidth = 0.66))
  #to add and adjust once ggplot makes GeomBin2d
  ggplot2::update_geom_defaults("tile", ggplot2::aes(colour = NA, fill = !!colour, alpha = 0.9, linewidth = 0.66))
}

#'  Update a series of annotate defaults
#'
#' @description Update a series of geom defaults commonly used for annotation (i.e. `*_vline`, `*_hline`, `*_abline`, `*_curve`, `*_text` and `*_label`).
#'
#' @param colour A hex colour. Defaults to "#121b24" (i.e. `lightness[1]`). The default annotate fill inherits.
#'
#' @noRd
weave_annotate <- function(annotate = "#121b24") {
  ggplot2::update_geom_defaults("hline", ggplot2::aes(colour = !!annotate, linewidth = 0.33))
  ggplot2::update_geom_defaults("vline", ggplot2::aes(colour = !!annotate, linewidth = 0.33))
  ggplot2::update_geom_defaults("abline", ggplot2::aes(colour = !!annotate, linewidth = 0.33))
  ggplot2::update_geom_defaults("curve", ggplot2::aes(colour = !!annotate, linewidth = 0.33))
  ggplot2::update_geom_defaults("text", ggplot2::aes(colour = !!annotate))
  ggplot2::update_geom_defaults("label", ggplot2::aes(colour = !!annotate, fill = !!annotate, alpha = 0.05))
}

#' Set a default discrete colour palette
#'
#' @param col_palette_d Colour palette to use for discrete scale. A character vector of hex codes (or names).
#'
#' @noRd
weave_col_palette_d <- function(col_palette_d = c(teal, orange, navy, pink)) {
  # options(ggblanket.col_palette_d = col_palette_d)
  old <- ggblanket_global$palette_d_current
  ggblanket_global$palette_d_current <- col_palette_d
  invisible(old)
}

#' Set a default continuous colour palette
#'
#' @param col_palette_c Colour palette to use for continuous scale. A character vector of hex codes (or names).
#'
#' @noRd
weave_col_palette_c <- function(col_palette_c = viridisLite::mako(9)) {
  # options(ggblanket.col_palette_c = col_palette_c)
  old <- ggblanket_global$palette_c_current
  ggblanket_global$palette_c_current <- col_palette_c
  invisible(old) 
}

#' Set a default discrete and continuous colour palettes
#'
#' @param col_palette_d Colour palette to use for discrete scale. A character vector of hex codes (or names).
#' @param col_palette_c Colour palette to use for continuous scale. A character vector of hex codes (or names).
#'
#' @noRd
weave_col_palette <- function(col_palette_d = c(teal, orange, navy, pink),
                              col_palette_c = viridisLite::mako(n = 9)) {
  weave_col_palette_d(col_palette_d)
  weave_col_palette_c(col_palette_c)
}