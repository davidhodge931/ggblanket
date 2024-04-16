# global defaults
ggblanket_global <- new.env(parent = emptyenv())

ggblanket_global$mode <- NULL
ggblanket_global$col_palette_discrete <- NULL
ggblanket_global$col_palette_continuous <- NULL
ggblanket_global$theme <- NULL

#' Set a default mode
#'
#' @description Set a default mode for the mode argument in `gg_*` functions.
#'
#' @param mode A default `*_mode_*`. E.g. [light_mode_t()], [grey_mode_r()], or [dark_mode_r()].
#'
#' @export
weave_mode <- function(mode = light_mode_r()) {
  old <- ggblanket_global$mode
  ggblanket_global$mode <- mode
  invisible(old)
}

#' Set a default theme
#'
#' @description Set a default theme to be `+`-ed on unmodified to `gg_*` functions. Note any set or supplied mode takes precedence.
#'
#' @param theme A default ggplot2 theme to be `+`-ed on unmodified to `gg_*` functions.
#'
#' @export
weave_theme <- function(theme = light_mode_r() +
                          ggplot2::theme(
                            panel.grid.major.x = ggplot2::element_blank(),
                            axis.line.y = ggplot2::element_blank(),
                            axis.ticks.y = ggplot2::element_blank()
                            )
                        ) {
  old <- ggblanket_global$theme
  ggblanket_global$theme <- theme
  invisible(old)
}

#' Set a series of geom defaults
#'
#' @description Update a series of geom defaults.
#'
#' @param colour A default hex colour (and fill) for geoms. Fill inherits from this colour. Defaults to `blue`. Use NULL to leave colour, fill and alpha as is.
#' @param linewidth A default linewidth for geoms. Fill inherits from this colour. Defaults to 0.66. Use NULL to leave linewidth as is.
#' @param size A default point size for `*_point`. `*_pointrange` multiplies this by 0.25. Defaults to 1.5. . Use NULL to leave size as is.
#'
#' @export
weave_geom_aes <- function(colour = "#357ba2", linewidth = 0.66, size = 1.5) {

  if (!rlang::is_null(colour)) {
    ggplot2::update_geom_defaults("area", ggplot2::aes(colour = !!colour, fill = !!colour, alpha = 0.9))
    ggplot2::update_geom_defaults("bar", ggplot2::aes(colour = !!colour, fill = !!colour, alpha = 0.9))
    ggplot2::update_geom_defaults("boxplot", ggplot2::aes(colour = !!colour, fill = !!colour, alpha = 0.9 * 0.67))
    ggplot2::update_geom_defaults("col", ggplot2::aes(colour = !!colour, fill = !!colour, alpha = 0.9))
    ggplot2::update_geom_defaults("contour", ggplot2::aes(colour = !!colour))
    ggplot2::update_geom_defaults("contour_filled", ggplot2::aes(fill = !!colour))
    ggplot2::update_geom_defaults("crossbar", ggplot2::aes(colour = !!colour, fill = !!colour, alpha = 0.9 * 0.67))
    ggplot2::update_geom_defaults("density", ggplot2::aes(colour = !!colour, fill = !!colour, alpha = 0.9 * 0.67))
    ggplot2::update_geom_defaults("density2d", ggplot2::aes(colour = !!colour))
    ggplot2::update_geom_defaults("density_2d_filled", ggplot2::aes(fill = !!colour))
    ggplot2::update_geom_defaults("errorbar", ggplot2::aes(colour = !!colour))
    ggplot2::update_geom_defaults("function", ggplot2::aes(colour = !!colour))
    ggplot2::update_geom_defaults("hex", ggplot2::aes(fill = !!colour))
    ggplot2::update_geom_defaults("line", ggplot2::aes(colour = !!colour))
    ggplot2::update_geom_defaults("linerange", ggplot2::aes(colour = !!colour))
    ggplot2::update_geom_defaults("path", ggplot2::aes(colour = !!colour))
    ggplot2::update_geom_defaults("point", ggplot2::aes(colour = !!colour, fill = !!colour))
    ggplot2::update_geom_defaults("pointrange", ggplot2::aes(colour = !!colour, fill = !!colour))
    ggplot2::update_geom_defaults("polygon", ggplot2::aes(colour = !!colour, fill = !!colour, alpha = 0.9))
    ggplot2::update_geom_defaults("quantile", ggplot2::aes(colour = !!colour))
    ggplot2::update_geom_defaults("raster", ggplot2::aes(fill = !!colour))
    ggplot2::update_geom_defaults("rect", ggplot2::aes(colour = !!colour, fill = !!colour, alpha = 0.9))
    ggplot2::update_geom_defaults("ribbon", ggplot2::aes(colour = !!colour, fill = !!colour, alpha = 0.9 * 0.67))
    ggplot2::update_geom_defaults("rug", ggplot2::aes(colour = !!colour))
    ggplot2::update_geom_defaults("segment", ggplot2::aes(colour = !!colour))
    ggplot2::update_geom_defaults("sf", ggplot2::aes(colour = !!colour, fill = !!colour, alpha = 0.9))
    ggplot2::update_geom_defaults("smooth", ggplot2::aes(colour = !!colour, fill = !!colour, alpha = 0.9 * 0.67))
    ggplot2::update_geom_defaults("spoke", ggplot2::aes(colour = !!colour))
    ggplot2::update_geom_defaults("step", ggplot2::aes(colour = !!colour))
    ggplot2::update_geom_defaults("violin", ggplot2::aes(colour = !!colour, fill = !!colour, alpha = 0.9))
    #to add and adjust once ggplot makes GeomBin2d
    ggplot2::update_geom_defaults("tile", ggplot2::aes(colour = NA, fill = !!colour, alpha = 0.9))
  }

  if (!rlang::is_null(linewidth)) {
    ggplot2::update_geom_defaults("area", ggplot2::aes(linewidth = !!linewidth))
    ggplot2::update_geom_defaults("bar", ggplot2::aes(linewidth = !!linewidth))
    ggplot2::update_geom_defaults("boxplot", ggplot2::aes(linewidth = !!linewidth))
    ggplot2::update_geom_defaults("col", ggplot2::aes(linewidth = !!linewidth))
    ggplot2::update_geom_defaults("contour", ggplot2::aes(linewidth = !!linewidth))
    ggplot2::update_geom_defaults("contour_filled", ggplot2::aes(linewidth = !!linewidth))
    ggplot2::update_geom_defaults("crossbar", ggplot2::aes(linewidth = !!linewidth))
    ggplot2::update_geom_defaults("density", ggplot2::aes(linewidth = !!linewidth))
    ggplot2::update_geom_defaults("density2d", ggplot2::aes(linewidth = !!linewidth))
    ggplot2::update_geom_defaults("density_2d_filled", ggplot2::aes(linewidth = !!linewidth))
    ggplot2::update_geom_defaults("errorbar", ggplot2::aes(linewidth = !!linewidth))
    ggplot2::update_geom_defaults("function", ggplot2::aes(linewidth = !!linewidth))
    ggplot2::update_geom_defaults("line", ggplot2::aes(linewidth = !!linewidth))
    ggplot2::update_geom_defaults("linerange", ggplot2::aes(linewidth = !!linewidth))
    ggplot2::update_geom_defaults("path", ggplot2::aes(linewidth = !!linewidth))
    ggplot2::update_geom_defaults("polygon", ggplot2::aes(linewidth = !!linewidth))
    ggplot2::update_geom_defaults("quantile", ggplot2::aes(linewidth = !!linewidth))
    ggplot2::update_geom_defaults("rect", ggplot2::aes(linewidth = !!linewidth))
    ggplot2::update_geom_defaults("ribbon", ggplot2::aes(linewidth = !!linewidth))
    ggplot2::update_geom_defaults("rug", ggplot2::aes(linewidth = !!linewidth))
    ggplot2::update_geom_defaults("segment", ggplot2::aes(linewidth = !!linewidth))
    ggplot2::update_geom_defaults("sf", ggplot2::aes(linewidth = !!linewidth))
    ggplot2::update_geom_defaults("smooth", ggplot2::aes(linewidth = !!linewidth))
    ggplot2::update_geom_defaults("spoke", ggplot2::aes(linewidth = !!linewidth))
    ggplot2::update_geom_defaults("step", ggplot2::aes(linewidth = !!linewidth))
    ggplot2::update_geom_defaults("violin", ggplot2::aes(linewidth = !!linewidth))
    #to add and adjust once ggplot makes GeomBin2d
    ggplot2::update_geom_defaults("tile", ggplot2::aes(linewidth = !!linewidth))
  }

  if (!rlang::is_null(size)) {
    ggplot2::update_geom_defaults("point", ggplot2::aes(size = !!size))
    ggplot2::update_geom_defaults("pointrange", ggplot2::aes(linewidth = !!linewidth, size = !!size * 0.25))
  }
}

#'  Set a series of annotate defaults
#'
#' @description Update a series of geom defaults commonly used for annotation (i.e. `*_vline`, `*_hline`, `*_abline`, `*_curve`, `*_text` and `*_label`).
#'
#' @param colour A default hex colour (and fill) for geoms commonly used for annotation (i.e. `*_vline`, `*_hline`, `*_abline`, `*_curve`, `*_text` and `*_label`). Defaults to "#121b24" (i.e. `lightness[1]`).
#' @param linewidth A default linewidth for geoms commonly used for annotation (i.e. `*_vline`, `*_hline`, `*_abline`, `*_curve`, `*_text` and `*_label`). Defaults to 0.33 (i.e. `linewidthness[1]`).
#' @param size A default size for `*_text` and `*_label`. Defaults to 3.88.
#' @param family A default family for `*_text` and `*_label`. Defaults to ""
#'
#' @export
weave_annotate_aes <- function(colour = "#121b24", linewidth = 0.33, size = 3.88, family = "") {
  ggplot2::update_geom_defaults("hline", ggplot2::aes(colour = !!colour, linewidth = !!linewidth))
  ggplot2::update_geom_defaults("vline", ggplot2::aes(colour = !!colour, linewidth = !!linewidth))
  ggplot2::update_geom_defaults("abline", ggplot2::aes(colour = !!colour, linewidth = !!linewidth))
  ggplot2::update_geom_defaults("curve", ggplot2::aes(colour = !!colour, linewidth = !!linewidth))
  ggplot2::update_geom_defaults("text", ggplot2::aes(colour = !!colour, size = !!size, family = !!family))
  ggplot2::update_geom_defaults("label", ggplot2::aes(colour = !!colour, fill = !!colour, alpha = 0.05, size = !!size, family = !!family))
}

#' Set a default discrete colour palette
#'
#' @param discrete Colour palette to use for discrete scale. A character vector of hex codes (or names).
#'
#' @noRd
weave_col_palette_discrete <- function(discrete = c(teal, orange, navy, pink)) {
  old <- ggblanket_global$col_palette_discrete
  ggblanket_global$col_palette_discrete <- discrete
  invisible(old)
}

#' Set a default continuous colour palette
#'
#' @param continuous Colour palette to use for continuous scale. A character vector of hex codes (or names).
#'
#' @noRd
weave_col_palette_continuous <- function(continuous = viridisLite::mako(n = 20, direction = -1)) {
  old <- ggblanket_global$col_palette_continuous
  ggblanket_global$col_palette_continuous <- continuous
  invisible(old)
}

#' Set a default col_palette
#'
#' @description Set a default discrete and continuous col_palette for the col_palette argument in `gg_*` functions.
#'
#' @param discrete A default col_palette to use in the discrete scale. A character vector of hex codes (or names). Use NULL to leave as is.
#' @param continuous A default col_palette to use in the continuous scale. A character vector of hex codes (or names). Use NULL to leave as is.
#'
#' @export
weave_col_palette <- function(discrete = c(teal, orange, navy, pink),
                              continuous = viridisLite::mako(n = 9)) {
  if (!rlang::is_null(discrete)) weave_col_palette_discrete(discrete)
  if (!rlang::is_null(continuous)) weave_col_palette_continuous(continuous)
}

#' Get the default mode
#' @description Get the currently set default mode.
#' @noRd
get_mode <- function() ggblanket_global$mode

#' Get the default theme
#' @description Get the currently set default theme.
#' @noRd
get_theme <- function() ggblanket_global$theme

#' Get the default discrete palette
#' @description Get the currently set default discrete palette.
#' @noRd
get_col_palette_discrete <- function() ggblanket_global$col_palette_discrete

#' Get the default continuous palette
#' @description Get the currently set default continuous palette.
#' @noRd
get_col_palette_continuous <- function() ggblanket_global$col_palette_continuous
