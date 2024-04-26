# global defaults
ggblanket_global <- new.env(parent = emptyenv())

ggblanket_global$mode <- NULL
ggblanket_global$col_palette_d <- NULL
ggblanket_global$col_palette_na_d <- NULL
ggblanket_global$col_palette_c <- NULL
ggblanket_global$col_palette_na_c <- NULL
ggblanket_global$theme <- NULL

#' Set a default mode
#'
#' @description Set a default mode for the mode argument in `gg_*` functions.
#'
#' @param new A default `*_mode_*`. E.g. [light_mode_t()], [grey_mode_r()], or [dark_mode_r()].
#'
#' @export
weave_mode <- function(new = light_mode_r()) {
  old <- ggblanket_global$mode
  ggblanket_global$mode <- new
  invisible(old)
}

#' Set a default theme
#'
#' @description Set a default theme to be `+`-ed on unmodified to `gg_*` functions. Note, `mode` takes precedence unless NULL.
#'
#' @param new A default ggplot2 theme to be `+`-ed on unmodified to `gg_*` functions.
#'
#' @export
weave_theme <- function(new = light_mode_r(orientation = "x")) {
  old <- ggblanket_global$theme
  ggblanket_global$theme <- new
  invisible(old)

  ggplot2::theme_set(new = new)
}

#' Set a series of geom defaults
#'
#' @description Update a series of geom defaults.
#'
#' @param colour A default hex colour (and fill) for geoms. Fill inherits from this colour. Defaults to `blue`.
#' @param linewidth A default linewidth for geoms. Fill inherits from this colour. Defaults to 0.66.
#' @param size A default point size for `*_point`. `*_pointrange` multiplies this by 0.25. Defaults to 1.5. .
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
    ggplot2::update_geom_defaults("pointrange", ggplot2::aes(linewidth = !!linewidth))
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
    ggplot2::update_geom_defaults("pointrange", ggplot2::aes(size = !!size * 0.25))
  }
}

#'  Set a series of annotate defaults
#'
#' @description Update a series of geom defaults commonly used for annotation (i.e. `*_vline`, `*_hline`, `*_abline`, `*_curve`, `*_text` and `*_label`).
#'
#' @param colour A default hex colour (and fill) for geoms commonly used for annotation (i.e. `*_vline`, `*_hline`, `*_abline`, `*_curve`, `*_text` and `*_label`). Defaults to "#121b24" (i.e. `"#121b24"`).
#' @param linewidth A default linewidth for geoms commonly used for annotation (i.e. `*_vline`, `*_hline`, `*_abline`, `*_curve`, `*_text` and `*_label`). Defaults to 0.33 (i.e. `0.33`).
#' @param size A default size for `*_text` and `*_label`. Defaults to 3.88.
#' @param family A default family for `*_text` and `*_label`. Defaults to ""
#'
#' @export
weave_annotate_aes <- function(colour = "#121b24", linewidth = 0.33, size = 3.88, family = "") {

  if (!rlang::is_null(colour)) {
    ggplot2::update_geom_defaults("hline", ggplot2::aes(colour = !!colour))
    ggplot2::update_geom_defaults("vline", ggplot2::aes(colour = !!colour))
    ggplot2::update_geom_defaults("abline", ggplot2::aes(colour = !!colour))
    ggplot2::update_geom_defaults("curve", ggplot2::aes(colour = !!colour))
    ggplot2::update_geom_defaults("text", ggplot2::aes(colour = !!colour))
    ggplot2::update_geom_defaults("label", ggplot2::aes(colour = !!colour, fill = !!colour, alpha = 0.05))
  }

  if (!rlang::is_null(linewidth)) {
    ggplot2::update_geom_defaults("hline", ggplot2::aes(linewidth = !!linewidth))
    ggplot2::update_geom_defaults("vline", ggplot2::aes(linewidth = !!linewidth))
    ggplot2::update_geom_defaults("abline", ggplot2::aes(linewidth = !!linewidth))
    ggplot2::update_geom_defaults("curve", ggplot2::aes(linewidth = !!linewidth))
  }

  if (!rlang::is_null(size)) {
    ggplot2::update_geom_defaults("text", ggplot2::aes(size = !!size))
    ggplot2::update_geom_defaults("label", ggplot2::aes(size = !!size))
  }

  if (!rlang::is_null(family)) {
    ggplot2::update_geom_defaults("text", ggplot2::aes(family = !!family))
    ggplot2::update_geom_defaults("label", ggplot2::aes(family = !!family))
  }
}

#' Set a default discrete colour palette
#'
#' @param new Colour palette to use for discrete scale. A character vector of hex codes (or names).
#' @param na A default colour for NA on a discrete scale. A hex code or name.
#'
#' @export
weave_col_palette_d <- function(new = jumble, na = "#cdc5bfff") {

    new2 <- c(new, rep(na, times = 13))

    old <- ggblanket_global$col_palette_d
    ggblanket_global$col_palette_d <- new2
    invisible(old)

    old <- ggblanket_global$col_palette_na_d
    ggblanket_global$col_palette_na_d <- na
    invisible(old)

    options(
      ggplot2.discrete.colour = function()
        ggplot2::scale_colour_manual(
          values = new2,
          na.value = na
        ),
      ggplot2.discrete.fill = function()
        ggplot2::scale_fill_manual(
          values = new2,
          na.value = na
        )
    )
}

#' Set a default continuous colour palette
#'
#' @param new Colour palette to use for continuous scale. A character vector of hex codes (or names).
#' @param na A default colour for NA on a continuous scale. A hex code or name.
#'
#' @export
weave_col_palette_c <- function(new = blues9, na = "#cdc5bfff") {

    old <- ggblanket_global$col_palette_c
    ggblanket_global$col_palette_c <- new
    invisible(old)

    old <- ggblanket_global$col_palette_na_c
    ggblanket_global$col_palette_na_c <- na
    invisible(old)

    options(
      ggplot2.continuous.colour = function()
        ggplot2::scale_color_gradientn(
          colours = new,
          na.value = na
        ),
      ggplot2.continuous.fill = function()
        ggplot2::scale_fill_gradientn(
          colours = new,
          na.value = na
        )
    )
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
get_col_palette_d <- function() ggblanket_global$col_palette_d

#' Get the default continuous palette
#' @description Get the currently set default continuous palette.
#' @noRd
get_col_palette_c <- function() ggblanket_global$col_palette_c

#' Get the default discrete NA colour
#' @description Get the currently set default discrete NA colour.
#' @noRd
get_col_palette_na_d <- function() ggblanket_global$col_palette_na_d

#' Get the default continuous NA colour
#' @description Get the currently set default continuous NA colour.
#' @noRd
get_col_palette_na_c <- function() ggblanket_global$col_palette_na_c
