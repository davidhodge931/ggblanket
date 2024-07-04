# global defaults
ggblanket_global <- new.env(parent = emptyenv())

ggblanket_global$mode <- NULL
ggblanket_global$col_palette_d <- NULL
ggblanket_global$col_palette_c <- NULL
ggblanket_global$col_palette_o <- NULL
ggblanket_global$col_palette_na_d <- NULL
ggblanket_global$col_palette_na_c <- NULL
ggblanket_global$col_palette_na_o <- NULL
ggblanket_global$theme <- NULL

#' Set a mode
#'
#' @description Set a mode for the mode argument in `gg_*` functions.
#'
#' @param mode A ggplot2 theme (e.g. [light_mode_t()] or [dark_mode_r()]) that anticipates `gg_*` side-effects of removing relevant axis line/ticks and gridlines per the `mode_orientation`.
#'
#' @export
weave_mode <- function(mode = light_mode_r()) {
  old <- ggblanket_global$mode
  ggblanket_global$mode <- mode
  invisible(old)
}

#' Set a theme
#'
#' @description Set a theme to be `+`-ed on unmodified to `gg_*` functions. Note, `mode` takes precedence unless NULL.
#'
#' @param theme A ggplot2 theme that the `gg_*` function will add without side-effects. Note, `mode` takes precedence, unless `mode = NULL`.
#'
#' @export
weave_theme <- function(theme = light_mode_r() + mode_orientation_to_x()) {
  old <- ggblanket_global$theme
  ggblanket_global$theme <- theme
  invisible(old)

  ggplot2::theme_set(new = theme)
}

#' Set a series of geom defaults
#'
#' @description Update all geom defaults.
#'
#' @param colour A hex colour (and fill) for geoms other than `*_hline`/`*_vline`/`*_abline` and `*_curve`. Fill inherits from this colour. Defaults to `blue` (i.e. `#357BA2FF`).
#' @param annotation_colour A hex annotation_colour (and fill) for `*_hline`/`*_vline`/`*_abline` and `*_curve`. Defaults to "#121b24" (i.e. `lightness[1]`).
#' @param annotation_linewidth A annotation_linewidth for `*_hline`/`*_vline`/`*_abline` and `*_curve`. Defaults to 0.33.
#' @param annotation_family A annotation_family for `*_text` and `*_label`. Defaults to ""
#' @param annotation_size A annotation_size for `*_text` and `*_label`. Defaults to 3.88.
#'
#' @export
weave_geom_defaults <- function(colour = "#357BA2FF",
                                annotation_colour = "#121B24FF",
                                annotation_linewidth = 0.33,
                                annotation_family = "",
                                annotation_size = 3.88) {

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
    ggplot2::update_geom_defaults("pointrange", ggplot2::aes(colour = !!colour, fill = !!colour, linewidth = 0.66, size = 1.5 / 7.5))
    ggplot2::update_geom_defaults("polygon", ggplot2::aes(colour = !!colour, fill = !!colour, alpha = 0.9, linewidth = 0.66))
    ggplot2::update_geom_defaults("quantile", ggplot2::aes(colour = !!colour, linewidth = 0.66))
    ggplot2::update_geom_defaults("raster", ggplot2::aes(fill = !!colour))
    ggplot2::update_geom_defaults("rect", ggplot2::aes(colour = !!colour, fill = !!colour, alpha = 0.9, linewidth = 0.66))
    ggplot2::update_geom_defaults("ribbon", ggplot2::aes(colour = !!colour, fill = !!colour, alpha = 0.9 * 0.67, linewidth = 0.66))
    ggplot2::update_geom_defaults("rug", ggplot2::aes(colour = !!colour, linewidth = 0.66))
    ggplot2::update_geom_defaults("segment", ggplot2::aes(colour = !!colour, linewidth = 0.66))
    ggplot2::update_geom_defaults("sf", ggplot2::aes(colour = !!colour, fill = !!colour, alpha = 0.9, linewidth = 0.66, size = 1.5))
    ggplot2::update_geom_defaults("smooth", ggplot2::aes(colour = !!colour, fill = !!colour, alpha = 0.9 * 0.67, linewidth = 0.66))
    ggplot2::update_geom_defaults("spoke", ggplot2::aes(colour = !!colour, linewidth = 0.66))
    ggplot2::update_geom_defaults("step", ggplot2::aes(colour = !!colour, linewidth = 0.66))
    ggplot2::update_geom_defaults("violin", ggplot2::aes(colour = !!colour, fill = !!colour, alpha = 0.9, linewidth = 0.66))
    #to add and adjust once ggplot makes GeomBin2d
    ggplot2::update_geom_defaults("tile", ggplot2::aes(colour = NA, fill = !!colour, alpha = 0.9, linewidth = 0.66))

    ggplot2::update_geom_defaults("abline", ggplot2::aes(colour = !!annotation_colour, linewidth = !!annotation_linewidth))
    ggplot2::update_geom_defaults("hline", ggplot2::aes(colour = !!annotation_colour, linewidth = !!annotation_linewidth))
    ggplot2::update_geom_defaults("vline", ggplot2::aes(colour = !!annotation_colour, linewidth = !!annotation_linewidth))
    ggplot2::update_geom_defaults("curve", ggplot2::aes(colour = !!annotation_colour, linewidth = !!annotation_linewidth))
    ggplot2::update_geom_defaults("text", ggplot2::aes(colour = !!annotation_colour, size = !!annotation_size, family = !!annotation_family))
    ggplot2::update_geom_defaults("label", ggplot2::aes(colour = !!annotation_colour, fill = !!annotation_colour, alpha = 0.05, size = !!annotation_size, family = !!annotation_family))
}

#' Set a discrete colour palette
#'
#' @param col_palette_d For a discrete scale, a character vector of hex codes (or col_palette_d_names) for the `col_palette_d`.
#' @param col_palette_d_na For a discrete scale, a hex code (or col_palette_d_name) for the `col_palette_d_col_palette_d_na`.
#'
#' @export
weave_col_palette_d <- function(col_palette_d = jumble, col_palette_d_na = "#CDC5BFFF") {

  if (rlang::is_null(col_palette_d_na)) col_palette_d_na <- "grey50"

  if (!rlang::is_null(col_palette_d)) {
    if (!rlang::is_function(col_palette_d)) {
      col_palette_d <- c(col_palette_d, rep(col_palette_d_na, times = 100))
    }
  }

  old <- ggblanket_global$col_palette_d
  ggblanket_global$col_palette_d <- col_palette_d
  invisible(old)

  old <- ggblanket_global$col_palette_na_d
  ggblanket_global$col_palette_na_d <- col_palette_d_na
  invisible(old)

  if (rlang::is_null(col_palette_d)) {
    options(
      ggplot2.discrete.colour = function()
        ggplot2::scale_colour_hue(),
      ggplot2.discrete.fill = function()
        ggplot2::scale_fill_hue()
    )
  }
  else {
    options(
      ggplot2.discrete.colour = function()
        ggplot2::scale_colour_manual(
          values = col_palette_d,
          na.value = col_palette_d_na
        ),
      ggplot2.discrete.fill = function()
        ggplot2::scale_fill_manual(
          values = col_palette_d,
          na.value = col_palette_d_na
        )
    )
  }
}

#' Set a continuous colour palette
#'
#' @param col_palette_c For a continuous scale, a character vector of hex codes (or col_palette_c_names)
#' @param col_palette_c_na For a continuous scale, a hex code (or col_palette_c_name) for the `col_palette_col_palette_c_na`.
#'
#' @export
weave_col_palette_c <- function(col_palette_c = viridisLite::mako(n = 9, direction = -1),
                                col_palette_c_na = "#988F88FF") { # i.e. colorspace::darken(grey, 0.25)

  if (rlang::is_null(col_palette_c)) {
    col_palette_c <- scales::pal_seq_gradient(low = "#132B43", high = "#56B1F7")(seq(0, 1, length.out = 20))
  }

  if (rlang::is_null(col_palette_c_na)) {
    col_palette_c_na <- "grey50"
  }

  old <- ggblanket_global$col_palette_c
  ggblanket_global$col_palette_c <- col_palette_c
  invisible(old)

  old <- ggblanket_global$col_palette_c_na
  ggblanket_global$col_palette_c_na <- col_palette_c_na
  invisible(old)

  options(
    ggplot2.continuous.colour = function()
      ggplot2::scale_color_gradientn(
        colours = col_palette_c,
        na.value = col_palette_c_na
      ),
    ggplot2.continuous.fill = function()
      ggplot2::scale_fill_gradientn(
        colours = col_palette_c,
        na.value = col_palette_c_na
      )
  )
}

#' Set an ordinal colour palette
#'
#' @param col_palette_o For an ordicol_palette_o_nal scale, a `scales::pal_*()` function for the `col_palette_o`.
#' @param col_palette_o_na For an ordicol_palette_o_nal scale, a hex code (or col_palette_o_name) for the `col_palette_o_col_palette_o_na`.
#'
#' @export
weave_col_palette_o <- function(col_palette_o = scales::pal_viridis(option = "G", direction = -1),
                                col_palette_o_na = "#988F88FF") {

  if (rlang::is_null(col_palette_o)) col_palette_o <- scales::pal_viridis()
  if (rlang::is_null(col_palette_o_na)) col_palette_o_na <- "grey50"

  old <- ggblanket_global$col_palette_o
  ggblanket_global$col_palette_o <- col_palette_o
  invisible(old)

  old <- ggblanket_global$col_palette_o_na
  ggblanket_global$col_palette_o_na <- col_palette_o_na
  invisible(old)
}

#' Get the mode
#' @description Get the currently set mode.
#' @noRd
get_mode <- function() ggblanket_global$mode

#' Get the theme
#' @description Get the currently set theme.
#' @noRd
get_theme <- function() ggblanket_global$theme

#' Get the discrete palette
#' @description Get the currently set discrete palette.
#' @noRd
get_col_palette_d <- function() ggblanket_global$col_palette_d

#' Get the continuous palette
#' @description Get the currently set continuous palette.
#' @noRd
get_col_palette_c <- function() ggblanket_global$col_palette_c

#' Get the ordinal palette
#' @description Get the currently set ordinal palette.
#' @noRd
get_col_palette_o <- function() ggblanket_global$col_palette_o

#' Get the discrete NA colour
#' @description Get the currently set discrete NA colour.
#' @noRd
get_col_palette_na_d <- function() ggblanket_global$col_palette_na_d

#' Get the continuous NA colour
#' @description Get the currently set continuous NA colour.
#' @noRd
get_col_palette_na_c <- function() ggblanket_global$col_palette_na_c

#' Get the ordinal NA colour
#' @description Get the currently set ordinal NA colour.
#' @noRd
get_col_palette_na_o <- function() ggblanket_global$col_palette_na_o

