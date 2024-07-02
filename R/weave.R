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
#' @param new A ggplot2 theme (e.g. [light_mode_t()] or [dark_mode_r()]) that anticipates `gg_*` side-effects of removing relevant axis line/ticks and gridlines per the `mode_orientation`.
#'
#' @noRd
weave_mode <- function(new = light_mode_r()) {
  old <- ggblanket_global$mode
  ggblanket_global$mode <- new
  invisible(old)
}

#' Set a theme
#'
#' @description Set a theme to be `+`-ed on unmodified to `gg_*` functions. Note, `mode` takes precedence unless NULL.
#'
#' @param new A ggplot2 theme that the `gg_*` function will add without side-effects. Note, `mode` takes precedence, unless `mode = NULL`.
#'
#' @noRd
weave_theme <- function(new = light_mode_r() + mode_orientation_to_x()) {
  old <- ggblanket_global$theme
  ggblanket_global$theme <- new
  invisible(old)

  ggplot2::theme_set(new = new)
}

#' Set a series of geom defaults
#'
#' @description Update most geom defaults.
#'
#' @param colour A hex colour (and fill) for geoms. Fill inherits from this colour. Defaults to `blue` (i.e. `#357BA2FF`).
#'
#' @noRd
weave_geom_aes <- function(colour = "#357BA2FF") {

  if (!rlang::is_null(colour)) {
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
    ggplot2::update_geom_defaults("pointrange", ggplot2::aes(colour = !!colour, fill = !!colour, linewidth = 0.66, size = 0.2))
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
  }
  else {
    ggplot2::update_geom_defaults("area", NULL)
    ggplot2::update_geom_defaults("bar", NULL)
    ggplot2::update_geom_defaults("boxplot", NULL)
    ggplot2::update_geom_defaults("col", NULL)
    ggplot2::update_geom_defaults("contour", NULL)
    ggplot2::update_geom_defaults("contour_filled", NULL)
    ggplot2::update_geom_defaults("crossbar", NULL)
    ggplot2::update_geom_defaults("density", NULL)
    ggplot2::update_geom_defaults("density2d", NULL)
    ggplot2::update_geom_defaults("density_2d_filled", NULL)
    ggplot2::update_geom_defaults("errorbar", NULL)
    ggplot2::update_geom_defaults("function", NULL)
    ggplot2::update_geom_defaults("hex", NULL)
    ggplot2::update_geom_defaults("line", NULL)
    ggplot2::update_geom_defaults("linerange", NULL)
    ggplot2::update_geom_defaults("path", NULL)
    ggplot2::update_geom_defaults("point", NULL)
    ggplot2::update_geom_defaults("pointrange", NULL)
    ggplot2::update_geom_defaults("polygon", NULL)
    ggplot2::update_geom_defaults("quantile", NULL)
    ggplot2::update_geom_defaults("raster", NULL)
    ggplot2::update_geom_defaults("rect", NULL)
    ggplot2::update_geom_defaults("ribbon", NULL)
    ggplot2::update_geom_defaults("rug", NULL)
    ggplot2::update_geom_defaults("segment", NULL)
    ggplot2::update_geom_defaults("sf", NULL)
    ggplot2::update_geom_defaults("smooth", NULL)
    ggplot2::update_geom_defaults("spoke", NULL)
    ggplot2::update_geom_defaults("step", NULL)
    ggplot2::update_geom_defaults("violin", NULL)
    #to add and adjust once ggplot makes GeomBin2d
    ggplot2::update_geom_defaults("tile", NULL)
  }
}

#'  Set a series of annotation defaults
#'
#' @description Update other geom defaults commonly used for annotation (i.e. `*_vline`/`*_hline`/`*_abline`, `*_curve`, and `*_text`/`*_label`).
#'
#' @param colour A hex colour (and fill) for `*_hline`/`*_vline`/`*_abline` and `*_curve`. Defaults to "#121b24" (i.e. `lightness[1]`).
#' @param linewidth A linewidth for `*_hline`/`*_vline`/`*_abline` and `*_curve`. Defaults to 0.33.
#' @param family A family for `*_text` and `*_label`. Defaults to ""
#' @param size A size for `*_text` and `*_label`. Defaults to 3.88.
#'
#' @noRd
weave_annotate_aes <- function(colour = "#121B24FF", linewidth = 0.33, family = "", size = 3.88) {
  if (!rlang::is_null(colour)) {
    ggplot2::update_geom_defaults("abline", ggplot2::aes(colour = !!colour, linewidth = !!linewidth))
    ggplot2::update_geom_defaults("hline", ggplot2::aes(colour = !!colour, linewidth = !!linewidth))
    ggplot2::update_geom_defaults("vline", ggplot2::aes(colour = !!colour, linewidth = !!linewidth))
    ggplot2::update_geom_defaults("curve", ggplot2::aes(colour = !!colour, linewidth = !!linewidth))
    ggplot2::update_geom_defaults("text", ggplot2::aes(colour = !!colour, size = !!size, family = !!family))
    ggplot2::update_geom_defaults("label", ggplot2::aes(colour = !!colour, fill = !!colour, alpha = 0.05, size = !!size, family = !!family))
  }
  else {
    ggplot2::update_geom_defaults("abline", NULL)
    ggplot2::update_geom_defaults("hline", NULL)
    ggplot2::update_geom_defaults("vline", NULL)
    ggplot2::update_geom_defaults("curve", NULL)
    ggplot2::update_geom_defaults("text", NULL)
    ggplot2::update_geom_defaults("label", NULL)
  }
}

#' Set a discrete colour palette
#'
#' @param new For a discrete scale, a character vector of hex codes (or names) for the `col_palette`.
#' @param na For a discrete scale, a hex code (or name) for the `col_palette_na`.
#'
#' @noRd
weave_col_palette_d <- function(new = jumble, na = "#CDC5BFFF") {

  if (rlang::is_null(na)) na <- "grey50"

  if (!rlang::is_null(new) & !rlang::is_function(new)) new <- c(new, rep(na, times = 100))

  old <- ggblanket_global$col_palette_d
  ggblanket_global$col_palette_d <- new
  invisible(old)

  old <- ggblanket_global$col_palette_na_d
  ggblanket_global$col_palette_na_d <- na
  invisible(old)

  if (rlang::is_null(new)) {
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
          values = new,
          na.value = na
        ),
      ggplot2.discrete.fill = function()
        ggplot2::scale_fill_manual(
          values = new,
          na.value = na
        )
    )
  }
}

#' Set a continuous colour palette
#'
#' @param new For a continuous scale, a character vector of hex codes (or names)
#' @param na For a continuous scale, a hex code (or name) for the `col_palette_na`.
#'
#' @noRd
weave_col_palette_c <- function(new = viridisLite::mako(n = 9, direction = -1),
                                na = "#988F88FF") { # i.e. colorspace::darken(grey, 0.25)

  if (rlang::is_null(new)) {
    new <- scales::pal_seq_gradient(low = "#132B43", high = "#56B1F7")(seq(0, 1, length.out = 20))
  }

  if (rlang::is_null(na)) {
    na <- "grey50"
  }

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

#' Set an ordinal colour palette
#'
#' @param new For an ordinal scale, a `scales::pal_*()` function for the `col_palette`.
#' @param na For an ordinal scale, a hex code (or name) for the `col_palette_na`.
#'
#' @noRd
weave_col_palette_o <- function(new = scales::pal_viridis(option = "G", direction = -1),
                                na = "#988F88FF") {

  if (rlang::is_null(new)) new <- scales::pal_viridis()
  if (rlang::is_null(na)) na <- "grey50"

  old <- ggblanket_global$col_palette_o
  ggblanket_global$col_palette_o <- new
  invisible(old)

  old <- ggblanket_global$col_palette_na_o
  ggblanket_global$col_palette_na_o <- na
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

