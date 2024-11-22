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

#' Set a series of geom defaults
#'
#' @description Update the colour/fill geom default, and update other defaults for text and reference line geoms.
#'
#' [ggplot2::update_geom_defaults()] can be used to further fine-tune geom defaults.
#'
#' @param colour A default hex colour for the colour of geoms (other than text or reference line geoms).
#' @param fill A default hex colour for the fill of geoms (other than text or reference line geoms).
#' @param text_colour A default hex colour for the colour (and fill) of the "text" and "label" geoms.
#' @param text_size A default size for the "text" and "label" geoms.
#' @param text_family A default family for the "text" and "label" geoms.
#' @param reference_line_colour A default hex colour for the colour of the "hline", "vline", "abline" and "curve" geoms.
#' @param reference_line_linewidth A default linewidth for the the "hline", "vline", "abline" and "curve" geoms.
#'
#' @export
weave_geom_defaults <- function(
    colour = "#357BA2FF",
    fill = colour,
    text_colour = "#121B24FF",
    text_size = 11 / 2.835052,
    text_family = "",
    reference_line_colour = "#121B24FF",
    reference_line_linewidth = 0.33) {

  ggplot2::update_geom_defaults("area", ggplot2::aes(colour = !!colour, fill = !!fill, linewidth = 0))
  ggplot2::update_geom_defaults("bar", ggplot2::aes(colour = !!colour, fill = !!fill, linewidth = 0))
  ggplot2::update_geom_defaults("col", ggplot2::aes(colour = !!colour, fill = !!fill, linewidth = 0.66))
  ggplot2::update_geom_defaults("contour", ggplot2::aes(colour = !!colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("contour_filled", ggplot2::aes(fill = !!fill, linewidth = 0.66))
  ggplot2::update_geom_defaults("density", ggplot2::aes(colour = !!colour, fill = !!fill, linewidth = 0))
  ggplot2::update_geom_defaults("density2d", ggplot2::aes(colour = !!colour, linewidth = !!0.66))
  ggplot2::update_geom_defaults("density_2d_filled", ggplot2::aes(fill = !!fill, linewidth = 0))
  ggplot2::update_geom_defaults("errorbar", ggplot2::aes(colour = !!colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("function", ggplot2::aes(colour = !!colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("hex", ggplot2::aes(fill = !!fill))
  ggplot2::update_geom_defaults("line", ggplot2::aes(colour = !!colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("linerange", ggplot2::aes(colour = !!colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("path", ggplot2::aes(colour = !!colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("point", ggplot2::aes(colour = !!colour, fill = !!fill))
  ggplot2::update_geom_defaults("pointrange", ggplot2::aes(colour = !!colour, fill = !!fill, linewidth = 0.66, size = 0.2)) # 1.5 / 7.5
  ggplot2::update_geom_defaults("polygon", ggplot2::aes(colour = !!colour, fill = !!fill, linewidth = 0))
  ggplot2::update_geom_defaults("quantile", ggplot2::aes(colour = !!colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("raster", ggplot2::aes(fill = !!fill))
  ggplot2::update_geom_defaults("rect", ggplot2::aes(colour = !!colour, fill = !!fill, linewidth = 0))
  ggplot2::update_geom_defaults("ribbon", ggplot2::aes(colour = !!colour, fill = !!fill, alpha = 0.4, linewidth = 0))
  ggplot2::update_geom_defaults("rug", ggplot2::aes(colour = !!colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("segment", ggplot2::aes(colour = !!colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("sf", ggplot2::aes(colour = !!colour, fill = !!fill, linewidth = 0))
  ggplot2::update_geom_defaults("smooth", ggplot2::aes(colour = !!colour, fill = !!fill, linewidth = 0.66))
  ggplot2::update_geom_defaults("spoke", ggplot2::aes(colour = !!colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("step", ggplot2::aes(colour = !!colour, linewidth = 0.66))
  ggplot2::update_geom_defaults("violin", ggplot2::aes(colour = !!colour, fill = !!fill, linewidth = 0))

  # ggplot2::update_geom_defaults("bin2d", ggplot2::aes(colour = NA, fill = !!fill, linewidth = 0))
  # ggplot2::update_geom_defaults("tile", ggplot2::aes(colour = !!colour, fill = !!fill, linewidth = 0))
  ggplot2::update_geom_defaults("tile", ggplot2::aes(colour = NA, fill = !!fill, linewidth = 0))

  ggplot2::update_geom_defaults("boxplot", ggplot2::aes(colour = !!colour, fill = !!fill, linewidth = 0.66))
  ggplot2::update_geom_defaults("crossbar", ggplot2::aes(colour = !!colour, fill = !!fill, linewidth = 0.66))

  ggplot2::update_geom_defaults("abline", ggplot2::aes(colour = !!reference_line_colour, linewidth = !!reference_line_linewidth))
  ggplot2::update_geom_defaults("hline", ggplot2::aes(colour = !!reference_line_colour, linewidth = !!reference_line_linewidth))
  ggplot2::update_geom_defaults("vline", ggplot2::aes(colour = !!reference_line_colour, linewidth = !!reference_line_linewidth))
  ggplot2::update_geom_defaults("curve", ggplot2::aes(colour = !!reference_line_colour, linewidth = !!reference_line_linewidth))
  ggplot2::update_geom_defaults("text", ggplot2::aes(colour = !!text_colour, size = !!text_size, family = !!text_family))
  ggplot2::update_geom_defaults("label", ggplot2::aes(colour = !!text_colour, fill = !!text_colour, alpha = 0.05, size = !!text_size, family = !!text_family))
}

#' Set a discrete colour and fill palettes
#'
#' @param col_palette_d For a discrete scale, a character vector of hex codes. Use NULL for ggplot2 default.
#' @param col_palette_c For a continuous scale, a character vector of hex codes. Use NULL for ggplot2 default.
#' @param col_palette_o For an ordinal scale, a `scales::pal_*()` function. Use NULL for ggplot2 default.
#' @param col_palette_na_d For a discrete scale, a hex code.
#' @param col_palette_na_c For a continuous scale, a hex code.
#' @param col_palette_na_o For an ordinal scale, a hex code.
#' @param ... Dots to support trailing commas etc.
#'
#' @export
weave_col_palettes <- function(
    col_palette_d = jumble,
    col_palette_c = viridisLite::mako(n = 9, direction = -1),
    col_palette_o = scales::pal_viridis(option = "G", direction = -1),
    col_palette_na_d = "#CDC5BFFF",
    col_palette_na_c = "#988F88FF", # i.e. colorspace::darken(grey, 0.25)
    col_palette_na_o = "#988F88FF",
    ...) {
  weave_col_palette_d(col_palette_d = col_palette_d, col_palette_na_d = col_palette_na_d)
  weave_col_palette_c(col_palette_c = col_palette_c, col_palette_na_c = col_palette_na_c)
  weave_col_palette_o(col_palette_o = col_palette_o, col_palette_na_o = col_palette_na_o)
}

#' Set a discrete colour and fill palette
#'
#' @param col_palette_d For a discrete scale, a character vector of hex codes. Use NULL for ggplot2 default.
#' @param col_palette_na_d For a discrete scale, a hex code.
#' @param ... Dots to support trailing commas etc.
#'
#' @noRd
weave_col_palette_d <- function(col_palette_d = jumble, col_palette_na_d = "#CDC5BFFF", ...) {

  if (rlang::is_null(col_palette_na_d)) col_palette_na_d <- "grey50"

  if (!rlang::is_null(col_palette_d)) {
    if (!rlang::is_function(col_palette_d)) {
      col_palette_d <- c(col_palette_d, rep(col_palette_na_d, times = 100))
    }
  }

  old <- ggblanket_global$col_palette_d
  ggblanket_global$col_palette_d <- col_palette_d
  invisible(old)

  old <- ggblanket_global$col_palette_na_d
  ggblanket_global$col_palette_na_d <- col_palette_na_d
  invisible(old)

  if (rlang::is_null(col_palette_d)) {
    options(
      ggplot2.discrete.colour = function(...)
        ggplot2::scale_colour_hue(...),
      ggplot2.discrete.fill = function(...)
        ggplot2::scale_fill_hue(...)
    )
  }
  else {
    options(
      ggplot2.discrete.colour = function(...)
        ggplot2::scale_colour_manual(
          values = col_palette_d,
          na.value = col_palette_na_d,
          ...
        ),
      ggplot2.discrete.fill = function(...)
        ggplot2::scale_fill_manual(
          values = col_palette_d,
          na.value = col_palette_na_d,
          ...
        )
    )
  }
}

#' Set a continuous colour and fill palette
#'
#' @param col_palette_c For a continuous scale, a character vector of hex codes. Use NULL for ggplot2 default.
#' @param col_palette_na_c For a continuous scale, a hex code.
#' @param ... Dots to support trailing commas etc.
#'
#' @noRd
weave_col_palette_c <- function(col_palette_c = viridisLite::mako(n = 9, direction = -1),
                                col_palette_na_c = "#988F88FF", # i.e. colorspace::darken(grey, 0.25)
                                ...) {

  if (rlang::is_null(col_palette_c)) {
    col_palette_c <- scales::pal_seq_gradient(low = "#132B43", high = "#56B1F7")(seq(0, 1, length.out = 20))
  }

  if (rlang::is_null(col_palette_na_c)) {
    col_palette_na_c <- "grey50"
  }

  old <- ggblanket_global$col_palette_c
  ggblanket_global$col_palette_c <- col_palette_c
  invisible(old)

  old <- ggblanket_global$col_palette_na_c
  ggblanket_global$col_palette_na_c <- col_palette_na_c
  invisible(old)

  options(
    ggplot2.continuous.colour = function(...)
      ggplot2::scale_color_gradientn(
        colours = col_palette_c,
        na.value = col_palette_na_c,
        ...
      ),
    ggplot2.continuous.fill = function(...)
      ggplot2::scale_fill_gradientn(
        colours = col_palette_c,
        na.value = col_palette_na_c,
        ...
      )
  )
}

#' Set an ordinal colour and fill palette
#'
#' @param col_palette_o For an ordinal scale, a `scales::pal_*()` function. Use NULL for ggplot2 default.
#' @param col_palette_na_o For an ordinal scale, a hex code.
#'
#' @noRd
weave_col_palette_o <- function(col_palette_o = scales::pal_viridis(option = "G", direction = -1),
                                col_palette_na_o = "#988F88FF") {

  if (rlang::is_null(col_palette_o)) col_palette_o <- scales::pal_viridis()
  if (rlang::is_null(col_palette_na_o)) col_palette_na_o <- "grey50"

  old <- ggblanket_global$col_palette_o
  ggblanket_global$col_palette_o <- col_palette_o
  invisible(old)

  old <- ggblanket_global$col_palette_na_o
  ggblanket_global$col_palette_na_o <- col_palette_na_o
  invisible(old)
}

#' Set a theme (without side-effects)
#'
#' @description Set a theme to be `+`-ed on unmodified to `gg_*` functions. Note, the `mode` takes precedence, unless the set/weaved mode is `mode = NULL`.
#'
#' @param theme A ggplot2 theme that the `gg_*` function will add without side-effects if the mode is set/weaved to `NULL` (and also is applied to ggplot code outside of ggblanket).
#'
#' @export
weave_theme <- function(theme = light_mode_r() + mode_orientation_to_x()) {
  old <- ggblanket_global$theme
  ggblanket_global$theme <- theme
  invisible(old)

  if (rlang::is_null(theme)) {
    ggplot2::theme_set(new = ggplot2::theme_grey())
  }
  else {
    ggplot2::theme_set(new = theme)
  }
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

