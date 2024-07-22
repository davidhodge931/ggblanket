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
#' @description Update all geom defaults.
#'
#' @param colour A default hex colour for the colour of geoms.
#' @param colour_text A default hex colour for the colour of the "text" geom.
#' @param colour_label A default hex colour for the colour of the "label" geom.
#' @param colour_reference_line A default hex colour for the colour of the "hline", "vline" and "abline" geoms.
#' @param colour_curve A default hex colour for the colour of the "curve" geom.
#' @param fill A default hex colour for the fill of geoms.
#' @param fill_label A default hex colour for the fill of the "label" geom.
#' @param alpha A default alpha for geoms.
#' @param alpha_area A default alpha for the "area" geom.
#' @param alpha_bar A default alpha for the "bar" geom.
#' @param alpha_boxplot A default alpha for the "boxplot" geom.
#' @param alpha_crossbar A default alpha for the "crossbar" geom.
#' @param alpha_density A default alpha for the "density" geom.
#' @param alpha_label A default alpha for the "label" geom.
#' @param alpha_polygon A default alpha for the "polygon" geom.
#' @param alpha_rect A default alpha for the "rect" geom.
#' @param alpha_ribbon A default alpha for the "ribbon" geom.
#' @param alpha_smooth A default alpha for the "smooth" geom.
#' @param alpha_tile A default alpha for the "tile" geom.
#' @param alpha_violin A default alpha for the "violin" geom.
#' @param alpha_recursive A default alpha applied to all geoms.
#' @param linewidth A default linewidth for geoms.
#' @param linewidth_reference_line A default linewidth for the the "hline", "vline" and "abline" geoms.
#' @param linewidth_curve A default linewidth for the the "curve" geom.
#' @param size_point A default size for the "point" geom.
#' @param size_pointrange A default size for the "pointrange" geom.
#' @param size_sf A default size for the "sf" geom.
#' @param size_text A default size for the "text" geom.
#' @param size_label A default size for the "label" geom.
#' @param family_text A default family for the "text" geom.
#' @param family_label A default family for the "text" geom.
#'
#' @export
weave_geom_defaults <- function(colour = "#357BA2FF",
                                colour_text = "#121B24FF",
                                colour_label = colour_text,
                                colour_reference_line = colour_text,
                                colour_curve = colour_reference_line,

                                fill = colour,
                                fill_label = colour_label,

                                alpha = 1,
                                alpha_area = 0.9,
                                alpha_bar = 0.9,
                                alpha_boxplot = 0.6,
                                alpha_crossbar = 0.6,
                                alpha_density = 0.6,
                                alpha_label = 0.05,
                                alpha_polygon = 0.9,
                                alpha_rect = 0.9,
                                alpha_ribbon = 0.6,
                                alpha_smooth = 0.6,
                                alpha_tile = 0.9,
                                alpha_violin = 0.9,
                                alpha_recursive = NULL,

                                linewidth = 0.66,
                                linewidth_reference_line = 0.33,
                                linewidth_curve = linewidth_reference_line,

                                size_point = 1.5,
                                size_pointrange = 0.2, # 1.5 / 7.5
                                size_sf = 1.5,
                                size_text = 11 / 2.835052,
                                size_label = size_text,

                                family_text = "",
                                family_label = family_text) {

  if (!rlang::is_null(alpha_recursive)) {
    alpha <- alpha_recursive
    alpha_area <- alpha_recursive
    alpha_bar <- alpha_recursive
    alpha_boxplot <- alpha_recursive
    alpha_crossbar <- alpha_recursive
    alpha_density <- alpha_recursive
    alpha_label <- alpha_recursive
    alpha_polygon <- alpha_recursive
    alpha_rect <- alpha_recursive
    alpha_ribbon <- alpha_recursive
    alpha_smooth <- alpha_recursive
    alpha_tile <- alpha_recursive
    alpha_violin <- alpha_recursive
  }

  ggplot2::update_geom_defaults("area", ggplot2::aes(colour = !!colour, fill = !!fill, alpha = !!alpha_area, linewidth = !!linewidth))
  ggplot2::update_geom_defaults("bar", ggplot2::aes(colour = !!colour, fill = !!fill, alpha = !!alpha_bar, linewidth = !!linewidth))
  ggplot2::update_geom_defaults("boxplot", ggplot2::aes(colour = !!colour, fill = !!fill, alpha = !!alpha_boxplot, linewidth = !!linewidth))
  ggplot2::update_geom_defaults("col", ggplot2::aes(colour = !!colour, fill = !!fill, alpha = !!alpha_bar, linewidth = !!linewidth))
  ggplot2::update_geom_defaults("contour", ggplot2::aes(colour = !!colour, linewidth = !!linewidth, alpha = !!alpha))
  ggplot2::update_geom_defaults("contour_filled", ggplot2::aes(fill = !!fill, alpha = !!alpha, linewidth = !!linewidth))
  ggplot2::update_geom_defaults("crossbar", ggplot2::aes(colour = !!colour, fill = !!fill, alpha = !!alpha_crossbar, linewidth = !!linewidth))
  ggplot2::update_geom_defaults("density", ggplot2::aes(colour = !!colour, fill = !!fill, alpha = !!alpha_density, linewidth = !!linewidth))
  ggplot2::update_geom_defaults("density2d", ggplot2::aes(colour = !!colour, linewidth = !!linewidth, alpha = !!alpha))
  ggplot2::update_geom_defaults("density_2d_filled", ggplot2::aes(fill = !!fill, alpha = !!alpha, linewidth = !!linewidth))
  ggplot2::update_geom_defaults("errorbar", ggplot2::aes(colour = !!colour, linewidth = !!linewidth, alpha = !!alpha))
  ggplot2::update_geom_defaults("function", ggplot2::aes(colour = !!colour, linewidth = !!linewidth, alpha = !!alpha))
  ggplot2::update_geom_defaults("hex", ggplot2::aes(fill = !!fill, alpha = !!alpha))
  ggplot2::update_geom_defaults("line", ggplot2::aes(colour = !!colour, linewidth = !!linewidth, alpha = !!alpha))
  ggplot2::update_geom_defaults("linerange", ggplot2::aes(colour = !!colour, linewidth = !!linewidth, alpha = !!alpha))
  ggplot2::update_geom_defaults("path", ggplot2::aes(colour = !!colour, linewidth = !!linewidth, alpha = !!alpha))
  ggplot2::update_geom_defaults("point", ggplot2::aes(colour = !!colour, fill = !!fill, size = !!size_point, alpha = !!alpha))
  ggplot2::update_geom_defaults("pointrange", ggplot2::aes(colour = !!colour, fill = !!fill, alpha = !!alpha, linewidth = !!linewidth, size = !!size_pointrange))
  ggplot2::update_geom_defaults("polygon", ggplot2::aes(colour = !!colour, fill = !!fill, alpha = !!alpha_polygon, linewidth = !!linewidth))
  ggplot2::update_geom_defaults("quantile", ggplot2::aes(colour = !!colour, linewidth = !!linewidth, alpha = !!alpha))
  ggplot2::update_geom_defaults("raster", ggplot2::aes(fill = !!fill, alpha = !!alpha))
  ggplot2::update_geom_defaults("rect", ggplot2::aes(colour = !!colour, fill = !!fill, alpha = !!alpha_rect, linewidth = !!linewidth))
  ggplot2::update_geom_defaults("ribbon", ggplot2::aes(colour = !!colour, fill = !!fill, alpha = !!alpha_ribbon, linewidth = !!linewidth))
  ggplot2::update_geom_defaults("rug", ggplot2::aes(colour = !!colour, linewidth = !!linewidth, alpha = !!alpha))
  ggplot2::update_geom_defaults("segment", ggplot2::aes(colour = !!colour, linewidth = !!linewidth, alpha = !!alpha))
  ggplot2::update_geom_defaults("sf", ggplot2::aes(colour = !!colour, fill = !!fill, alpha = !!alpha, linewidth = !!linewidth, size = !!size_sf))
  ggplot2::update_geom_defaults("smooth", ggplot2::aes(colour = !!colour, fill = !!fill, alpha = !!alpha_smooth, linewidth = !!linewidth))
  ggplot2::update_geom_defaults("spoke", ggplot2::aes(colour = !!colour, linewidth = !!linewidth, alpha = !!alpha))
  ggplot2::update_geom_defaults("step", ggplot2::aes(colour = !!colour, linewidth = !!linewidth, alpha = !!alpha))
  ggplot2::update_geom_defaults("violin", ggplot2::aes(colour = !!colour, fill = !!fill, alpha = !!alpha_violin, linewidth = !!linewidth))
  #adjust once ggplot makes GeomBin2d to make colour = !!colour
  ggplot2::update_geom_defaults("tile", ggplot2::aes(colour = NA, fill = !!fill, alpha = !!alpha_tile, linewidth = !!linewidth))

  ggplot2::update_geom_defaults("abline", ggplot2::aes(colour = !!colour_reference_line, alpha = !!alpha, linewidth = !!linewidth_reference_line))
  ggplot2::update_geom_defaults("hline", ggplot2::aes(colour = !!colour_reference_line, alpha = !!alpha, linewidth = !!linewidth_reference_line))
  ggplot2::update_geom_defaults("vline", ggplot2::aes(colour = !!colour_reference_line, alpha = !!alpha, linewidth = !!linewidth_reference_line))
  ggplot2::update_geom_defaults("curve", ggplot2::aes(colour = !!colour_curve, alpha = !!alpha, linewidth = !!linewidth_curve))
  ggplot2::update_geom_defaults("text", ggplot2::aes(colour = !!colour_text, alpha = !!alpha, size = !!size_text, family = !!family_text))
  ggplot2::update_geom_defaults("label", ggplot2::aes(colour = !!colour_label, fill = !!fill_label, alpha = !!alpha_label, size = !!size_label, family = !!family_label))
}

#' Set a discrete colour palette
#'
#' @param col_palette_d For a discrete scale, a character vector of hex codes. Use NULL for ggplot2 default.
#' @param col_palette_d_na For a discrete scale, a hex code.
#' @param ... Dots to support trailing commas etc.
#'
#' @export
weave_col_palette_d <- function(col_palette_d = jumble, col_palette_d_na = "#CDC5BFFF", ...) {

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
          na.value = col_palette_d_na,
          ...
        ),
      ggplot2.discrete.fill = function(...)
        ggplot2::scale_fill_manual(
          values = col_palette_d,
          na.value = col_palette_d_na,
          ...
        )
    )
  }
}

#' Set a continuous colour palette
#'
#' @param col_palette_c For a continuous scale, a character vector of hex codes. Use NULL for ggplot2 default.
#' @param col_palette_c_na For a continuous scale, a hex code.
#' @param ... Dots to support trailing commas etc.
#'
#' @export
weave_col_palette_c <- function(col_palette_c = viridisLite::mako(n = 9, direction = -1),
                                col_palette_c_na = "#988F88FF", # i.e. colorspace::darken(grey, 0.25)
                                ...) {

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
    ggplot2.continuous.colour = function(...)
      ggplot2::scale_color_gradientn(
        colours = col_palette_c,
        na.value = col_palette_c_na,
        ...
      ),
    ggplot2.continuous.fill = function(...)
      ggplot2::scale_fill_gradientn(
        colours = col_palette_c,
        na.value = col_palette_c_na,
        ...
      )
  )
}

#' Set an ordinal colour palette
#'
#' @param col_palette_o For an ordinal scale, a `scales::pal_*()` function. Use NULL for ggplot2 default.
#' @param col_palette_o_na For an ordinal scale, a hex code.
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

#' Set a theme (without side-effects)
#'
#' @description Set a theme to be `+`-ed on unmodified to `gg_*` functions. Note, the `mode` takes precedence, unless `mode = NULL`.
#'
#' @param theme A ggplot2 theme that the `gg_*` function will add without side-effects. Use NULL for ggplot2 default.
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

