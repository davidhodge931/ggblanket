#' Set a style
#'
#' @description
#' Set a style by setting:
#'
#' * the mode
#' * geom defaults
#' * col_palettes for discrete, continuous and ordinal scales.
#'
#' Alternatively, use the `weave_*` functions to only apply a subset of these.
#'
#' @param ... Provided to force user argument naming etc.
#' @param mode A ggplot2 theme (e.g. [light_mode_t()] or [dark_mode_r()]) that anticipates `gg_*` side-effects of removing relevant axis line/ticks and gridlines per the `mode_orientation`.
#' @param colour A default hex colour for the colour of geoms.
#' @param colour_text A default hex colour for the colour of the "text" geom.
#' @param colour_label A default hex colour for the colour of the "label" geom.
#' @param colour_reference_line A default hex colour for the colour of the "hline", "vline" and "abline" geoms.
#' @param colour_curve A default hex colour for the colour of the "curve" geom.
#' @param fill A default hex colour for the fill of geoms.
#' @param fill_label A default hex colour for the fill of the "label" geom.
#' @param linewidth A default linewidth for geoms.
#' @param linewidth_reference_line A default linewidth for the the "hline", "vline" and "abline" geoms.
#' @param linewidth_curve A default linewidth for the the "curve" geom.
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
#' @param size_point A default size for the "point" geom.
#' @param size_pointrange A default size for the "pointrange" geom.
#' @param size_sf A default size for the "sf" geom.
#' @param size_text A default size for the "text" geom.
#' @param size_label A default size for the "label" geom.
#' @param family_text A default family for the "text" geom.
#' @param family_label A default family for the "text" geom.
#' @param col_palette_d For a discrete scale, a character vector of hex codes.
#' @param col_palette_c For a continuous scale, a character vector of hex codes.
#' @param col_palette_o For an ordinal scale, a `scales::pal_*()` function.
#' @param col_palette_na_d For a discrete scale, a hex code.
#' @param col_palette_na_c For a continuous scale, a hex code.
#' @param col_palette_na_o For an ordinal scale, a hex code.
#' @param theme A ggplot2 theme that the `gg_*` function will add without side-effects. Note, the `mode` takes precedence, unless `mode = NULL`.
#'
#' @return A globally set style.
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(ggblanket)
#' library(palmerpenguins)
#'
#' set_blanket(
#'   mode = dark_mode_r(),
#'   colour = orange,
#'   annotation_colour = darkness[1],
#' )
#'
#' penguins |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     x_breaks = scales::breaks_pretty(3),
#'   ) +
#'   geom_vline(xintercept = 200) +
#'   annotate("text", x = I(0.25), y = I(0.75), label = "Here")
#'
#' penguins |>
#'   gg_histogram(
#'     x = flipper_length_mm,
#'     x_breaks = scales::breaks_pretty(3),
#'   ) +
#'   geom_vline(xintercept = 200) +
#'   annotate("text", x = I(0.75), y = I(0.75), label = "Here")
#'
set_blanket <- function(
    ...,
    mode = light_mode_r(),

    colour = "#357BA2FF",
    colour_text = "#121B24FF",
    colour_label = colour_text,
    colour_reference_line = colour_text,
    colour_curve = colour_reference_line,

    fill = colour,
    fill_label = colour_label,

    linewidth = 0.66,
    linewidth_reference_line = 0.33,
    linewidth_curve = linewidth_reference_line,

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

    size_point = 1.5,
    size_pointrange = 0.2, # 1.5 / 7.5
    size_sf = 1.5,
    size_text = 11 / 2.835052,
    size_label = size_text,

    family_text = "",
    family_label = family_text,

    col_palette_d = jumble,
    col_palette_na_d = "#CDC5BFFF",
    col_palette_c = viridisLite::mako(n = 9, direction = -1),
    col_palette_na_c = "#988F88FF",
    col_palette_o = scales::pal_viridis(option = "G", direction = -1),
    col_palette_na_o = "#988F88FF",

    theme = light_mode_r() + mode_orientation_to_x()) {

  weave_mode(mode = mode)

  weave_geom_defaults(
    colour = colour,
    colour_text = colour_text,
    colour_label = colour_label,
    colour_reference_line = colour_reference_line,
    colour_curve = colour_curve,

    fill = fill,
    fill_label = fill_label,

    linewidth = linewidth,
    linewidth_reference_line = linewidth_reference_line,
    linewidth_curve = linewidth_curve,

    alpha_area = alpha_area,
    alpha_bar = alpha_bar,
    alpha_boxplot = alpha_boxplot,
    alpha_crossbar = alpha_crossbar,
    alpha_density = alpha_density,
    alpha_label = alpha_label,
    alpha_polygon = alpha_polygon,
    alpha_rect = alpha_rect,
    alpha_ribbon = alpha_ribbon,
    alpha_smooth = alpha_smooth,
    alpha_tile = alpha_tile,
    alpha_violin = alpha_violin,

    size_point = size_point,
    size_pointrange = size_pointrange,
    size_sf = size_sf,
    size_text = size_text,
    size_label = size_label,

    family_text = family_text,
    family_label = family_label)

  weave_col_palette_d(
    col_palette_d = col_palette_d,
    col_palette_d_na = col_palette_na_d,
    ...
  )

  weave_col_palette_c(
    col_palette_c = col_palette_c,
    col_palette_c_na = col_palette_na_c,
    ...
  )

  weave_col_palette_o(
    col_palette_o = col_palette_o,
    col_palette_o_na = col_palette_na_o
  )

  weave_theme(theme = theme)
}

