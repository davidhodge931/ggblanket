#' Set a style
#'
#' @description
#' Weave the style by setting:
#'
#' 1. the mode to be added with `gg_*()` side-effects.
#' 2. updated geom defaults
#' 3. col_palettes for discrete, continuous and ordinal colour/fill scales
#' 4. a theme to be added _without_ `gg_*()` side-effects.
#'
#' Alternatively, use the `weave_*` functions to only apply a subset of these.
#'
#' @param ... Provided to force user argument naming etc.
#' @param mode A ggplot2 theme (e.g. [light_mode_t()] or [dark_mode_r()]) that anticipates `gg_*` side-effects of removing relevant axis line/ticks and gridlines per the `mode_orientation`.
#' @param colour A default hex colour for the colour of geoms without a more specific `colour_*` argument.
#' @param colour_text A default hex colour for the colour of the "text" geom.
#' @param colour_label A default hex colour for the colour of the "label" geom.
#' @param colour_reference_line A default hex colour for the colour of the "hline", "vline" and "abline" geoms.
#' @param colour_curve A default hex colour for the colour of the "curve" geom.
#' @param fill A default hex colour for the fill of geoms without a more specific `fill_*` argument.
#' @param fill_label A default hex colour for the fill of the "label" geom.
#' @param linewidth_reference_line A default linewidth for the the "hline", "vline" and "abline" geoms.
#' @param linewidth_curve A default linewidth for the the "curve" geom.
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
#' @param theme A ggplot2 theme that (1). the `gg_*` function will add without side-effects if the mode is set/weaved to `NULL` - and (2) is applied to ggplot code outside of ggblanket.
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
#'   colour_text = darkness[1],
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
    linewidth_reference_line = 0.33,
    linewidth_curve = linewidth_reference_line,
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
    linewidth_reference_line = linewidth_reference_line,
    linewidth_curve = linewidth_curve,
    size_text = size_text,
    size_label = size_label,
    family_text = family_text,
    family_label = family_label)

  weave_col_palettes(
    col_palette_d = col_palette_d,
    col_palette_na_d = col_palette_na_d,
    col_palette_c = col_palette_c,
    col_palette_na_c = col_palette_na_c,
    col_palette_o = col_palette_o,
    col_palette_na_o = col_palette_na_o
  )

  weave_theme(theme = theme)
}

