#' Set the style
#'
#' @description
#' Set the style by setting:
#'
#' 1. the theme to be added by default
#' 2. the geom defaults (e.g. colour/fill), and text and reference line defaults
#' 3. the col_palettes for discrete, continuous and ordinal colour/fill scales
#'
#' Alternatively, use the `weave_*` functions to only apply a subset of these.
#' A `weave_theme` function is also provided to set a theme _without_ `gg_*` side-effects.
#' [ggplot2::update_geom_defaults()] can be used to further fine-tune geom defaults.
#'
#' @param ... Provided to force user argument naming etc.
#' @param theme A ggplot2 theme (e.g. [light_mode_t()] or [dark_mode_r()]) that anticipates `gg_*` side-effects of removing relevant axis line/ticks and gridlines per the `theme_orientation`.
#' @param colour A default hex colour for the colour of geoms (other than "text", "label", "hline", "vline" and "abline" geoms).
#' @param fill A default hex colour for the fill of geoms (other than "text", "label", "hline", "vline" and "abline" geoms).
#' @param text_colour A default hex colour for the colour of the "text" and "label" geoms.
#' @param text_size A default size for the "text" and "label" geoms.
#' @param text_family A default family for the "text" and "label" geoms.
#' @param reference_colour A default hex colour for the colour of the "hline", "vline" and "abline" geoms.
#' @param reference_linewidth A default hex colour for the colour of the "hline", "vline" and "abline" geoms.
#' @param col_palette_d For a discrete scale, a character vector of hex codes.
#' @param col_palette_c For a continuous scale, a character vector of hex codes.
#' @param col_palette_o For an ordinal scale, a `scales::pal_*()` function.
#' @param col_palette_na_d For a discrete scale, a hex code.
#' @param col_palette_na_c For a continuous scale, a hex code.
#' @param col_palette_na_o For an ordinal scale, a hex code.
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
#'   theme = dark_mode_r(),
#'   colour = "#E7298AFF",
#'   colour_text = darkness[1],
#'   colour_hline = darkness[1],
#'   colour_vline = darkness[1],
#'   col_palette_d = c("#1B9E77FF", "#D95F02FF", "#7570b3FF", "#E7298AFF",
#'                     "#66A61EFF", "#E6AB02FF", "#A6761DFF", "#666666FF"),
#' )
#'
#' penguins |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'   ) +
#'   geom_vline(xintercept = 200) +
#'   annotate("text", x = I(0.25), y = I(0.75), label = "Here")
#'
#' penguins |>
#'   gg_histogram(
#'     x = flipper_length_mm,
#'     col = species,
#'   ) +
#'   geom_vline(xintercept = 200) +
#'   annotate("text", x = I(0.75), y = I(0.75), label = "Here")
#'
set_blanket <- function(
    ...,
    theme = light_mode_r(),
    colour = "#357BA2FF",
    fill = colour,
    text_colour = "#121B24FF",
    text_size = 11 / 2.835052,
    text_family = "",
    reference_colour = "#121B24FF",
    reference_linewidth = 0.25,
    col_palette_d = jumble,
    col_palette_c = viridisLite::mako(n = 9, direction = -1),
    col_palette_o = scales::pal_viridis(option = "G", direction = -1),
    col_palette_na_d = "#CDC5BFFF",
    col_palette_na_c = "#988F88FF",
    col_palette_na_o = "#988F88FF") {

  weave_theme(theme = theme)

  weave_geom_defaults(
    colour = colour,
    fill = fill,
    text_colour = text_colour,
    text_size = text_size,
    text_family = text_family,
    reference_colour = reference_colour,
    reference_linewidth = reference_linewidth
  )

  weave_col_palettes(
    col_palette_d = col_palette_d,
    col_palette_c = col_palette_c,
    col_palette_o = col_palette_o,
    col_palette_na_d = col_palette_na_d,
    col_palette_na_c = col_palette_na_c,
    col_palette_na_o = col_palette_na_o
  )
}

