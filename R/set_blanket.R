#' Set a style
#'
#' @description
#' Weave the style by setting:
#'
#' 1. the mode to be added with `gg_*()` side-effects
#' 2. the colour/fill geom default, and other defaults for text, reference line and curve geoms
#' 3. the ink_palettes for discrete, continuous and ordinal colour/fill scales
#' 4. a theme to be added _without_ `gg_*()` side-effects.
#'
#' Alternatively, use the `weave_*` functions to only apply a subset of these.
#' [ggplot2::update_geom_defaults()] can be used to further fine-tune geom defaults.
#'
#' @param ... Provided to force user argument naming etc.
#' @param mode A ggplot2 theme (e.g. [light_mode_t()] or [dark_mode_r()]) that anticipates `gg_*` side-effects of removing relevant axis line/ticks and gridlines per the `mode_orientation`.
#' @param ink A default hex colour for the colour and fill of geoms (other than text or reference line geoms).
#' @param text_ink A default hex colour for the colour (and fill) of the "text" and "label" geoms.
#' @param text_size A default size for the "text" and "label" geoms.
#' @param text_family A default family for the "text" and "label" geoms.
#' @param reference_line_ink A default hex colour for the colour of the "hline", "vline", "abline" and "curve" geoms.
#' @param reference_line_linewidth A default linewidth for the the "hline", "vline", "abline" and "curve" geoms.
#' @param ink_palette_d For a discrete scale, a character vector of hex codes.
#' @param ink_palette_c For a continuous scale, a character vector of hex codes.
#' @param ink_palette_o For an ordinal scale, a `scales::pal_*()` function.
#' @param ink_palette_na_d For a discrete scale, a hex code.
#' @param ink_palette_na_c For a continuous scale, a hex code.
#' @param ink_palette_na_o For an ordinal scale, a hex code.
#' @param theme A ggplot2 theme that the `gg_*` function will add without side-effects if the mode is set/weaved to `NULL` (and also is applied to ggplot code outside of ggblanket).
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
#'   ink = "#E7298AFF",
#'   text_ink = darkness[1],
#'   reference_line_ink = darkness[1],
#'   ink_palette_d = c("#1B9E77FF", "#D95F02FF", "#7570b3FF", "#E7298AFF",
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
#'     ink = species,
#'   ) +
#'   geom_vline(xintercept = 200) +
#'   annotate("text", x = I(0.75), y = I(0.75), label = "Here")
#'
set_blanket <- function(
    ...,
    mode = light_mode_r(),
    ink = "#357BA2FF",
    text_ink = "#121B24FF",
    text_size = 11 / 2.835052,
    text_family = "",
    reference_line_ink = "#121B24FF",
    reference_line_linewidth = 0.33,
    ink_palette_d = jumble,
    ink_palette_na_d = "#CDC5BFFF",
    ink_palette_c = viridisLite::mako(n = 9, direction = -1),
    ink_palette_na_c = "#988F88FF",
    ink_palette_o = scales::pal_viridis(option = "G", direction = -1),
    ink_palette_na_o = "#988F88FF",
    theme = light_mode_r() + mode_orientation_to_x()) {

  weave_mode(mode = mode)

  weave_geom_defaults(
    ink = ink,
    text_ink = text_ink,
    text_size = text_size,
    text_family = text_family,
    reference_line_ink = reference_line_ink,
    reference_line_linewidth = reference_line_linewidth
  )

  weave_ink_palettes(
    ink_palette_d = ink_palette_d,
    ink_palette_na_d = ink_palette_na_d,
    ink_palette_c = ink_palette_c,
    ink_palette_na_c = ink_palette_na_c,
    ink_palette_o = ink_palette_o,
    ink_palette_na_o = ink_palette_na_o
  )

  weave_theme(theme = theme)
}

