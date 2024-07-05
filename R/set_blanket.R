#' Set a style
#'
#' @description
#' Set a style by setting a:
#'
#' * mode
#' * geom defaults, including for annotation
#' * col_palettes for discrete, continuous and ordinal scales.
#'
#' Alternatively, use the `weave_*` functions to only apply a subset of these.
#'
#' @param ... Provided to force user argument naming etc.
#' @param mode A ggplot2 theme (e.g. [light_mode_t()] or [dark_mode_r()]) that anticipates `gg_*` side-effects of removing relevant axis line/ticks and gridlines per the `mode_orientation`.
#' @param colour A hex colour for the colour of geoms other than `*_hline`/`*_vline`/`*_abline` and `*_curve`. Defaults to `blue` (i.e. `#357BA2FF`).
#' @param fill A hex colour for the fill of geoms `*_hline`/`*_vline`/`*_abline` and `*_curve`. Defaults to `colour`.
#' @param annotation_colour A hex annotation colour for the colour of`*_hline`/`*_vline`/`*_abline` and `*_curve`. Defaults to "#121b24" (i.e. `lightness[1]`).
#' @param annotation_fill A hex annotation colour for the colour of `*_hline`/`*_vline`/`*_abline` and `*_curve`. Defaults to `annotation_colour`.
#' @param annotation_linewidth A linewidth for `*_hline`/`*_vline`/`*_abline` and `*_curve`. Defaults to 0.33.
#' @param annotation_family A family for `*_text` and `*_label`. Defaults to "".
#' @param annotation_size A size for `*_text` and `*_label`. Defaults to 3.88.
#' @param col_palette_d For a discrete scale, a character vector of hex codes.
#' @param col_palette_c For a continuous scale, a character vector of hex codes.
#' @param col_palette_o For an ordinal scale, a `scales::pal_*()` function.
#' @param col_palette_na_d For a discrete scale, a hex code.
#' @param col_palette_na_c For a continuous scale, a hex code.
#' @param col_palette_na_o For an ordinal scale, a hex code.
#' @param theme A ggplot2 theme that the `gg_*` function will add without side-effects. Note, `mode` takes precedence, unless `mode = NULL`.
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
    fill = colour,
    annotation_colour = "#121B24FF",
    annotation_fill = annotation_colour,
    annotation_linewidth = 0.33,
    annotation_family = "",
    annotation_size = 3.88,
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
    fill = fill,
    annotation_colour = annotation_colour,
    annotation_fill = annotation_fill,
    annotation_linewidth = annotation_linewidth,
    annotation_size = annotation_size,
    annotation_family = annotation_family
  )

  weave_col_palette_d(
    col_palette_d = col_palette_d,
    col_palette_d_na = col_palette_na_d
  )

  weave_col_palette_o(
    col_palette_o = col_palette_o,
    col_palette_o_na = col_palette_na_o
  )

  weave_col_palette_c(
    col_palette_c = col_palette_c,
    col_palette_c_na = col_palette_na_c
  )

  weave_theme(theme = theme)
}

