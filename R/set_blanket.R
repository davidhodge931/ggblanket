#' Set the style
#'
#' @description
#' Set the style by setting:
#'
#' 1. the theme, and how/what side-effects are to be applied
#' 2. the geom defaults, including those for text and reference lines
#' 3. the colour palettes (i.e. discrete, continuous and ordinal)
#' 4. and more
#'
#' Alternatively, use the `weave_*` functions to only apply a subset of these.
#'
#' [ggplot2::update_geom_defaults()] can be used to further fine-tune geom defaults.
#'
#' @param ... Provided to require argument naming, support trailing commas etc.
#' @param theme A ggplot2 theme (e.g. [light_mode_t()] or [dark_mode_r()]).
#' @param theme_orientation The orientation of plot, which affects the theme components that can be removed by the `gg_*` function. Either `"x"` or `"y"`. Defaults to `NULL`, which lets the `gg_*` function guess it based on the data.
#' @param theme_axis_line_rm `TRUE` or `FALSE` of whether the `gg_*` function should remove the relevant axis line per the `theme_orientation` of the plot.
#' @param theme_axis_ticks_rm `TRUE` or `FALSE` of whether the `gg_*` function should remove the relevant axis ticks per the `theme_orientation` of the plot.
#' @param theme_panel_grid_rm `TRUE` or `FALSE` of whether the `gg_*` function should remove the relevant panel grid per the `theme_orientation` of the plot.
#' @param geom_colour A default hex geom_colour for the geom_colour of geoms (other than "text", "label", "hline", "vline" and "abline" geoms).
#' @param geom_fill A default hex geom_colour for the geom_fill of geoms (other than "text", "label", "hline", "vline" and "abline" geoms).
#' @param geom_text_colour A default hex geom_colour for the geom_colour of the "text" and "label" geoms.
#' @param geom_text_size A default size for the "text" and "label" geoms.
#' @param geom_text_family A default family for the "text" and "label" geoms.
#' @param geom_reference_colour A default hex geom_colour for the geom_colour of the "hline", "vline" and "abline" geoms.
#' @param geom_reference_linewidth A default hex geom_colour for the geom_colour of the "hline", "vline" and "abline" geoms.
#' @param col_palette_d For a discrete scale, a character vector of hex codes.
#' @param col_palette_c For a continuous scale, a character vector of hex codes.
#' @param col_palette_o For an ordinal scale, a `scales::pal_*()` function.
#' @param col_palette_na_d For a discrete scale, a hex code.
#' @param col_palette_na_c For a continuous scale, a hex code.
#' @param col_palette_na_o For an ordinal scale, a hex code.
#' @param label_case A function to format the label of unlabelled variables. Defaults to `snakecase::to_sentence_case`.
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
#'   geom_colour = "#E7298AFF",
#'   geom_colour_text = darkness[1],
#'   geom_colour_hline = darkness[1],
#'   geom_colour_vline = darkness[1],
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
    theme_orientation = NULL,
    theme_axis_line_rm = TRUE,
    theme_axis_ticks_rm = TRUE,
    theme_panel_grid_rm = TRUE,
    label_case = snakecase::to_sentence_case,
    geom_colour = "#357BA2FF",
    geom_fill = geom_colour,
    geom_text_colour = "#121B24FF",
    geom_text_size = 11 / 2.835052,
    geom_text_family = "",
    geom_reference_colour = "#121B24FF",
    geom_reference_linewidth = 0.25,
    col_palette_d = jumble,
    col_palette_c = viridisLite::mako(n = 9, direction = -1),
    col_palette_o = scales::pal_viridis(option = "G", direction = -1),
    col_palette_na_d = "#CDC5BFFF",
    col_palette_na_c = "#988F88FF",
    col_palette_na_o = "#988F88FF") {

  weave_theme(theme = theme,
              theme_orientation = theme_orientation,
              theme_axis_line_rm = theme_axis_line_rm,
              theme_axis_ticks_rm = theme_axis_ticks_rm,
              theme_panel_grid_rm = theme_panel_grid_rm)

  weave_label_case(label_case = label_case)

  weave_geom_defaults(
    geom_colour = geom_colour,
    geom_fill = geom_fill,
    geom_text_colour = geom_text_colour,
    geom_text_size = geom_text_size,
    geom_text_family = geom_text_family,
    geom_reference_colour = geom_reference_colour,
    geom_reference_linewidth = geom_reference_linewidth
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

