#' Set the style
#'
#' @description
#' Set the style by setting:
#'
#' 1. the theme, and how/what side-effects are to be applied
#' 2. the geom defaults, including the colour (and fill) of geoms
#' 3. the geom colour (and fill) palettes (i.e. discrete, continuous and ordinal)
#' 4. the function to apply to a unspecified/unlabelled `x_label`, `y_label`, `col_label` etc.
#'
#' [ggplot2::update_geom_defaults()] can be used to further fine-tune geom defaults.
#'
#' @param ... Provided to require argument naming, support trailing commas etc.
#' @param theme A ggplot2 theme (e.g. [light_mode_t()] or [dark_mode_r()]).
#' @param theme_orientation The orientation of plot, which affects the theme components that can be removed by the `gg_*` function. Either `"x"` or `"y"`. Defaults to `NULL`, which lets the `gg_*` function guess it based on the data.
#' @param theme_axis_line_rm `TRUE` or `FALSE` of whether the `gg_*` function should remove the relevant axis line per the `theme_orientation` of the plot.
#' @param theme_axis_ticks_rm `TRUE` or `FALSE` of whether the `gg_*` function should remove the relevant axis ticks per the `theme_orientation` of the plot.
#' @param theme_panel_grid_rm `TRUE` or `FALSE` of whether the `gg_*` function should remove the relevant panel grid per the `theme_orientation` of the plot.
#' @param colour A default hex code for the colour of geoms (i.e. geoms other than "text", "label", "hline", and "vline"). Note, the "fill" inherits from this argument.
#' @param fill A default hex code for the fill of relevant geoms  (i.e. geoms other than "label"). Inherits from colour.
#' @param linewidth A numeric linewidth (i.e. for geoms that are polygons, and do not potentially have necessary adjacent lines - these get a linewidth of 0). Defaults to 0.66.
#' @param col_palette_d For a discrete scale, a character vector of hex codes.
#' @param col_palette_c For a continuous scale, a character vector of hex codes.
#' @param col_palette_o For an ordinal scale, a `scales::pal_*()` function.
#' @param col_palette_na_d For a discrete scale, a hex code.
#' @param col_palette_na_c For a continuous scale, a hex code.
#' @param col_palette_na_o For an ordinal scale, a hex code.
#' @param label_case A function to apply to a unspecified/unlabelled `x_label`, `y_label`, `col_label` etc. Defaults to `snakecase::to_sentence_case`.
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
#'   col_palette_d = c("#1B9E77FF", "#D95F02FF", "#7570b3FF", "#E7298AFF",
#'                     "#66A61EFF", "#E6AB02FF", "#A6761DFF", "#666666FF"),
#' )
#'
#' penguins |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'   )
#'
#' penguins |>
#'   gg_histogram(
#'     x = flipper_length_mm,
#'     col = species,
#'   )
#'
set_blanket <- function(
  ...,
  theme = light_mode_r(),
  theme_orientation = NULL,
  theme_axis_line_rm = TRUE,
  theme_axis_ticks_rm = TRUE,
  theme_panel_grid_rm = TRUE,
  col_palette_d = jumble,
  col_palette_c = viridisLite::mako(n = 9, direction = -1),
  col_palette_o = scales::pal_viridis(option = "G", direction = -1),
  col_palette_na_d = "#CDC5BFFF",
  col_palette_na_c = "#988F88FF",
  col_palette_na_o = "#988F88FF",
  colour = "#357BA2FF",
  fill = colour,
  linewidth = 0.66,
  label_case = snakecase::to_sentence_case
) {
  weave_theme(
    theme = theme,
    theme_orientation = theme_orientation,
    theme_axis_line_rm = theme_axis_line_rm,
    theme_axis_ticks_rm = theme_axis_ticks_rm,
    theme_panel_grid_rm = theme_panel_grid_rm
  )

  weave_geom_base(colour = colour, fill = fill, linewidth = linewidth)

  weave_geom_text()

  weave_geom_label()

  weave_geom_reference_line()

  weave_col_palettes(
    col_palette_d = col_palette_d,
    col_palette_c = col_palette_c,
    col_palette_o = col_palette_o,
    col_palette_na_d = col_palette_na_d,
    col_palette_na_c = col_palette_na_c,
    col_palette_na_o = col_palette_na_o
  )

  weave_label_case(label_case = label_case)
}
