#' Set the style
#'
#' @description
#' Set the style by setting:
#'
#' 1. the theme, and how/what side-effects are to be applied
#' 2. the geom defaults, including the colour (and fill) of geoms
#' 3. the colour (and fill) palettes (i.e. discrete, continuous and ordinal)
#' 4. the function to apply to a unspecified/unlabelled `x_label`, `y_label`, `col_label` etc.
#'
#' [ggplot2::update_geom_defaults()] can be used to further fine-tune geom defaults.
#'
#' @param theme A ggplot2 theme (e.g. [theme_lighter()] or [theme_darker()]).
#' @param ... Provided to require argument naming, support trailing commas etc.
#' @param colour For most geoms, a default hex code for the colour of geoms (i.e. geoms other than "text", "label", "hline", and "vline"). Note, the "fill" inherits from this argument.
#' @param col_palette_discrete For a discrete scale, a character vector of hex codes.
#' @param col_palette_continuous For a continuous scale, a character vector of hex codes.
#' @param col_palette_o For an ordinal scale, a `scales::pal_*()` function.
#' @param col_palette_na_d For a discrete scale, a hex code.
#' @param col_palette_na_c For a continuous scale, a hex code.
#' @param col_palette_na_o For an ordinal scale, a hex code.
#' @param label_case A function to apply to a unspecified/unlabelled `x_label`, `y_label`, `col_label` etc. Defaults to `snakecase::to_sentence_case`.
#' @param perspective The perspective of plot, which affects the theme components that can be removed by the `gg_*` function. Either `"x"` or `"y"`. Defaults to `NULL`, which lets the `gg_*` function guess it based on the data.
#' @param axis_line_transparent `TRUE` or `FALSE` of whether the `gg_*` function should remove the relevant axis line per the `perspective` of the plot.
#' @param axis_ticks_transparent `TRUE` or `FALSE` of whether the `gg_*` function should remove the relevant axis ticks per the `perspective` of the plot.
#' @param panel_grid_transparent `TRUE` or `FALSE` of whether the `gg_*` function should remove the relevant panel grid per the `perspective` of the plot.
#'
#' @return A globally set style.
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(ggblanket)
#'
#' set_blanket(
#'   theme = theme_darker(),
#'   colour = "#E7298AFF",
#'   col_palette_discrete = c("#1B9E77FF", "#D95F02FF", "#7570b3FF", "#E7298AFF",
#'                     "#66A61EFF", "#E6AB02FF", "#A6761DFF", "#666666FF"),
#' )
#'
#' palmerpenguins::penguins |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'   )
#'
#' palmerpenguins::penguins |>
#'   gg_histogram(
#'     x = flipper_length_mm,
#'     col = species,
#'   )
#'
set_blanket <- function(
    theme = theme_lighter(),
    ...,
    colour = "#357BA2FF",

    # col_palette_discreteiscrete = jumble,
    # col_palette_continuousontinuous = viridisLite::mako(n = 9, direction = -1),

    col_palette_discrete = jumble,
    col_palette_continuous = viridisLite::mako(n = 9, direction = -1),
    col_palette_o = scales::pal_viridis(option = "G", direction = -1),
    col_palette_na_d = "#CDC5BFFF",
    col_palette_na_c = "#988F88FF",
    col_palette_na_o = "#988F88FF",
    label_case = snakecase::to_sentence_case,

    perspective = NULL,
    axis_line_transparent = TRUE,
    axis_ticks_transparent = TRUE,
    panel_grid_transparent = TRUE
) {

  weave_theme(
    theme = theme,
    perspective = perspective,
    axis_line_transparent = axis_line_transparent,
    axis_ticks_transparent = axis_ticks_transparent,
    panel_grid_transparent = panel_grid_transparent
  )

  # weave_geom(colour = colour)
  weave_geom_colour_fill(colour = colour)
  weave_geom_linewidth()
  weave_geom_bordertype()
  weave_geom_size()

  weave_geom_text()
  weave_geom_label()
  weave_geom_vline()
  weave_geom_hline()

  weave_geom_palettes(
    col_palette_discrete = col_palette_discrete,
    col_palette_continuous = col_palette_continuous,
    col_palette_o = col_palette_o,
    col_palette_na_d = col_palette_na_d,
    col_palette_na_c = col_palette_na_c,
    col_palette_na_o = col_palette_na_o
  )

  weave_label_case(label_case = label_case)
}
