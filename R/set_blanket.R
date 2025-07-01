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
#' @param col A default hex code for the colour and fill of most geoms. Defaults to "#357BA2FF" (i.e. `blue`).
#' @param colour A default hex code for the colour of most geoms. Defaults to col.
#' @param fill A default hex code for the fill of most geoms. Defaults to col.
#' @param size A default size for the point geom. Defaults to 1.5. The pointrange size defaults to dividing by 6 (i.e. 0.25).
#' @param linewidth A default linewidth for most geoms. Defaults to 0.66.
#' @param linetype A default linetype for geoms not specified by linetype_border, linetype_box. or linetype_sf. Defaults to 1.
#' @param linetype_sf A default linetype for sf geoms. Defaults to 0.
#' @param linetype_box A default linetype for boxplot and crossbar geoms. Defaults to 1.
#' @param linetype_border A default linetype for polygon-is geoms (other than boxplot, crossbar or sf). Defaults to 0.
#' @param col_palette For a colour/fill scale, a character vector of hex codes or a `scales::pal_*` function.
#' @param col_palette_d For a discrete colour/fill scale, a character vector of hex codes or a `scales::pal_*` function.
#' @param col_palette_c For a continuous (or ordinal) colour/fill scale, a character vector of hex codes or a `scales::pal_*` function.
#' @param titles_case A function to apply to a unspecified/unlabelled `x_label`, `y_label`, `col_label` etc. Defaults to `snakecase::to_sentence_case`.
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
#'   col_palette_d = c("#1B9E77FF", "#D95F02FF", "#7570b3FF", "#E7298AFF",
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
    col = "#357BA2FF",

    colour = col,
    # colour_border = colour,
    # colour_box = colour_border,
    # colour_sf = colour_border,

    fill = col,

    linewidth = 0.66,
    size = 1.5,

    linetype = 1,
    linetype_border = 0,
    linetype_box = linetype,
    linetype_sf = linetype,

    col_palette = NULL,
    col_palette_d = jumble,
    col_palette_c = viridisLite::mako(n = 9, direction = -1),

    # col_palette_o = scales::pal_viridis(option = "G", direction = -1),
    # col_palette_na_d = "#CDC5BFFF",
    # col_palette_na_c = "#988F88FF",
    # col_palette_na_o = "#988F88FF",
    titles_case = snakecase::to_sentence_case,

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

  weave_geom_col(col = col, colour = colour, fill = fill)

  weave_geom_size(size = size)

  weave_geom_linewidth(linewidth = linewidth)

  weave_geom_linetype(
    linetype = linetype,
    linetype_border = linetype_border,
    linetype_box = linetype_box,
    linetype_sf = linetype_sf
  )

  # weave_geom_annotation(
  #   colour = NULL,
  #   fill = NULL,
  #   linewidth = NULL,
  #   size = NULL,
  # )

  weave_geom_text()
  weave_geom_label()
  weave_geom_vline() #ABLINE TOO??
  weave_geom_hline()
  weave_geom_curve() #NOT SURE

  weave_geom_palettes(
    col_palette = col_palette,
    col_palette_d = col_palette_d,
    col_palette_c = col_palette_c,
    # col_palette_o = col_palette_o,
    # col_palette_na_d = col_palette_na_d,
    # col_palette_na_c = col_palette_na_c,
    # col_palette_na_o = col_palette_na_o
  )

  weave_titles_case(titles_case = titles_case)
}
