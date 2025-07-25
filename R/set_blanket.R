#' Set the style
#'
#' @description
#' Set a consistent style for ggplot2 visualizations by configuring:
#' theme, geom default colour/fill, linetypes, linewidths, sizes,
#' fonts, palettes, and other styling options. Most users will only need
#' `theme`, `col`, `col_palette_d` and , `col_palette_c` arguments.
#'
#' @param theme A ggplot2 theme (e.g. [theme_lighter()] or [theme_darker()]).
#' @param col A default hex code for the colour and fill of most geoms. Defaults to `blue`.
#' @param linewidth A default linewidth for most geoms. Defaults to 0.66.
#' @param linetype A default linetype for most geoms. Defaults to 1 (solid).
#' @param shape A default shape for point geoms. Must be an integer between 0 and 25.
#' @param size A default size for point geoms.
#' @param stroke A default stroke for point geoms.
#' @param col_palette_d For a discrete colour/fill scale, a character vector or a `scales::pal_*` function.
#' @param col_palette_c For a continuous colour/fill scale, a character vector or a `scales::pal_*` function.
#' @param col_palette_o For a ordinal colour/fill scale, a `scales::pal_*` function. If NULL, determined from `col_palette_c`.
#' @param col_na A NA colour/fill value.
#' @param shape_palette_d For shape scales, a numeric vector of shape codes. Defaults to c(21, 24, 22, 23, 25).
#' @param linetype_palette_d For linetype scales, a character vector or a `scales::pal_*` function. Defaults to 1:6.
#' @param bordered_colour_by A function with input of `col`. Defaults to `(x) col_multiply(x)`. Affects colour as well as colour palette.
#' @param bordered_fill_by A function with input of `col`. Defaults to NA. Affects fill as well as fill palette.
#' @param bordered_linewidth A function with input of `linewidth`. Defaults to \(x) x / 2.64.

#' @param titles_case A function to apply to unspecified/unlabelled titles in `gg_*` functions.
#'   Defaults to `snakecase::to_sentence_case`.
#' @param aspect_axis_line_rm Logical. Whether `gg_*` functions should remove the relevant
#'   axis line based on plot aspect. Defaults to TRUE.
#' @param aspect_axis_ticks_rm Logical. Whether `gg_*` functions should remove the relevant
#'   axis ticks based on plot aspect. Defaults to TRUE.
#' @param aspect_panel_grid_rm Logical. Whether `gg_*` functions should remove the relevant
#'   panel grid based on plot aspect. Defaults to TRUE.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns NULL. Sets global styling options as a side effect.
#'
#' @seealso
#' [theme_lighter()], [theme_darker()] for theme options
#' [col_multiply()] for creating accent colours
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(ggblanket)
#'
#' # Use col_palette_d for both colour and fill
#' set_blanket(
#'   col_palette_d = c("#E69F00", "#56B4E9", "#009E73")
#' )
#'
set_blanket <- function(
  theme = theme_lighter(),

  col = "#357BA2FF",
  linewidth = 0.66,
  linetype = 1,
  size = 1.5,
  shape = 21,
  stroke = 0.5,

  bordered_colour_by = \(x) {
    ifelse(is_panel_dark(), col_screen(x), col_multiply(x))
  },
  bordered_fill_by = NULL,
  bordered_linewidth = 0.25,

  col_palette_d = scales::pal_hue(),
  col_palette_c = pal_viridis_by_panel("mako", 0.1, 0.9),
  col_palette_o = NULL,
  col_na = "#A6A6A6FF",
  shape_palette_d = c(21, 24, 22, 23, 25),
  linetype_palette_d = 1:6,

  titles_case = snakecase::to_sentence_case,
  aspect_axis_line_rm = TRUE,
  aspect_axis_ticks_rm = TRUE,
  aspect_panel_grid_rm = TRUE,
  ...
) {
  # Set the theme first
  ggplot2::set_theme(theme)

  current_theme <- ggplot2::get_theme()

  # Handle colour/fill defaults
  update_geom_col(col = col)
  update_geom_shape(shape = shape)
  update_geom_linetype(linetype = linetype)
  update_geom_linewidth(linewidth = linewidth)
  update_geom_size(size = size)
  update_geom_stroke(stroke = stroke)

  update_geom_palettes(
    col_palette_d = col_palette_d,
    col_palette_c = col_palette_c,
    col_palette_o = col_palette_o,
    col_na = col_na,
    shape_palette_d = shape_palette_d,
    linetype_palette_d = linetype_palette_d
  )

  update_geom_bordered(
    bordered_colour_by = bordered_colour_by,
    bordered_fill_by = bordered_fill_by,
    bordered_linewidth = bordered_linewidth
  )

  update_geom_font()

  update_geom_reference()

  update_titles_case(titles_case = titles_case)

  update_aspect(
    aspect_axis_line_rm = aspect_axis_line_rm,
    aspect_axis_ticks_rm = aspect_axis_ticks_rm,
    aspect_panel_grid_rm = aspect_panel_grid_rm
  )
}
