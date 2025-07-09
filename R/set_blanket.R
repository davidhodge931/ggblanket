#' Set the style
#'
#' @description
#' Set a consistent style for ggplot2 visualizations by configuring:
#' theme, geom aesthetics (colour/fill), linetypes, linewidths, sizes,
#' fonts, palettes, and other styling options.
#'
#' For convenience, `col_palette_d` and `col_palette_c` can be used to set
#' both colour and fill palettes simultaneously. More specific palette arguments
#' (e.g., `colour_palette_d`, `fill_palette_d`) will override these general settings.
#'
#' @param theme A ggplot2 theme (e.g. [theme_lighter()] or [theme_darker()]).
#' @param col A default hex code for the colour and fill of most geoms. Defaults to `blue`.
#' @param colour A default hex code for the colour of most geoms. If NULL, uses `col`.
#' @param colour_polygon A hex code for polygon-type geoms (area, bar, density, etc.).
#'   If NULL, uses `col_squared(colour)`.
#' @param colour_box A hex code for box-type geoms (boxplot, crossbar). If NULL, uses `colour`.
#' @param colour_font A hex code for text/label geoms. If NULL, derived from theme axis text colour.
#' @param colour_reference_line A hex code for reference line geoms (abline, hline, vline).
#'   If NULL, derived from theme axis line colour.
#' @param fill A default hex code for the fill of most geoms. If NULL, uses `col`.
#' @param fill_polygon A hex code for polygon-type geoms. If NULL, uses `fill`.
#' @param fill_box A hex code for box-type geoms. If NULL, uses `fill`.
#' @param fill_font A hex code for label geom fills. If NULL, derived from theme panel background.
#' @param linewidth A default linewidth for most geoms. Defaults to 0.66.
#' @param linewidth_polygon A linewidth for polygon-type geoms. Defaults to 0.25.
#' @param linewidth_box A linewidth for box-type geoms. Defaults to 0.25.
#' @param linewidth_reference_line A linewidth for reference line geoms.
#'   If NULL, derived from theme axis line linewidth.
#' @param linetype A default linetype for most geoms. Defaults to 1 (solid).
#' @param linetype_polygon A linetype for polygon-type geoms. If NULL, uses `linetype`.
#' @param linetype_box A linetype for box-type geoms. If NULL, uses `linetype`.
#' @param linetype_reference_line A linetype for reference line geoms. If NULL, uses `linetype`.
#' @param size A default size for point geoms. Defaults to 1.5.
#' @param size_font A size for text/label geoms in mm. If NULL, derived from theme axis text size.
#' @param family_font A font family. If NULL, derived from axis text family.
#' @param col_palette_d For discrete scales, a palette to use for both colour and fill.
#'   A character vector of hex codes or a `scales::pal_*` function. Defaults to `jumble`.
#' @param col_palette_c For continuous scales, a palette to use for both colour and fill.
#'   A character vector of hex codes or a `scales::pal_*` function.
#'   Defaults to `scales::pal_viridis(option = "G", direction = -1)`.
#' @param colour_palette_d For discrete colour scales. If NULL, uses `col_palette_d`.
#' @param colour_palette_d_polygon For polygon-type geoms with discrete colour scales.
#'   If NULL, uses `col_squared(colour_palette_d)`.
#' @param colour_palette_d_box For box-type geoms with discrete colour scales.
#'   If NULL, uses `colour_palette_d`.
#' @param fill_palette_d For discrete fill scales. If NULL, uses `col_palette_d`.
#' @param fill_palette_d_polygon For polygon-type geoms with discrete fill scales.
#'   If NULL, uses `fill_palette_d`.
#' @param fill_palette_d_box For box-type geoms with discrete fill scales.
#'   If NULL, uses `fill_palette_d`.
#' @param colour_palette_c For continuous colour scales. If NULL, uses `col_palette_c`.
#' @param colour_palette_c_polygon For polygon-type geoms with continuous colour scales.
#'   If NULL, uses `colour_palette_c`.
#' @param colour_palette_c_box For box-type geoms with continuous colour scales.
#'   If NULL, uses `colour_palette_c`.
#' @param fill_palette_c For continuous fill scales. If NULL, uses `col_palette_c`.
#' @param fill_palette_c_polygon For polygon-type geoms with continuous fill scales.
#'   If NULL, uses `fill_palette_c`.
#' @param fill_palette_c_box For box-type geoms with continuous fill scales.
#'   If NULL, uses `fill_palette_c`.
#' @param titles_case A function to apply to unspecified/unlabelled titles in `gg_*` functions.
#'   Defaults to `snakecase::to_sentence_case`.
#' @param axis_line_transparent Logical. Whether `gg_*` functions should remove the relevant
#'   axis line based on plot perspective. Defaults to TRUE.
#' @param axis_ticks_transparent Logical. Whether `gg_*` functions should remove the relevant
#'   axis ticks based on plot perspective. Defaults to TRUE.
#' @param panel_grid_transparent Logical. Whether `gg_*` functions should remove the relevant
#'   panel grid based on plot perspective. Defaults to TRUE.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns NULL. Sets global styling options as a side effect.
#'
#' @seealso
#' [theme_lighter()], [theme_darker()] for theme options
#' [col_squared()] for creating accent colours
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
    col = blue,
    colour = NULL,
    colour_polygon = NULL,
    colour_box = NULL,
    colour_font = NULL,
    colour_reference_line = NULL,
    fill = NULL,
    fill_polygon = NULL,
    fill_box = NULL,
    fill_font = NULL,
    linewidth = 0.66,
    linewidth_polygon = 0.25,
    linewidth_box = NULL,
    linewidth_reference_line = NULL,
    linetype = 1,
    linetype_polygon = NULL,
    linetype_box = NULL,
    linetype_reference_line = NULL,
    size = 1.5,
    size_font = NULL,
    family_font = NULL,
    col_palette_d = jumble,
    col_palette_c = viridisLite::mako(20, direction = -1),
    colour_palette_d = NULL,
    colour_palette_d_polygon = NULL,
    colour_palette_d_box = NULL,
    fill_palette_d = NULL,
    fill_palette_d_polygon = NULL,
    fill_palette_d_box = NULL,
    colour_palette_c = NULL,
    colour_palette_c_polygon = NULL,
    colour_palette_c_box = NULL,
    fill_palette_c = NULL,
    fill_palette_c_polygon = NULL,
    fill_palette_c_box = NULL,
    titles_case = snakecase::to_sentence_case,
    axis_line_transparent = TRUE,
    axis_ticks_transparent = TRUE,
    panel_grid_transparent = TRUE,
    ...
) {

  # Handle colour/fill defaults
  colour <- colour %||% col
  colour_polygon <- colour_polygon %||% col_squared(colour)
  colour_box <- colour_box %||% colour

  fill <- fill %||% col
  fill_polygon <- fill_polygon %||% fill
  fill_box <- fill_box %||% fill

  # Handle linetype defaults
  linetype_polygon <- linetype_polygon %||% linetype
  linetype_box <- linetype_box %||% linetype

  # Handle linewidth defaults
  linewidth_box <- linewidth_box %||% linewidth_polygon %||% linewidth

  # Handle palette defaults
  # If specific palettes aren't provided, use col_palette as default
  colour_palette_d <- colour_palette_d %||% col_palette_d
  fill_palette_d <- fill_palette_d %||% col_palette_d
  colour_palette_c <- colour_palette_c %||% col_palette_c
  fill_palette_c <- fill_palette_c %||% col_palette_c

  # Handle polygon/box palette defaults
  colour_palette_d_polygon <- colour_palette_d_polygon %||% col_squared(colour_palette_d)
  colour_palette_d_box <- colour_palette_d_box %||% colour_palette_d
  fill_palette_d_polygon <- fill_palette_d_polygon %||% fill_palette_d
  fill_palette_d_box <- fill_palette_d_box %||% fill_palette_d

  colour_palette_c_polygon <- colour_palette_c_polygon %||% colour_palette_c
  colour_palette_c_box <- colour_palette_c_box %||% colour_palette_c
  fill_palette_c_polygon <- fill_palette_c_polygon %||% fill_palette_c
  fill_palette_c_box <- fill_palette_c_box %||% fill_palette_c

  # Set the theme first
  ggplot2::set_theme(theme)

  # Update geom defaults
  update_geom_col(
    colour = colour,
    colour_polygon = colour_polygon,
    colour_box = colour_box,
    colour_font = colour_font,
    colour_reference_line = colour_reference_line,
    fill = fill,
    fill_polygon = fill_polygon,
    fill_box = fill_box,
    fill_font = fill_font
  )

  update_geom_linetype(
    linetype = linetype,
    linetype_polygon = linetype_polygon,
    linetype_box = linetype_box,
    linetype_reference_line = linetype_reference_line
  )

  update_geom_linewidth(
    linewidth = linewidth,
    linewidth_polygon = linewidth_polygon,
    linewidth_box = linewidth_box,
    linewidth_reference_line = linewidth_reference_line
  )

  update_geom_size(
    size = size,
    size_font = size_font
  )

  update_geom_family(family_font = family_font)

  update_geom_palettes(
    col_palette_d = col_palette_d,
    col_palette_c = col_palette_c,
    colour_palette_d = colour_palette_d,
    fill_palette_d = fill_palette_d,
    colour_palette_c = colour_palette_c,
    fill_palette_c = fill_palette_c,
    colour_palette_d_polygon = colour_palette_d_polygon,
    colour_palette_d_box = colour_palette_d_box,
    fill_palette_d_polygon = fill_palette_d_polygon,
    fill_palette_d_box = fill_palette_d_box,
    colour_palette_c_polygon = colour_palette_c_polygon,
    colour_palette_c_box = colour_palette_c_box,
    fill_palette_c_polygon = fill_palette_c_polygon,
    fill_palette_c_box = fill_palette_c_box
  )

  weave_titles_case(titles_case = titles_case)

  weave_perspective(
    axis_line_transparent = axis_line_transparent,
    axis_ticks_transparent = axis_ticks_transparent,
    panel_grid_transparent = panel_grid_transparent
  )
}







