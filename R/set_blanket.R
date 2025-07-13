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
#' @param colour A default hex code for the colour of most geoms. If NULL, uses `col`.
#' @param colour_border A hex code for border geoms.
#'   If NULL, uses `col_multiply(colour)`.
#' @param colour_font A hex code for text/label geoms. If NULL, derived from theme axis text colour.
#' @param colour_reference_line A hex code for reference line geoms (abline, hline, vline).
#'   If NULL, derived from theme axis line colour.
#' @param fill A default hex code for the fill of most geoms. If NULL, uses `col`.
#' @param fill_border A hex code for border geoms. If NULL, uses `fill`.
#' @param fill_font A hex code for label geom fills. If NULL, derived from theme panel background.
#' @param linewidth A default linewidth for most geoms. Defaults to 0.66.
#' @param linewidth_border A linewidth for border geoms. Defaults to 0.25.
#' @param linewidth_reference_line A linewidth for reference line geoms.
#'   If NULL, derived from theme axis line linewidth.
#' @param linetype A default linetype for most geoms. Defaults to 1 (solid).
#' @param linetype_border A linetype for border geoms. If NULL, uses `linetype`.
#' @param linetype_reference_line A linetype for reference line geoms. If NULL, uses `linetype`.
#' @param shape A default shape for point geoms. Must be an integer between 0 and 25.
#' @param size A default size for point geoms.
#' @param size_font A size for text/label geoms in mm. If NULL, derived from theme axis text size.
#' @param family_font A font family. If NULL, derived from axis text family.
#' @param col_palette_d For a discrete colour/fill scale, a character vector or a `scales::pal_*` function.
#' @param col_palette_c For a continuous colour/fill scale, a character vector or a `scales::pal_*` function.
#' @param colour_palette_d For a discrete colour scale, a character vector or a `scales::pal_*` function.
#' @param colour_palette_d_border For border geoms with discrete colour scale, a character vector or a `scales::pal_*` function.
#' @param fill_palette_d For a discrete fill scale, a character vector or a `scales::pal_*` function.
#' @param fill_palette_d_border For border geoms with discrete fill scale, a character vector or a `scales::pal_*` function.
#' @param colour_palette_c For a continuous colour scale, a character vector or a `scales::pal_*` function.
#' @param colour_palette_c_border For border geoms with continuous colour scale, a character vector or a `scales::pal_*` function.
#' @param fill_palette_c For a continuous fill scale, a character vector or a `scales::pal_*` function.
#' @param fill_palette_c_border For border geoms with continuous fill scale, a character vector or a `scales::pal_*` function.
#' @param col_palette_na For NA values in both colour/fill scales, a hex code. Defaults to "#CDC5BFFF".
#' @param colour_palette_na For NA values in colour scales, a hex code.
#' @param colour_palette_na_border For NA values in border geoms with colour scale, a hex code.
#' @param fill_palette_na For NA values in fill scales, a hex code.
#' @param fill_palette_na_border For NA values in border geoms with fill scale, a hex code.
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

    col = "#8991A1",
    colour = NULL,
    colour_border = NULL,
    colour_font = NULL,
    colour_reference_line = NULL,
    fill = NULL,
    fill_border = NULL,
    fill_font = NULL,

    linewidth = 0.66,
    linewidth_border = 0.25,
    linewidth_reference_line = NULL,

    linetype = 1,
    linetype_border = NULL,
    linetype_reference_line = NULL,

    size = 1.5,
    size_font = NULL,
    shape = 21,
    family_font = NULL,

    col_palette_d = scales::pal_hue(),
    col_palette_c = mako_cruise(),

    colour_palette_d = NULL,
    colour_palette_d_border = NULL,
    fill_palette_d = NULL,
    fill_palette_d_border = NULL,

    colour_palette_c = NULL,
    colour_palette_c_border = NULL,
    fill_palette_c = NULL,
    fill_palette_c_border = NULL,

    col_palette_na = "#A6A6A6",
    colour_palette_na = NULL,
    colour_palette_na_border = NULL,
    fill_palette_na = NULL,
    fill_palette_na_border = NULL,

    titles_case = snakecase::to_sentence_case,
    axis_line_transparent = TRUE,
    axis_ticks_transparent = TRUE,
    panel_grid_transparent = TRUE,
    ...
) {

  # Set the theme first
  ggplot2::set_theme(theme)
  current_theme <- ggplot2::get_theme()

  # Handle colour/fill defaults
  if(is.null(colour)) colour <- col
  if (is.null(colour_border)) {
    if (is_panel_background_dark(theme = current_theme)) {
      colour_border <- col_screen(colour)
    } else {
      colour_border <- col_multiply(colour)
    }
  }
  if(is.null(fill)) fill <- col
  if(is.null(fill_border)) fill_border <- fill

  # Handle linetype defaults
  if(is.null(linetype_border)) linetype_border <- linetype

  # Handle palette defaults
  if (rlang::is_null(colour_palette_d)) colour_palette_d <- col_palette_d
  if (rlang::is_null(colour_palette_d_border)) {
    if (is_panel_background_dark(theme = current_theme)) {
      colour_palette_d_border <- col_screen(colour_palette_d)
    } else {
      colour_palette_d_border <- col_multiply(colour_palette_d)
    }
  }
  if (rlang::is_null(fill_palette_d)) fill_palette_d <- col_palette_d
  if (rlang::is_null(fill_palette_d_border)) fill_palette_d_border <- fill_palette_d

  if (rlang::is_null(colour_palette_c)) colour_palette_c <- col_palette_c
  if (rlang::is_null(colour_palette_c_border)) {
    if (is_panel_background_dark(current_theme)) {
      colour_palette_c_border <- col_screen(colour_palette_c)
    } else {
      colour_palette_c_border <- col_multiply(colour_palette_c)
    }
  }
  if (rlang::is_null(fill_palette_c)) fill_palette_c <- col_palette_c
  if (rlang::is_null(fill_palette_c_border)) fill_palette_c_border <- fill_palette_c

  # Handle NA color defaults
  if (rlang::is_null(colour_palette_na)) colour_palette_na <- col_palette_na
  if (rlang::is_null(colour_palette_na_border)) {
    if (is_panel_background_dark(theme = current_theme)) {
      colour_palette_na_border <- col_screen(colour_palette_na)
    } else {
      colour_palette_na_border <- col_multiply(colour_palette_na)
    }
  }
  if (rlang::is_null(fill_palette_na)) fill_palette_na <- col_palette_na
  if (rlang::is_null(fill_palette_na_border)) fill_palette_na_border <- fill_palette_na

  # Update geom defaults
  update_geom_shape(
    shape = shape
  )

  update_geom_col(
    colour = colour,
    colour_border = colour_border,
    colour_font = colour_font,
    colour_reference_line = colour_reference_line,
    fill = fill,
    fill_border = fill_border,
    fill_font = fill_font
  )

  update_geom_linetype(
    linetype = linetype,
    linetype_border = linetype_border,
    linetype_reference_line = linetype_reference_line
  )

  update_geom_linewidth(
    linewidth = linewidth,
    linewidth_border = linewidth_border,
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
    colour_palette_d_border = colour_palette_d_border,
    fill_palette_d_border = fill_palette_d_border,
    colour_palette_c_border = colour_palette_c_border,
    fill_palette_c_border = fill_palette_c_border,

    col_palette_na = col_palette_na,
    colour_palette_na = colour_palette_na,
    colour_palette_na_border = colour_palette_na_border,
    fill_palette_na = fill_palette_na,
    fill_palette_na_border = fill_palette_na_border
  )

  update_titles_case(titles_case = titles_case)

  update_perspective(
    axis_line_transparent = axis_line_transparent,
    axis_ticks_transparent = axis_ticks_transparent,
    panel_grid_transparent = panel_grid_transparent
  )
}
