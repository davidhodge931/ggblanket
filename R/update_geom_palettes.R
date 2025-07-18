#' Update the default geom palettes
#'
#' @description
#' Updates the active theme to apply consistent colour/fill palette styling.
#' Sets global options for col_palette and geom-specific palettes, including NA values.
#'
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
#' @param shape_palette For shape scales, a numeric vector of shape codes. Defaults to c(21, 25, 22:24).
#' @param linetype_palette For linetype scales, a character vector or a `scales::pal_*` function. Defaults to scales::pal_linetype().
#' @param ... Additional arguments (not used).
#'
#' @return An updated ggplot2 theme and global options.
#'
#' @noRd
update_geom_palettes <- function(
    col_palette_d = scales::pal_hue(),
    col_palette_c = viridis_by_theme(n = 256, begin = 0.05, end = 0.95, option = "G"),
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
    shape_palette = c(21, 24, 22, 23, 25),
    linetype_palette = c("solid", "dashed", "dotted", "longdash"),
    ...
) {

  # Get current theme first
  current_theme <- ggplot2::get_theme()

  # Handle palette defaults
  if (rlang::is_null(colour_palette_d)) colour_palette_d <- col_palette_d
  if (rlang::is_null(colour_palette_d_border)) {
    if (is_theme_dark(theme = current_theme)) {
      colour_palette_d_border <- col_screen(colour_palette_d)
    } else {
      colour_palette_d_border <- col_multiply(colour_palette_d)
    }
  }
  if (rlang::is_null(fill_palette_d)) fill_palette_d <- col_palette_d
  if (rlang::is_null(fill_palette_d_border)) fill_palette_d_border <- fill_palette_d

  if (rlang::is_null(colour_palette_c)) colour_palette_c <- col_palette_c
  if (rlang::is_null(colour_palette_c_border)) {
    if (is_theme_dark(current_theme)) {
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
    if (is_theme_dark(theme = current_theme)) {
      colour_palette_na_border <- col_screen(colour_palette_na)
    } else {
      colour_palette_na_border <- col_multiply(colour_palette_na)
    }
  }
  if (rlang::is_null(fill_palette_na)) fill_palette_na <- col_palette_na
  if (rlang::is_null(fill_palette_na_border)) fill_palette_na_border <- fill_palette_na

  # Set theme-level palettes
  ggplot2::update_theme(
    palette.colour.discrete = colour_palette_d,
    palette.colour.continuous = colour_palette_c,
    palette.fill.discrete = fill_palette_d,
    palette.fill.continuous = fill_palette_c
  )

  # Set global ggblanket options for all palettes and NA colors
  options(
    # Border palettes
    ggblanket.colour_palette_d_border = colour_palette_d_border,
    ggblanket.fill_palette_d_border = fill_palette_d_border,
    ggblanket.colour_palette_c_border = colour_palette_c_border,
    ggblanket.fill_palette_c_border = fill_palette_c_border,
    # NA colors
    ggblanket.col_palette_na = col_palette_na,
    ggblanket.colour_palette_na = colour_palette_na,
    ggblanket.colour_palette_na_border = colour_palette_na_border,
    ggblanket.fill_palette_na = fill_palette_na,
    ggblanket.fill_palette_na_border = fill_palette_na_border,
    # Shape and linetype palettes
    ggblanket.shape_palette = shape_palette,
    ggblanket.linetype_palette = linetype_palette
  )
}
