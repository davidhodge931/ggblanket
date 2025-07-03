#' Update the geom palettes
#'
#' @description
#' This function allows you to set colour and fill palettes with a hierarchical priority system. More specific parameters override more general ones.
#'
#' @param ... Provided to require argument naming, support trailing commas etc.
#' @param col_palette For a colour and fill scale, a character vector of hex codes, or a `scales::pal_*()` function. Note does not apply to ordinal scales.
#' @param col_palette_d For a discrete colour and fill scale, a character vector of hex codes, or a `scales::pal_*()` function.
#' @param col_palette_c For a continuous colour and fill scale, a character vector of hex codes, or a `scales::pal_*()` function.
#' @param colour_palette For a colour scale, a character vector of hex codes, or a `scales::pal_*()` function. Note does not apply to ordinal scales.
#' @param colour_palette_d For a discrete colour scale, a character vector of hex codes, or a `scales::pal_*()` function.
#' @param colour_palette_c For a continuous colour scale, a character vector of hex codes, or a `scales::pal_*()` function.
#' @param fill_palette For a fill scale, a character vector of hex codes, or a `scales::pal_*()` function. Note does not apply to ordinal scales.
#' @param fill_palette_d For a discrete fill scale, a character vector of hex codes, or a `scales::pal_*()` function.
#' @param fill_palette_c For a continuous fill scale, a character vector of hex codes, or a `scales::pal_*()` function.
#'
#' @details
#' The function uses a hierarchical priority system where more specific parameters
#' override more general ones. The priority order is:
#'
#' For colour scales:
#' 1. `colour_palette_d`/`colour_palette_c` (most specific)
#' 2. `colour_palette` (general colour)
#' 3. `col_palette_d`/`col_palette_c` (specific col)
#' 4. `col_palette` (most general)
#'
#' For fill scales:
#' 1. `fill_palette_d`/`fill_palette_c` (most specific)
#' 2. `fill_palette` (general fill)
#' 3. `col_palette_d`/`col_palette_c` (fallback to col)
#' 4. `col_palette` (most general fallback)
#'
#' @return An updated ggplot2 theme
#' @export
#'
#' @examples
#' # Set a general palette for all scales
#' update_geom_palettes(col_palette = scales::pal_viridis())
#'
#' # Override just discrete colours
#' update_geom_palettes(col_palette = scales::pal_hue())
#'
#' # Set different palettes for colour and fill discrete
#' update_geom_palettes(colour_palette_d = colorspace::darken(jumble, 0.1), fill_palette_d = jumble)
update_geom_palettes <- function(
    ...,
    col_palette = NULL,
    col_palette_d = NULL,
    col_palette_c = NULL,

    colour_palette = NULL,
    colour_palette_d = NULL,
    colour_palette_c = NULL,

    fill_palette = NULL,
    fill_palette_d = NULL,
    fill_palette_c = NULL
) {
  options(ggplot2.discrete.colour = NULL)
  options(ggplot2.continuous.colour = NULL)
  options(ggplot2.discrete.fill = NULL)
  options(ggplot2.continuous.fill = NULL)

  # Determine colour_palette_d (most specific to most general)
  colour_palette_d <- colour_palette_d %||%
    colour_palette %||%
    col_palette_d %||%
    col_palette %||%
    jumble

  # Determine colour_palette_c (most specific to most general)
  colour_palette_c <- colour_palette_c %||%
    colour_palette %||%
    col_palette_c %||%
    col_palette %||%
    scales::pal_viridis(option = "G", direction = -1)

  # Determine fill_palette_d (most specific to most general)
  fill_palette_d <- fill_palette_d %||%
    fill_palette %||%
    col_palette_d %||%
    col_palette %||%
    jumble

  # Determine fill_palette_c (most specific to most general)
  fill_palette_c <- fill_palette_c %||%
    fill_palette %||%
    col_palette_c %||%
    col_palette %||%
    scales::pal_viridis(option = "G", direction = -1)

  # Update the theme with palette values
  ggplot2::update_theme(
    palette.colour.discrete = colour_palette_d,
    palette.colour.continuous = colour_palette_c,
    palette.fill.discrete = fill_palette_d,
    palette.fill.continuous = fill_palette_c
  )
}
