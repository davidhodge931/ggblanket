#' Update the default geom palettes
#'
#' @description
#' Updates the active theme to apply consistent colour/fill palette styling.
#' Sets global options for col_palette and geom-specific palettes.
#'
#' @param col_palette_d For a discrete colour/fill scale, a character vector or a `scales::pal_*` function.
#' @param colour_palette_d For a discrete colour scale, a character vector or a `scales::pal_*` function.
#' @param colour_palette_d_border For border geoms with discrete colour scale, a character vector or a `scales::pal_*` function. Excludes boxplot.
#' @param fill_palette_d For a discrete fill scale, a character vector or a `scales::pal_*` function.
#' @param fill_palette_d_border For border geoms with discrete fill scale, a character vector or a `scales::pal_*` function. Excludes boxplot.
#' @param col_palette_c For a continuous colour/fill scale, a character vector or a `scales::pal_*` function.
#' @param colour_palette_c For a continuous colour scale, a character vector or a `scales::pal_*` function.
#' @param colour_palette_c_border For border geoms with continuous colour scale, a character vector or a `scales::pal_*` function. Excludes boxplot.
#' @param fill_palette_c For a continuous fill scale, a character vector or a `scales::pal_*` function.
#' @param fill_palette_c_border For border geoms with continuous fill scale, a character vector or a `scales::pal_*` function. Excludes boxplot.
#' @param ... Additional arguments (not used).
#'
#' @return An updated ggplot2 theme and global options.
#'
#' @noRd
update_geom_palettes <- function(
    col_palette_d = jumble,
    col_palette_c = viridisLite::mako(n = 20, direction = -1, end = 0.9),

    colour_palette_d = col_palette_d,
    colour_palette_d_border = col_squared(colour_palette_d),
    fill_palette_d = col_palette_d,
    fill_palette_d_border = fill_palette_d,

    colour_palette_c = col_palette_c,
    colour_palette_c_border = col_squared(colour_palette_c),
    fill_palette_c = col_palette_c,
    fill_palette_c_border = fill_palette_c,
    ...
) {

  # Set theme-level palettes
  ggplot2::update_theme(
    palette.colour.discrete = colour_palette_d,
    palette.colour.continuous = colour_palette_c,
    palette.fill.discrete = fill_palette_d,
    palette.fill.continuous = fill_palette_c
  )

  # Set global ggblanket options for border palettes
  options(
    ggblanket.colour_palette_d_border = colour_palette_d_border,
    ggblanket.fill_palette_d_border = fill_palette_d_border,
    ggblanket.colour_palette_c_border = colour_palette_c_border,
    ggblanket.fill_palette_c_border = fill_palette_c_border
  )
}
