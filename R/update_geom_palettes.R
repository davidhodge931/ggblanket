#' Update the default geom palettes
#'
#' @description
#' Updates the active theme to apply consistent colour/fill palette styling.
#' Sets global options for col_palette and geom-specific palettes.
#'
#' @param col_palette_d For a discrete colour/fill scale, a character vector of hex codes or a `scales::pal_*` function.
#' @param col_palette_c For a continuous colour/fill scale, a character vector of hex codes or a `scales::pal_*` function.
#' @param colour_palette_d For a discrete colour scale, a character vector of hex codes or a `scales::pal_*` function.
#' @param fill_palette_d For a discrete fill scale, a character vector of hex codes or a `scales::pal_*` function.
#' @param colour_palette_c For a continuous colour scale, a character vector of hex codes or a `scales::pal_*` function.
#' @param fill_palette_c For a continuous fill scale, a character vector of hex codes or a `scales::pal_*` function.
#' @param colour_palette_d_polygon For polygon-type geoms with discrete colour scale.
#' @param colour_palette_d_box For box-type geoms with discrete colour scale.
#' @param fill_palette_d_polygon For polygon-type geoms with discrete fill scale.
#' @param fill_palette_d_box For box-type geoms with discrete fill scale.
#' @param colour_palette_c_polygon For polygon-type geoms with continuous colour scale.
#' @param colour_palette_c_box For box-type geoms with continuous colour scale.
#' @param fill_palette_c_polygon For polygon-type geoms with continuous fill scale.
#' @param fill_palette_c_box For box-type geoms with continuous fill scale.
#' @param ... Additional arguments (not used).
#'
#' @return An updated ggplot2 theme and global options.
#'
#' @noRd
update_geom_palettes <- function(
    col_palette_d = jumble,
    col_palette_c = scales::pal_viridis(option = "G", direction = -1),
    colour_palette_d = col_palette_d,
    colour_palette_d_polygon = col_squared(colour_palette_d),
    colour_palette_d_box = colour_palette_d,
    fill_palette_d = col_palette_d,
    fill_palette_d_polygon = fill_palette_d,
    fill_palette_d_box = fill_palette_d,
    colour_palette_c = col_palette_c,
    colour_palette_c_polygon = colour_palette_c,
    colour_palette_c_box = colour_palette_c,
    fill_palette_c = col_palette_c,
    fill_palette_c_polygon = fill_palette_c,
    fill_palette_c_box = fill_palette_c,
    ...
) {

  # Set theme-level palettes
  ggplot2::update_theme(
    palette.colour.discrete = colour_palette_d,
    palette.colour.continuous = colour_palette_c,
    palette.fill.discrete = fill_palette_d,
    palette.fill.continuous = fill_palette_c
  )

  # Set global ggblanket options for all palettes
  options(
    ggblanket.col_palette_d = col_palette_d,
    ggblanket.col_palette_c = col_palette_c,
    ggblanket.colour_palette_d_polygon = colour_palette_d_polygon,
    ggblanket.colour_palette_d_box = colour_palette_d_box,
    ggblanket.fill_palette_d_polygon = fill_palette_d_polygon,
    ggblanket.fill_palette_d_box = fill_palette_d_box,
    ggblanket.colour_palette_c_polygon = colour_palette_c_polygon,
    ggblanket.colour_palette_c_box = colour_palette_c_box,
    ggblanket.fill_palette_c_polygon = fill_palette_c_polygon,
    ggblanket.fill_palette_c_box = fill_palette_c_box
  )
}
