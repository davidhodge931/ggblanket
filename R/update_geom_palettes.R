#' Update the default geom palettes
#'
#' @description
#' Updates the active theme for consistent colour/fill palette styling.
#' Sets global options for other palettes.
#'
#' @param col_palette_d For a discrete colour/fill scale, a character vector or a `scales::pal_*` function.
#' @param col_palette_c For a continuous colour/fill scale, a character vector or a `scales::pal_*` function.
#' @param col_palette_o For a ordinal colour/fill scale, a `scales::pal_*` function. If NULL, determined from `col_palette_c`.
#' @param col_na A hex code (or name) for the `NA` value.
#' @param shape_palette_d For shape scales, a numeric vector of shape codes. Defaults to c(21, 24, 22, 23, 25).
#' @param linetype_palette_d For linetype scales, a character vector or a `scales::pal_*` function. Defaults to 1:6.
#' @param ... Additional arguments (not used).
#'
#' @return An updated ggplot2 theme and global options.
#'
#' @export
update_geom_palettes <- function(
  col_palette_d = scales::pal_hue(),
  col_palette_c = pal_viridis_by_panel("mako", 0.1, 0.9),
  col_palette_o = NULL,
  col_na = "#A6A6A6FF",
  shape_palette_d = c(21, 24, 22, 23, 25),
  linetype_palette_d = 1:6,
  ...
) {
  # Update theme-level palettes
  ggplot2::update_theme(
    palette.colour.discrete = col_palette_d,
    palette.fill.discrete = col_palette_d,
    palette.colour.continuous = col_palette_c,
    palette.fill.continuous = col_palette_c
  )

  # Handle ordinal palette default
  if (rlang::is_null(col_palette_o)) {
    # If continuous palette is a vector, convert to gradient function
    if (is.character(col_palette_c) || is.numeric(col_palette_c)) {
      col_palette_o <- scales::pal_gradient_n(colours = col_palette_c)
    } else if (is.function(col_palette_c)) {
      # If it's already a function, use it directly
      col_palette_o <- col_palette_c
    }
  }

  # Set global ggblanket options for the rest
  options(
    # Ordinal col palette
    ggblanket.col_palette_o = col_palette_o,

    # NA col value
    ggblanket.col_na = col_na,

    # Other palettes
    ggblanket.shape_palette_d = shape_palette_d,
    ggblanket.linetype_palette_d = linetype_palette_d
  )
}
