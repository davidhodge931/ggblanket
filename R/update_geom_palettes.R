#' Update the default geom palettes
#'
#' @description
#' Updates the active theme for consistent colour/fill palette styling.
#' Sets global options for other palettes.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param col_palette_discrete For a discrete colour/fill scale, a character vector or a `scales::pal_*` function.
#' @param col_palette_continuous For a continuous colour/fill scale, a character vector or a `scales::pal_*` function.
#' @param col_palette_ordinal For a ordinal colour/fill scale, a `scales::pal_*` function. If NULL, determined from `col_palette_continuous`.
#' @param col_na A hex code (or name) for the `NA` value.
#' @param shape_palette_d For shape scales, a numeric vector of shape codes. Defaults to c(21, 24, 22, 23, 25).
#' @param shape_na A NA shape value.
#' @param linetype_palette_d For linetype scales, a character vector or a `scales::pal_*` function. Defaults to 1:6.
#'
#' @return An updated ggplot2 theme and global options.
#'
#' @export
update_geom_palettes <- function(
    ...,
    col_palette_discrete = scales::pal_hue(),
    col_palette_continuous = direction(scales::pal_viridis(option = "mako", begin = 0.1, end = 0.9)),
    col_palette_ordinal = NULL,
    col_na = "#A6A6A6FF",
    shape_palette_d = c(21, 24, 22, 23, 25),
    shape_na = 4,
    linetype_palette_d = 1:6
) {
  # Update theme-level palettes
  ggplot2::update_theme(
    palette.colour.discrete = col_palette_discrete,
    palette.fill.discrete = col_palette_discrete,
    palette.colour.continuous = col_palette_continuous,
    palette.fill.continuous = col_palette_continuous
  )

  # Handle ordinal palette default
  if (rlang::is_null(col_palette_ordinal)) {
    # If continuous palette is a vector, convert to gradient function
    if (is.character(col_palette_continuous) || is.numeric(col_palette_continuous)) {
      col_palette_ordinal <- scales::pal_gradient_n(colours = col_palette_continuous)
    } else if (is.function(col_palette_continuous)) {
      # If it's already a function, use it directly
      col_palette_ordinal <- col_palette_continuous
    }
  }

  # Set global ggblanket options for the rest
  options(
    # Ordinal col palette
    ggblanket.col_palette_ordinal = col_palette_ordinal,

    # NA col value
    ggblanket.col_na = col_na,
    ggblanket.shape_na = shape_na,

    # Other palettes
    ggblanket.shape_palette_d = shape_palette_d,
    ggblanket.linetype_palette_d = linetype_palette_d
  )
}
