#' Update the default geom palettes
#'
#' @description
#' Updates the active theme for consistent colour/fill palette styling.
#' Sets global options for other palettes.
#'
#' @param col_palette_discrete For a discrete colour/fill scale, a character vector or a `scales::pal_*` function.
#' @param col_palette_continuous For a continuous colour/fill scale, a character vector or a `scales::pal_*` function.
#' @param col_palette_ordinal For a ordinal colour/fill scale, a `scales::pal_*` function. If NULL, determined from `col_palette_continuous`.
#' @param col_palette_na A NA colour/fill value.
#' @param shape_palette_discrete For shape scales, a numeric vector of shape codes. Defaults to c(21, 24, 22, 23, 25).
#' @param linetype_palette_discrete For linetype scales, a character vector or a `scales::pal_*` function. Defaults to 1:6.
#' @param ... Additional arguments (not used).
#'
#' @return An updated ggplot2 theme and global options.
#'
#' @export
update_geom_palettes <- function(
    col_palette_discrete = scales::pal_hue(),
    col_palette_continuous = viridis_by_theme(n = 256, begin = 0.05, end = 0.95, option = "G"),
    col_palette_ordinal = NULL,
    col_palette_na = "#A6A6A6FF",
    shape_palette_discrete = c(21, 24, 22, 23, 25),
    linetype_palette_discrete = 1:6,
    ...
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
    ggblanket.col_palette_na = col_palette_na,

    # Other palettes
    ggblanket.shape_palette_discrete = shape_palette_discrete,
    ggblanket.linetype_palette_discrete = linetype_palette_discrete
  )
}

#' Update the border geom defaults
#'
#' @description
#' Sets global options for border geom transformations.
#'
#' @param ... Additional arguments (not used).
#' @param border_colour A function with input of `col`. Defaults to screen/multiply based on theme.
#' @param border_fill A function with input of `col`. Defaults to NULL.
#' @param border_linewidth A function with input of `linewidth`. Defaults to \(x) x / 2.64.
#'
#' @return Global options for border geom styling.
#'
#' @export
update_geom_border <- function(
    ...,
    border_colour = \(x) ifelse(is_theme_dark(), col_screen(x), col_multiply(x)),
    border_fill = NULL,
    border_linewidth = 0.25
) {
  options(
    ggblanket.border_colour = border_colour,
    ggblanket.border_fill = border_fill
  )

  ggplot2::update_theme(
    geom.area = ggplot2::element_geom(linewidth = border_linewidth, borderwidth = border_linewidth),
    geom.bin2d = ggplot2::element_geom(linewidth = border_linewidth, borderwidth = border_linewidth),
    geom.bar = ggplot2::element_geom(linewidth = border_linewidth, borderwidth = border_linewidth),
    geom.boxplot = ggplot2::element_geom(linewidth = border_linewidth, borderwidth = border_linewidth),
    geom.col = ggplot2::element_geom(linewidth = border_linewidth, borderwidth = border_linewidth),
    geom.contour_filled = ggplot2::element_geom(linewidth = border_linewidth, borderwidth = border_linewidth),
    geom.crossbar = ggplot2::element_geom(linewidth = border_linewidth, borderwidth = border_linewidth),
    geom.density = ggplot2::element_geom(linewidth = border_linewidth, borderwidth = border_linewidth),
    geom.density2d_filled = ggplot2::element_geom(linewidth = border_linewidth, borderwidth = border_linewidth),
    geom.hex = ggplot2::element_geom(linewidth = border_linewidth, borderwidth = border_linewidth),
    geom.map = ggplot2::element_geom(linewidth = border_linewidth, borderwidth = border_linewidth),
    geom.polygon = ggplot2::element_geom(linewidth = border_linewidth, borderwidth = border_linewidth),
    geom.raster = ggplot2::element_geom(linewidth = border_linewidth, borderwidth = border_linewidth),
    geom.rect = ggplot2::element_geom(linewidth = border_linewidth, borderwidth = border_linewidth),
    geom.ribbon = ggplot2::element_geom(linewidth = border_linewidth, borderwidth = border_linewidth),
    # sf inherits from point/line, so not required
    geom.tile = ggplot2::element_geom(linewidth = border_linewidth, borderwidth = border_linewidth),
    geom.violin = ggplot2::element_geom(linewidth = border_linewidth, borderwidth = border_linewidth)
  )
}

