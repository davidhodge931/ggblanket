#' Update the border geom defaults
#'
#' @description
#' Sets global options for border geom transformations.
#'
#' @param ... Additional arguments (not used).
#' @param border_colour_transform A function with input of the set `col`. Defaults to `col_screen`/`col_multiply` based on the panel.
#' @param border_fill_transform A function with input of the set `col`. Defaults to NULL.
#' @param border_linewidth A number, or a function with input of the set linewidth. Defaults to 0.25.
#'
#' @return Global options for border geom styling.
#'
#' @export
update_geom_border <- function(
    ...,
    border_colour_transform = \(x) {
      ifelse(is_panel_dark(), col_screen(x), col_multiply(x))
    },
    border_fill_transform = NULL,
    border_linewidth = 0.25
) {
  options(
    ggblanket.border_colour_transform = border_colour_transform,
    ggblanket.border_fill_transform = border_fill_transform
  )

  if (is.function(border_linewidth)) {
    border_linewidth <- border_linewidth(ggplot2::get_geom_defaults("line")$linewidth)
  }

  ggplot2::update_theme(
    geom.area = ggplot2::element_geom(
      linewidth = border_linewidth,
      borderwidth = border_linewidth
    ),
    geom.bin2d = ggplot2::element_geom(
      linewidth = border_linewidth,
      borderwidth = border_linewidth
    ),
    geom.bar = ggplot2::element_geom(
      linewidth = border_linewidth,
      borderwidth = border_linewidth
    ),
    geom.boxplot = ggplot2::element_geom(
      linewidth = border_linewidth,
      borderwidth = border_linewidth
    ),
    geom.col = ggplot2::element_geom(
      linewidth = border_linewidth,
      borderwidth = border_linewidth
    ),
    geom.contour_filled = ggplot2::element_geom(
      linewidth = border_linewidth,
      borderwidth = border_linewidth
    ),
    geom.crossbar = ggplot2::element_geom(
      linewidth = border_linewidth,
      borderwidth = border_linewidth
    ),
    geom.density = ggplot2::element_geom(
      linewidth = border_linewidth,
      borderwidth = border_linewidth
    ),
    geom.density2d_filled = ggplot2::element_geom(
      linewidth = border_linewidth,
      borderwidth = border_linewidth
    ),
    geom.hex = ggplot2::element_geom(
      linewidth = border_linewidth,
      borderwidth = border_linewidth
    ),
    geom.map = ggplot2::element_geom(
      linewidth = border_linewidth,
      borderwidth = border_linewidth
    ),
    geom.polygon = ggplot2::element_geom(
      linewidth = border_linewidth,
      borderwidth = border_linewidth
    ),
    geom.raster = ggplot2::element_geom(
      linewidth = border_linewidth,
      borderwidth = border_linewidth
    ),
    geom.rect = ggplot2::element_geom(
      linewidth = border_linewidth,
      borderwidth = border_linewidth
    ),
    geom.ribbon = ggplot2::element_geom(
      linewidth = border_linewidth,
      borderwidth = border_linewidth
    ),
    geom.sf = ggplot2::element_geom(
      linewidth = border_linewidth,
      borderwidth = border_linewidth
    ),
    geom.tile = ggplot2::element_geom(
      linewidth = border_linewidth,
      borderwidth = border_linewidth
    ),
    geom.violin = ggplot2::element_geom(
      linewidth = border_linewidth,
      borderwidth = border_linewidth
    )
  )
}
