#' Update the bordered geom defaults
#'
#' @description
#' Sets global options for bordered geom transformations.
#'
#' @param ... Additional arguments (not used).
#' @param bordered_colour A function with input of the set `col`. Defaults to `col_screen`/`col_multiply` based on the panel.
#' @param bordered_fill A function with input of the set `col`. Defaults to NULL.
#' @param bordered_linewidth A number, or a function with input of the set linewidth. Defaults to 0.25.
#'
#' @return Global options for border geom styling.
#'
#' @export
update_geom_bordered <- function(
    ...,
    bordered_colour = \(x) {
      ifelse(is_panel_dark(), col_screen(x), col_multiply(x))
    },
    bordered_fill = NULL,
    bordered_linewidth = 0.25
) {
  options(
    ggblanket.bordered_colour = bordered_colour,
    ggblanket.bordered_fill = bordered_fill
  )

  if (is.function(bordered_linewidth)) {
    bordered_linewidth <- bordered_linewidth(ggplot2::get_geom_defaults("line")$linewidth)
  }

  ggplot2::update_theme(
    geom.area = ggplot2::element_geom(
      linewidth = bordered_linewidth,
      borderwidth = bordered_linewidth
    ),
    geom.bin2d = ggplot2::element_geom(
      linewidth = bordered_linewidth,
      borderwidth = bordered_linewidth
    ),
    geom.bar = ggplot2::element_geom(
      linewidth = bordered_linewidth,
      borderwidth = bordered_linewidth
    ),
    geom.boxplot = ggplot2::element_geom(
      linewidth = bordered_linewidth,
      borderwidth = bordered_linewidth
    ),
    geom.col = ggplot2::element_geom(
      linewidth = bordered_linewidth,
      borderwidth = bordered_linewidth
    ),
    geom.contour_filled = ggplot2::element_geom(
      linewidth = bordered_linewidth,
      borderwidth = bordered_linewidth
    ),
    geom.crossbar = ggplot2::element_geom(
      linewidth = bordered_linewidth,
      borderwidth = bordered_linewidth
    ),
    geom.density = ggplot2::element_geom(
      linewidth = bordered_linewidth,
      borderwidth = bordered_linewidth
    ),
    geom.density2d_filled = ggplot2::element_geom(
      linewidth = bordered_linewidth,
      borderwidth = bordered_linewidth
    ),
    geom.hex = ggplot2::element_geom(
      linewidth = bordered_linewidth,
      borderwidth = bordered_linewidth
    ),
    geom.map = ggplot2::element_geom(
      linewidth = bordered_linewidth,
      borderwidth = bordered_linewidth
    ),
    geom.polygon = ggplot2::element_geom(
      linewidth = bordered_linewidth,
      borderwidth = bordered_linewidth
    ),
    geom.raster = ggplot2::element_geom(
      linewidth = bordered_linewidth,
      borderwidth = bordered_linewidth
    ),
    geom.rect = ggplot2::element_geom(
      linewidth = bordered_linewidth,
      borderwidth = bordered_linewidth
    ),
    geom.ribbon = ggplot2::element_geom(
      linewidth = bordered_linewidth,
      borderwidth = bordered_linewidth
    ),
    # sf inherits from point/line, so not required
    geom.tile = ggplot2::element_geom(
      linewidth = bordered_linewidth,
      borderwidth = bordered_linewidth
    ),
    geom.violin = ggplot2::element_geom(
      linewidth = bordered_linewidth,
      borderwidth = bordered_linewidth
    )
  )
}
