#' Update the border geom defaults
#'
#' @description
#' Sets global options for border geom transformations.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param colour_border_transform A function with input of the set `col`. Defaults to `blend_screen`/`blend_multiply` based on the panel.
#' @param fill_border_transform A function with input of the set `col`. Defaults to NULL.
#' @param linewidth_border A number, or a function with input of the set linewidth.
#'
#' @return Global options for border geom styling.
#'
#' @export
update_geom_border <- function(
  ...,
  colour_border_transform = \(x) {
    if (is_panel_dark()) {
      blend_screen(x)
    } else {
      blend_multiply(x)
    }
  },

  fill_border_transform = NULL,
  linewidth_border = \(x) x * 0.3787879
) {
  options(
    ggblanket.colour_border_transform = colour_border_transform,
    ggblanket.fill_border_transform = fill_border_transform
  )

  if (is.function(linewidth_border)) {
    linewidth_border <- linewidth_border(
      ggplot2::get_geom_defaults("line")$linewidth
    )
  }

  ggplot2::update_theme(
    geom.area = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
    ),
    geom.bin2d = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
    ),
    geom.bar = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
    ),
    geom.boxplot = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
    ),
    geom.col = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
    ),
    geom.contour_filled = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
    ),
    geom.crossbar = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
    ),
    geom.density = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
    ),
    geom.density2d_filled = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
    ),
    geom.hex = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
    ),
    geom.map = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
    ),
    geom.polygon = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
    ),
    geom.raster = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
    ),
    geom.rect = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
    ),
    geom.ribbon = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
    ),
    geom.sf = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
    ),
    geom.tile = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
    ),
    geom.violin = ggplot2::element_geom(
      linewidth = linewidth_border,
      borderwidth = linewidth_border
    )
  )
}
