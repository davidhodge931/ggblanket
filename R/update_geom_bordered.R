#' Update the border geom defaults
#'
#' @description
#' Sets global options for border geom transformations.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param colour_bordered_transform A function with input of the set `col`. Defaults to `blend_screen`/`blend_multiply` based on the panel.
#' @param fill_bordered_transform A function with input of the set `col`. Defaults to NULL.
#' @param linewidth_bordered A number, or a function with input of the set linewidth. Defaults to 0.25.
#'
#' @return Global options for border geom styling.
#'
#' @export
update_geom_bordered <- function(
  ...,
  # colour_bordered_transform = \(x) {
  #   ifelse(is_panel_light(), blend_multiply(x), blend_screen(x))
  # },
  colour_bordered_transform = \(x) {
    if (is_panel_light()) {
      blend_multiply(x)
    } else {
      blend_screen(x)
    }
  },

  fill_bordered_transform = NULL,
  linewidth_bordered = 0.25
) {
  options(
    ggblanket.colour_bordered_transform = colour_bordered_transform,
    ggblanket.fill_bordered_transform = fill_bordered_transform
  )

  if (is.function(linewidth_bordered)) {
    linewidth_bordered <- linewidth_bordered(
      ggplot2::get_geom_defaults("line")$linewidth
    )
  }

  ggplot2::update_theme(
    geom.area = ggplot2::element_geom(
      linewidth = linewidth_bordered,
      borderwidth = linewidth_bordered
    ),
    geom.bin2d = ggplot2::element_geom(
      linewidth = linewidth_bordered,
      borderwidth = linewidth_bordered
    ),
    geom.bar = ggplot2::element_geom(
      linewidth = linewidth_bordered,
      borderwidth = linewidth_bordered
    ),
    geom.boxplot = ggplot2::element_geom(
      linewidth = linewidth_bordered,
      borderwidth = linewidth_bordered
    ),
    geom.col = ggplot2::element_geom(
      linewidth = linewidth_bordered,
      borderwidth = linewidth_bordered
    ),
    geom.contour_filled = ggplot2::element_geom(
      linewidth = linewidth_bordered,
      borderwidth = linewidth_bordered
    ),
    geom.crossbar = ggplot2::element_geom(
      linewidth = linewidth_bordered,
      borderwidth = linewidth_bordered
    ),
    geom.density = ggplot2::element_geom(
      linewidth = linewidth_bordered,
      borderwidth = linewidth_bordered
    ),
    geom.density2d_filled = ggplot2::element_geom(
      linewidth = linewidth_bordered,
      borderwidth = linewidth_bordered
    ),
    geom.hex = ggplot2::element_geom(
      linewidth = linewidth_bordered,
      borderwidth = linewidth_bordered
    ),
    geom.map = ggplot2::element_geom(
      linewidth = linewidth_bordered,
      borderwidth = linewidth_bordered
    ),
    geom.polygon = ggplot2::element_geom(
      linewidth = linewidth_bordered,
      borderwidth = linewidth_bordered
    ),
    geom.raster = ggplot2::element_geom(
      linewidth = linewidth_bordered,
      borderwidth = linewidth_bordered
    ),
    geom.rect = ggplot2::element_geom(
      linewidth = linewidth_bordered,
      borderwidth = linewidth_bordered
    ),
    geom.ribbon = ggplot2::element_geom(
      linewidth = linewidth_bordered,
      borderwidth = linewidth_bordered
    ),
    geom.sf = ggplot2::element_geom(
      linewidth = linewidth_bordered,
      borderwidth = linewidth_bordered
    ),
    geom.tile = ggplot2::element_geom(
      linewidth = linewidth_bordered,
      borderwidth = linewidth_bordered
    ),
    geom.violin = ggplot2::element_geom(
      linewidth = linewidth_bordered,
      borderwidth = linewidth_bordered
    )
  )
}
