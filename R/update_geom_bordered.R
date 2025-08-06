#' Update the border geom defaults
#'
#' @description
#' Sets global options for border geom transformations.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param border_transform_colour A function with input of the set `col`. Defaults to `col_screen`/`col_multiply` based on the panel.
#' @param border_transform_fill A function with input of the set `col`. Defaults to NULL.
#' @param borderwidth A number, or a function with input of the set linewidth. Defaults to 0.25.
#'
#' @return Global options for border geom styling.
#'
#' @export
update_geom_border <- function(
  ...,
  border_transform_colour = \(x) {
    ifelse(is_panel_light(), col_multiply(x), col_screen(x))
  },
  border_transform_fill = NULL,
  borderwidth = 0.25
) {
  options(
    ggblanket.border_transform_colour = border_transform_colour,
    ggblanket.border_transform_fill = border_transform_fill
  )

  if (is.function(borderwidth)) {
    borderwidth <- borderwidth(
      ggplot2::get_geom_defaults("line")$linewidth
    )
  }

  ggplot2::update_theme(
    geom.area = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    ),
    geom.bin2d = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    ),
    geom.bar = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    ),
    geom.boxplot = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    ),
    geom.col = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    ),
    geom.contour_filled = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    ),
    geom.crossbar = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    ),
    geom.density = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    ),
    geom.density2d_filled = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    ),
    geom.hex = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    ),
    geom.map = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    ),
    geom.polygon = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    ),
    geom.raster = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    ),
    geom.rect = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    ),
    geom.ribbon = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    ),
    geom.sf = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    ),
    geom.tile = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    ),
    geom.violin = ggplot2::element_geom(
      linewidth = borderwidth,
      borderwidth = borderwidth
    )
  )
}
