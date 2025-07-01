#' Update the colour and fill for geoms
#'
#' @description
#' Update the colour and fill for most geoms. Excludes "text", "label", "hline", and "vline".
#' Uses a hierarchical system where more specific parameters override more general ones.
#'
#' @param col A default hex code for the colour and fill of most geoms. Defaults to NULL.
#' @param colour A default hex code for the colour of most geoms. Defaults to NULL.
#' @param fill A default hex code for the fill of most geoms. Defaults to NULL.
#' @param ... Provided to require argument naming, support trailing commas etc.
#'
#' @details
#' The function uses a hierarchical priority system where more specific parameters
#' override more general ones. The priority order is:
#' 1. `colour`/`fill` (most specific)
#' 2. `col` (general)
#' 3. `blue` ("#357BA2FF") as final fallback
#'
#' @return Updated geom defaults for colour and fill
#' @export
#'
#' @examples
#' # Use default blue colour
#' weave_geom_col()
#'
#' # Set a general colour for all geoms
#' weave_geom_col(col = "#E74C3C")
#'
#' # Override colour and fill separately
#' weave_geom_col(colour = "#3498DB", fill = "#2ECC71")
#'
#' # Set general colour but override just fill
#' weave_geom_col(col = "#9B59B6", fill = "#E67E22")
weave_geom_col <- function(col = NULL,
                           colour = NULL,
                           colour_border = NULL,
                           colour_box = NULL,
                           colour_sf = NULL,
                           fill = NULL,
                           fill_border = NULL,
                           fill_box = NULL,
                           fill_sf = NULL,
                           ...) {

  # Determine from most specific to most general
  colour_sf <- colour_sf %||% colour_border %||% colour %||% col %||% blue
  fill_sf <- fill_sf %||% fill_border %||% fill %||% col %||% blue

  colour_box <- colour_box %||% colour_border %||% colour %||% col %||% blue
  fill_box <- fill_box %||% fill_border %||% fill %||% col %||% blue

  colour_border <- colour_border %||% colour %||% col %||% blue
  fill_border <- fill_border %||% fill %||% col %||% blue

  colour <- colour %||% col %||% blue
  fill <- fill %||% col %||% blue

  ggplot2::update_theme(
    # sf
    geom.sf = ggplot2::element_geom(colour = colour_sf, fill = fill_sf),

    # box
    geom.boxplot = ggplot2::element_geom(colour = colour_box, fill = fill_box),
    geom.crossbar = ggplot2::element_geom(colour = colour_box, fill = fill_box),

    # border
    geom.area = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.bar = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.bin2d = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.col = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.contour_filled = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.density = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.density_2d_filled = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.dotplot = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.hex = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.map = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.polygon = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.raster = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.rect = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.ribbon = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.smooth = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.tile = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.violin = ggplot2::element_geom(colour = colour_border, fill = fill_border),

    # other
    geom.abline = ggplot2::element_geom(colour = colour),
    geom.contour = ggplot2::element_geom(colour = colour),
    geom.count = ggplot2::element_geom(colour = colour, fill = colour),
    geom.curve = ggplot2::element_geom(colour = colour),
    geom.density2d = ggplot2::element_geom(colour = colour),
    geom.errorbar = ggplot2::element_geom(colour = colour),
    geom.freqpoly = ggplot2::element_geom(colour = colour),
    geom.function = ggplot2::element_geom(colour = colour),
    geom.jitter = ggplot2::element_geom(colour = colour, fill = colour),
    geom.line = ggplot2::element_geom(colour = colour),
    geom.linerange = ggplot2::element_geom(colour = colour),
    geom.path = ggplot2::element_geom(colour = colour),
    geom.point = ggplot2::element_geom(colour = colour, fill = colour),
    geom.pointrange = ggplot2::element_geom(colour = colour, fill = colour),
    geom.qq = ggplot2::element_geom(colour = colour, fill = colour),
    geom.qq_line = ggplot2::element_geom(colour = colour),
    geom.quantile = ggplot2::element_geom(colour = colour),
    geom.rug = ggplot2::element_geom(colour = colour),
    geom.segment = ggplot2::element_geom(colour = colour),
    geom.spoke = ggplot2::element_geom(colour = colour),
    geom.step = ggplot2::element_geom(colour = colour),
  )
}
