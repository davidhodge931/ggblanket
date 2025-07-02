#' Update the linetype for some geoms
#'
#' @description
#' Update the linetype for geoms with unnecessary border lines to 0. Excludes boxplot, crossbar and smooth.
#'
#' @param linetype A default linetype for most geoms without borders. Defaults to 1. Defaults to 1.
#' @param linetype_sf A default linetype for sf geoms. Defaults to 0.
#' @param linetype_box A default linetype for boxplot and crossbar geoms. Defaults to 1.
#' @param linetype_border A default linetype for polygon-is geoms (other than boxplot, crossbar or sf). Defaults to 0.
#' @param ... Provided to require argument naming, support trailing commas etc.
#'
#' @return Updated geom defaults for linetype
#' @export
#'
weave_geom_linetype <- function(
    linetype = 1,
    linetype_border = 0,
    linetype_box = 1,
    linetype_sf = 0,
    ...) {


  ggplot2::update_theme(
    # sf
    geom.sf = ggplot2::element_geom(linetype = linetype_sf),
    # box
    geom.boxplot = ggplot2::element_geom(bordertype = linetype_box),
    geom.crossbar = ggplot2::element_geom(bordertype = linetype_box),

    # border
    geom.area = ggplot2::element_geom(bordertype = linetype_border),
    geom.bar = ggplot2::element_geom(bordertype = linetype_border),
    geom.bin2d = ggplot2::element_geom(bordertype = linetype_border),
    geom.col = ggplot2::element_geom(bordertype = linetype_border),
    geom.contour_filled = ggplot2::element_geom(bordertype = linetype_border),
    geom.density = ggplot2::element_geom(bordertype = linetype_border),
    geom.density_2d_filled = ggplot2::element_geom(bordertype = linetype_border),
    geom.dotplot = ggplot2::element_geom(bordertype = linetype_border),
    geom.hex = ggplot2::element_geom(bordertype = linetype_border),
    geom.map = ggplot2::element_geom(bordertype = linetype_border),
    geom.polygon = ggplot2::element_geom(bordertype = linetype_border),
    geom.raster = ggplot2::element_geom(bordertype = linetype_border),
    geom.rect = ggplot2::element_geom(bordertype = linetype_border),
    geom.ribbon = ggplot2::element_geom(bordertype = linetype_border),
    geom.tile = ggplot2::element_geom(bordertype = linetype_border),
    geom.violin = ggplot2::element_geom(bordertype = linetype_border),

    # other
    geom.contour = ggplot2::element_geom(linetype = linetype),
    geom.count = ggplot2::element_geom(linetype = linetype),
    geom.curve = ggplot2::element_geom(linetype = linetype),
    geom.density2d = ggplot2::element_geom(linetype = linetype),
    geom.errorbar = ggplot2::element_geom(linetype = linetype),
    geom.freqpoly = ggplot2::element_geom(linetype = linetype),
    geom.function = ggplot2::element_geom(linetype = linetype),
    geom.jitter = ggplot2::element_geom(linetype = linetype),
    geom.line = ggplot2::element_geom(linetype = linetype),
    geom.linerange = ggplot2::element_geom(linetype = linetype),
    geom.path = ggplot2::element_geom(linetype = linetype),
    geom.point = ggplot2::element_geom(linetype = linetype),
    geom.pointrange = ggplot2::element_geom(linetype = linetype),
    geom.qq = ggplot2::element_geom(linetype = linetype),
    geom.qq_line = ggplot2::element_geom(linetype = linetype),
    geom.quantile = ggplot2::element_geom(linetype = linetype),
    geom.rug = ggplot2::element_geom(linetype = linetype),
    geom.segment = ggplot2::element_geom(linetype = linetype),
    geom.smooth = ggplot2::element_geom(linetype = linetype),
    geom.spoke = ggplot2::element_geom(linetype = linetype),
    geom.step = ggplot2::element_geom(linetype = linetype),
  )
}
