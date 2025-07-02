#' Update the linewidth for geoms
#'
#' @description
#' Update the linewidth for most geoms. Excludes "text", "label", "hline", and "vline".
#'
#' @param linewidth A default linewidth for most geoms. Defaults to 0.66.
#' @param ... Provided to require argument naming, support trailing commas etc.
#'
#' @return Updated geom defaults for linewidth
#' @export
weave_geom_linewidth <- function(linewidth = 0.66, ...) {
  #includes polygons (with borders)
  ggplot2::update_theme(
    geom.area = ggplot2::element_geom(borderwidth = linewidth),
    geom.bar = ggplot2::element_geom(borderwidth = linewidth),
    geom.bin2d = ggplot2::element_geom(borderwidth = linewidth),
    geom.boxplot = ggplot2::element_geom(borderwidth = linewidth),
    geom.col = ggplot2::element_geom(borderwidth = linewidth),
    geom.contour_filled = ggplot2::element_geom(borderwidth = linewidth),
    geom.crossbar = ggplot2::element_geom(borderwidth = linewidth),
    geom.density = ggplot2::element_geom(borderwidth = linewidth),
    geom.density_2d_filled = ggplot2::element_geom(borderwidth = linewidth),
    geom.dotplot = ggplot2::element_geom(borderwidth = linewidth),
    geom.hex = ggplot2::element_geom(borderwidth = linewidth),
    geom.map = ggplot2::element_geom(borderwidth = linewidth),
    geom.polygon = ggplot2::element_geom(borderwidth = linewidth),
    geom.raster = ggplot2::element_geom(borderwidth = linewidth),
    geom.rect = ggplot2::element_geom(borderwidth = linewidth),
    geom.ribbon = ggplot2::element_geom(borderwidth = linewidth),
    geom.sf = ggplot2::element_geom(borderwidth = linewidth),
    geom.tile = ggplot2::element_geom(borderwidth = linewidth),
    geom.violin = ggplot2::element_geom(borderwidth = linewidth),
    #else includes points
    geom.count = ggplot2::element_geom(linewidth = linewidth),
    geom.jitter = ggplot2::element_geom(linewidth = linewidth),
    geom.point = ggplot2::element_geom(linewidth = linewidth),
    geom.pointrange = ggplot2::element_geom(linewidth = linewidth),
    geom.qq = ggplot2::element_geom(linewidth = linewidth),
    #else just lines
    geom.contour = ggplot2::element_geom(linewidth = linewidth),
    geom.curve = ggplot2::element_geom(linewidth = linewidth),
    geom.density2d = ggplot2::element_geom(linewidth = linewidth),
    geom.errorbar = ggplot2::element_geom(linewidth = linewidth),
    geom.freqpoly = ggplot2::element_geom(linewidth = linewidth),
    geom.function = ggplot2::element_geom(linewidth = linewidth),
    geom.line = ggplot2::element_geom(linewidth = linewidth),
    geom.linerange = ggplot2::element_geom(linewidth = linewidth),
    geom.path = ggplot2::element_geom(linewidth = linewidth),
    geom.qq_line = ggplot2::element_geom(linewidth = linewidth),
    geom.quantile = ggplot2::element_geom(linewidth = linewidth),
    geom.rug = ggplot2::element_geom(linewidth = linewidth),
    geom.segment = ggplot2::element_geom(linewidth = linewidth),
    geom.smooth = ggplot2::element_geom(linewidth = linewidth),
    geom.spoke = ggplot2::element_geom(linewidth = linewidth),
    geom.step = ggplot2::element_geom(linewidth = linewidth),
  )
}
