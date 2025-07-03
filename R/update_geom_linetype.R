#' Update the linetype for geoms
#'
#' @description
#' Update the linetype defaults for ggplot2 geoms using a hierarchical system.
#' More specific parameters override more general ones, allowing fine-grained control
#' over different geom categories.
#'
#' @param linetype A default linetype for geoms that don't fall into other categories
#'   (e.g., point, line, path). Defaults to 1 (solid line).
#' @param ... Provided to require argument naming and support trailing commas.
#' @param linetype_border The linetype for polygon geoms (excluding boxplot, crossbar). Defaults to 0 (no line).
#' @param linetype_box The linetype for boxplot and crossbar geoms specifically.
#'   Defaults to 1 (solid line).
#'
#' @details
#' The default behaviour allows polygon-based visualizations allows for neat
#' display of adjacent shapes. Set `linetype_border = 1` to restore visible
#' borders.
#'
#' @section Geom Categories:
#' Geoms are grouped into categories for consistent styling:
#' \describe{
#'   \item{**border**}{Polygon geoms (excluding boxplot, crossbar) where
#'     borders are often unnecessary: area, bar, col, density, dotplot, map,
#'     polygon, rect, ribbon, tile, violin.}
#'   \item{**box**}{Specifically boxplot and crossbar geoms}
#'   \item{**other**}{Line-based and point geoms: contour, count, curve, density2d,
#'     errorbar, freqpoly, function, jitter, line, linerange, path, point,
#'     pointrange, qq, qq_line, quantile, rug, segment, smooth, spoke, step}
#' }
#'
#' @note
#' Heatmap geoms (bin2d, hex, raster) and filled contour geoms (contour_filled,
#' density_2d_filled) always have their borders set to 0 regardless of the
#' `linetype` setting.
#'
#' @return An updated ggplot2 theme with modified geom linetype defaults
#' @export
#'
#' @examples
#' # Default: remove outlines from polygons for nice adjacency
#' update_geom_linetype()
#'
#' # Restore outlines on all border category geoms
#' update_geom_linetype(linetype_border = 1)
#'
#' # Dashed outlines for border category
#' update_geom_linetype(
#'   linetype_border = 2,  # dashed
#' )
#'
update_geom_linetype <- function(
    linetype = 1,
    ...,
    linetype_border = 0,
    linetype_box = 1) {
  ggplot2::update_theme(
    # box
    geom.boxplot = ggplot2::element_geom(linetype = linetype_box),
    geom.crossbar = ggplot2::element_geom(linetype = linetype_box),

    # border (using bordertype for polygon-based geoms)
    geom.area = ggplot2::element_geom(bordertype = linetype_border),
    geom.bar = ggplot2::element_geom(bordertype = linetype_border),
    geom.col = ggplot2::element_geom(bordertype = linetype_border),
    geom.density = ggplot2::element_geom(bordertype = linetype_border),
    geom.map = ggplot2::element_geom(bordertype = linetype_border),
    geom.polygon = ggplot2::element_geom(bordertype = linetype_border),
    geom.rect = ggplot2::element_geom(bordertype = linetype_border),
    geom.ribbon = ggplot2::element_geom(bordertype = linetype_border),
    geom.tile = ggplot2::element_geom(bordertype = linetype_border),
    geom.violin = ggplot2::element_geom(bordertype = linetype_border),

    # heatmaps - always remove borders
    geom.bin2d = ggplot2::element_geom(bordertype = 0),
    geom.hex = ggplot2::element_geom(bordertype = 0),
    geom.raster = ggplot2::element_geom(bordertype = 0),

    # filled contours/density2d - always remove borders
    geom.contour.filled = ggplot2::element_geom(bordertype = 0),
    geom.density2d.filled = ggplot2::element_geom(bordertype = 0),

    # other (using linetype for non-polygon geoms)
    geom.contour = ggplot2::element_geom(linetype = linetype),
    geom.count = ggplot2::element_geom(linetype = linetype),
    geom.curve = ggplot2::element_geom(linetype = linetype),
    geom.dotplot = ggplot2::element_geom(linetype = linetype),
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
