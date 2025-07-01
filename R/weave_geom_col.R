#' #' Update the colour and fill for geoms
#' #'
#' #' @description
#' #' Update the colour and fill for most geoms. Excludes "text", "label", "hline", and "vline".
#' #' Uses a hierarchical system where more specific parameters override more general ones.
#' #'
#' #' @param col A default hex code for the colour and fill of most geoms. Defaults to NULL.
#' #' @param colour A default hex code for the colour of most geoms. Defaults to NULL.
#' #' @param fill A default hex code for the fill of most geoms. Defaults to NULL.
#' #' @param ... Provided to require argument naming, support trailing commas etc.
#' #'
#' #' @details
#' #' The function uses a hierarchical priority system where more specific parameters
#' #' override more general ones. The priority order is:
#' #' 1. `colour`/`fill` (most specific)
#' #' 2. `col` (general)
#' #' 3. `blue` ("#357BA2FF") as final fallback
#' #'
#' #' @return Updated geom defaults for colour and fill
#' #' @export
#' #'
#' #' @examples
#' #' # Use default blue colour
#' #' weave_geom_col()
#' #'
#' #' # Set a general colour for all geoms
#' #' weave_geom_col(col = "#E74C3C")
#' #'
#' #' # Override colour and fill separately
#' #' weave_geom_col(colour = "#3498DB", fill = "#2ECC71")
#' #'
#' #' # Set general colour but override just fill
#' #' weave_geom_col(col = "#9B59B6", fill = "#E67E22")
#' weave_geom_col <- function(col = NULL,
#'                            colour = NULL,
#'                            colour_border = NULL,
#'                            fill = NULL,
#'                            fill_border = NULL,
#'                            ...) {
#'   # Determine colour (most specific to most general)
#'   colour <- colour %||% col %||% blue
#'   # Determine fill (most specific to most general)
#'   fill <- fill %||% col %||% blue
#'   ggplot2::update_theme(
#'     #colour and fill
#'     geom.area = ggplot2::element_geom(colour = colour, fill = fill),
#'     geom.bar = ggplot2::element_geom(colour = colour, fill = fill),
#'     geom.bin2d = ggplot2::element_geom(colour = colour, fill = fill),
#'     geom.boxplot = ggplot2::element_geom(colour = colour, fill = fill),
#'     geom.col = ggplot2::element_geom(colour = colour, fill = fill),
#'     geom.contour_filled = ggplot2::element_geom(colour = colour, fill = fill),
#'     geom.crossbar = ggplot2::element_geom(colour = colour, fill = fill),
#'     geom.density = ggplot2::element_geom(colour = colour, fill = fill),
#'     geom.density_2d_filled = ggplot2::element_geom(colour = colour, fill = fill),
#'     geom.dotplot = ggplot2::element_geom(colour = colour, fill = fill),
#'     geom.hex = ggplot2::element_geom(colour = colour, fill = fill),
#'     geom.map = ggplot2::element_geom(colour = colour, fill = fill),
#'     geom.polygon = ggplot2::element_geom(colour = colour, fill = fill),
#'     geom.raster = ggplot2::element_geom(colour = colour, fill = fill),
#'     geom.rect = ggplot2::element_geom(colour = colour, fill = fill),
#'     geom.ribbon = ggplot2::element_geom(colour = colour, fill = fill),
#'     geom.sf = ggplot2::element_geom(colour = colour, fill = fill),
#'     geom.smooth = ggplot2::element_geom(colour = colour, fill = fill),
#'     geom.tile = ggplot2::element_geom(colour = colour, fill = fill),
#'     geom.violin = ggplot2::element_geom(colour = colour, fill = fill),
#'     #colour only
#'     geom.abline = ggplot2::element_geom(colour = colour),
#'     geom.contour = ggplot2::element_geom(colour = colour),
#'     geom.count = ggplot2::element_geom(colour = colour, fill = colour),
#'     geom.curve = ggplot2::element_geom(colour = colour),
#'     geom.density2d = ggplot2::element_geom(colour = colour),
#'     geom.errorbar = ggplot2::element_geom(colour = colour),
#'     geom.freqpoly = ggplot2::element_geom(colour = colour),
#'     geom.function = ggplot2::element_geom(colour = colour),
#'     geom.jitter = ggplot2::element_geom(colour = colour, fill = colour),
#'     geom.line = ggplot2::element_geom(colour = colour),
#'     geom.linerange = ggplot2::element_geom(colour = colour),
#'     geom.path = ggplot2::element_geom(colour = colour),
#'     geom.point = ggplot2::element_geom(colour = colour, fill = colour),
#'     geom.pointrange = ggplot2::element_geom(colour = colour, fill = colour),
#'     geom.qq = ggplot2::element_geom(colour = colour, fill = colour),
#'     geom.qq_line = ggplot2::element_geom(colour = colour),
#'     geom.quantile = ggplot2::element_geom(colour = colour),
#'     geom.rug = ggplot2::element_geom(colour = colour),
#'     geom.segment = ggplot2::element_geom(colour = colour),
#'     geom.spoke = ggplot2::element_geom(colour = colour),
#'     geom.step = ggplot2::element_geom(colour = colour),
#'   )
#' }
