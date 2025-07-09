#' Update the default linewidth
#'
#' @description
#' Updates the active theme to apply consistent linewidth styling.
#'
#' @param linewidth Default linewidth for most geoms.
#' @param linewidth_polygon Linewidth for polygon-type geoms.
#' @param linewidth_box Linewidth for box-type geoms.
#' @param linewidth_reference_line Linewidth for reference line geoms. If NULL, derived from axis line linewidth.
#' @param ... Additional arguments (not used).
#'
#' @return An updated ggplot2 theme.
#'
#' @noRd
update_geom_linewidth <- function(
    linewidth = 0.66,
    linewidth_polygon = 0.25,
    linewidth_box = linewidth_polygon,
    linewidth_reference_line = NULL,
    ...
) {

  # Get current theme for NULL defaults
  current_theme <- ggplot2::get_theme()

  # Handle reference line linewidth defaults
  linewidth_reference_line <- linewidth_reference_line %||%
    current_theme$axis.line.x.bottom$linewidth %||%
    current_theme$axis.line.x.top$linewidth %||%
    current_theme$axis.line.y.left$linewidth %||%
    current_theme$axis.line.y.right$linewidth %||%
    current_theme$axis.line.x$linewidth %||%
    current_theme$axis.line.y$linewidth %||%
    current_theme$axis.line$linewidth %||%
    0.25

  # Define geom categories
  polygon_geoms <- c("area", "bar", "bin2d", "col", "contour.filled",
                     "density", "density2d.filled", "hex", "histogram", "map",
                     "polygon", "raster", "rect", "ribbon", "tile", "violin")

  box_geoms <- c("boxplot", "crossbar")

  reference_line_geoms <- c("abline", "hline", "vline")

  # Build theme updates
  theme_args <- list()

  # Reference line geoms
  for (geom in reference_line_geoms) {
    geom_name <- paste0("geom.", geom)
    theme_args[[geom_name]] <- ggplot2::element_geom(linewidth = linewidth_reference_line)
  }

  # Polygon geoms
  for (geom in polygon_geoms) {
    geom_name <- paste0("geom.", geom)
    if (geom == "tile") {
      theme_args[[geom_name]] <- ggplot2::element_geom(borderwidth = linewidth_polygon / 0.4)
    } else {
      theme_args[[geom_name]] <- ggplot2::element_geom(borderwidth = linewidth_polygon)
    }
  }

  # Box geoms
  for (geom in box_geoms) {
    geom_name <- paste0("geom.", geom)
    theme_args[[geom_name]] <- ggplot2::element_geom(borderwidth = linewidth_box)
  }

  # Other geoms with linewidth
  other_geoms <- c("count", "jitter", "point", "pointrange", "qq",
                   "contour", "curve", "errorbar", "freqpoly", "function",
                   "line", "linerange", "path", "qq_line", "quantile", "rug",
                   "segment", "smooth", "spoke", "step", "density2d")

  for (geom in other_geoms) {
    geom_name <- paste0("geom.", geom)
    if (geom == "smooth") {
      theme_args[[geom_name]] <- ggplot2::element_geom(linewidth = linewidth / 2)
    } else {
      theme_args[[geom_name]] <- ggplot2::element_geom(linewidth = linewidth)
    }
  }

  # Use inject and splice to pass the arguments
  rlang::inject(ggplot2::update_theme(!!!theme_args))
}
