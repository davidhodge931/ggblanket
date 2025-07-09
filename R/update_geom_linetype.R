#' Update the default linetype
#'
#' @description
#' Updates the active theme to apply consistent linetype styling.
#'
#' @param linetype Default linetype for most geoms.
#' @param linetype_polygon Linetype for polygon-type geoms.
#' @param linetype_box Linetype for box-type geoms.
#' @param linetype_reference_line Linetype for reference line geoms. If NULL, derived from axis line linetype.
#' @param ... Additional arguments (not used).
#'
#' @return An updated ggplot2 theme.
#'
#' @noRd
update_geom_linetype <- function(
    linetype = 1,
    linetype_polygon = linetype,
    linetype_box = linetype,
    linetype_reference_line = NULL,
    ...
) {

  # Get current theme for NULL defaults
  current_theme <- ggplot2::get_theme()

  # Handle reference line linetype defaults
  linetype_reference_line <- linetype_reference_line %||%
    current_theme$axis.line.x.bottom$linetype %||%
    current_theme$axis.line.x.top$linetype %||%
    current_theme$axis.line.y.left$linetype %||%
    current_theme$axis.line.y.right$linetype %||%
    current_theme$axis.line.x$linetype %||%
    current_theme$axis.line.y$linetype %||%
    current_theme$axis.line$linetype %||%
    1

  # Define geom categories
  polygon_geoms <- c("area", "bar", "col", "density", "map", "polygon",
                     "rect", "ribbon", "tile", "violin", "bin2d", "hex",
                     "raster", "contour_filled", "density2d_filled", "histogram")

  box_geoms <- c("boxplot", "crossbar")

  reference_line_geoms <- c("abline", "hline", "vline")

  # Get all geom names
  all_geoms <- c("boxplot", "crossbar", "contour", "count", "curve",
                 "dotplot", "density2d", "errorbar", "freqpoly", "function",
                 "jitter", "line", "linerange", "path", "point", "pointrange",
                 "qq", "qq_line", "quantile", "rug", "segment", "smooth",
                 "spoke", "step", "area", "bar", "col", "density", "map",
                 "polygon", "rect", "ribbon", "tile", "violin", "bin2d",
                 "hex", "raster", "contour_filled", "density2d_filled", "histogram",
                 "abline", "hline", "vline")

  # Build named list of theme elements
  theme_args <- list()

  # Apply linetype to all geoms
  for (geom in all_geoms) {
    geom_name <- paste0("geom.", gsub("_", "", geom))

    if (geom %in% reference_line_geoms) {
      theme_args[[geom_name]] <- ggplot2::element_geom(linetype = linetype_reference_line)
    } else if (geom %in% polygon_geoms) {
      theme_args[[geom_name]] <- ggplot2::element_geom(
        bordertype = linetype_polygon,
        linetype = linetype_polygon
      )
    } else if (geom %in% box_geoms) {
      theme_args[[geom_name]] <- ggplot2::element_geom(
        bordertype = linetype_box,
        linetype = linetype_box
      )
    } else {
      theme_args[[geom_name]] <- ggplot2::element_geom(
        bordertype = linetype,
        linetype = linetype
      )
    }
  }

  # Use inject and splice to pass the arguments
  rlang::inject(ggplot2::update_theme(!!!theme_args))
}
