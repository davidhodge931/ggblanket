#' Update the default linetype
#'
#' @description
#' Updates the active theme to apply consistent linetype styling.
#'
#' @param linetype Default linetype for most geoms.
#' @param linetype_border Linetype for border geoms. Excludes boxplot.
#' @param linetype_reference_line Linetype for reference line geoms. If NULL, derived from axis line linetype.
#' @param ... Additional arguments (not used).
#'
#' @return An updated ggplot2 theme.
#'
#' @noRd
update_geom_linetype <- function(
    linetype = 1,
    linetype_border = linetype,
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

  # Border geoms
  border_geoms <- c("area", "bar", "col", "crossbar", "density",
                    "map", "polygon", "rect", "ribbon", "smooth", "sf", "tile",
                    "violin", "raster", "contour_filled", "density2d_filled",
                    "bin2d", "hex")

  # Line geoms
  line_geoms <- c("contour", "curve", "errorbar", "freqpoly", "function",
                  "line", "linerange", "path", "qq_line", "quantile", "rug",
                  "segment", "spoke", "step", "density2d")

  # reference_line_geoms
  reference_line_geoms <- c("abline", "hline", "vline")

  # Build named list of theme elements
  theme_args <- list()

  for (geom in reference_line_geoms) {
    geom_name <- paste0("geom.", geom)
    theme_args[[geom_name]] <- ggplot2::element_geom(linetype = linetype_reference_line)
  }

  # Border geoms
  for (geom in border_geoms) {
    geom_name <- paste0("geom.", geom)
      theme_args[[geom_name]] <- ggplot2::element_geom(linetype = linetype_border,
                                                       bordertype = linetype_border)
  }

  # Line geoms
  for (geom in line_geoms) {
    geom_name <- paste0("geom.", geom)
    theme_args[[geom_name]] <- ggplot2::element_geom(linetype = linetype)
  }


  # Use inject and splice to pass the arguments
  rlang::inject(ggplot2::update_theme(!!!theme_args))
}
