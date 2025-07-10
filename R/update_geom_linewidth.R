#' Update the default linewidth
#'
#' @description
#' Updates the active theme to apply consistent linewidth styling.
#'
#' @param linewidth Default linewidth for most geoms.
#' @param linewidth_border Linewidth for border geoms. Excludes boxplot.
#' @param linewidth_reference_line Linewidth for reference line geoms. If NULL, derived from axis line linewidth.
#' @param ... Additional arguments (not used).
#'
#' @return An updated ggplot2 theme.
#'
#' @noRd
update_geom_linewidth <- function(
    linewidth = 0.66,
    linewidth_border = 0.25,
    linewidth_box = linewidth_border,
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

  # Build theme updates
  theme_args <- list()

  # Reference line geoms
  for (geom in reference_line_geoms) {
    geom_name <- paste0("geom.", geom)
    theme_args[[geom_name]] <- ggplot2::element_geom(linewidth = linewidth_reference_line)
  }

  # Border geoms
  for (geom in border_geoms) {
    geom_name <- paste0("geom.", geom)
    if (geom == "tile") {
      theme_args[[geom_name]] <- ggplot2::element_geom(linewidth = linewidth_border / 0.4,
                                                       borderwidth = linewidth_border / 0.4)
    } else if (geom == "smooth") {
      theme_args[[geom_name]] <- ggplot2::element_geom(linewidth = linewidth / 2,
                                                       borderwidth = linewidth / 2)
    } else {
      theme_args[[geom_name]] <- ggplot2::element_geom(linewidth = linewidth_border,
                                                       borderwidth = linewidth_border)
    }
  }

  # Line geoms
  for (geom in line_geoms) {
    geom_name <- paste0("geom.", geom)
    theme_args[[geom_name]] <- ggplot2::element_geom(linewidth = linewidth)
  }

  # Use inject and splice to pass the arguments
  rlang::inject(ggplot2::update_theme(!!!theme_args))
}
