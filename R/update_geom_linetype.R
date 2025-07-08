#' Update default linetype for geoms
#'
#' @description
#' Modifies the default linetype for all geoms or specific geoms in ggplot2 plots.
#' This function updates the active theme to apply consistent linetype styling across
#' plot elements.
#'
#' @return An updated ggplot2 theme.
#'
#' @seealso
#' \code{\link[ggplot2]{theme}}, \code{\link[ggplot2]{update_theme}}
#'
#' @export
update_geom_linetype <- function(
    ) {

  ###########
  linetype = 1
  linetype2 = 0
  linetype2_geoms <- c("area", "bar", "col", "density", "map", "polygon",
                      "rect", "ribbon", "tile", "violin", "bin2d", "hex",
                      "raster", "contour_filled", "density_2d_filled")
  ###########

  # Get all geom names from ggplot2
  all_geoms <- c("boxplot", "crossbar", "contour", "count", "curve",
                 "dotplot", "density2d", "errorbar", "freqpoly", "function",
                 "jitter", "line", "linerange", "path", "point", "pointrange",
                 "qq", "qq_line", "quantile", "rug", "segment", "smooth",
                 "spoke", "step", "area", "bar", "col", "density", "map",
                 "polygon", "rect", "ribbon", "tile", "violin", "bin2d",
                 "hex", "raster", "contour_filled", "density_2d_filled")

  # Build named list of theme elements
  theme_args <- list()

  # Apply linetype to all geoms
  for (geom in all_geoms) {
    geom_name <- paste0("geom.", gsub("_", "", geom))

    # Check if this geom should use linetype2
    if (geom %in% linetype2_geoms) {
      theme_args[[geom_name]] <- ggplot2::element_geom(
        bordertype = linetype2,
        linetype = linetype2
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
