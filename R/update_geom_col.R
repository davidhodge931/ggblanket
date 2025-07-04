#' Update default colours for geoms
#'
#' @description
#' Modifies the default colour and fill for all geoms or specific geoms in ggplot2 plots.
#' This function updates the active theme to apply consistent colour styling across
#' plot elements.
#'
#' @param col Convenience parameter that sets both `colour` and `fill` to the same value.
#'   Defaults to "blue".
#' @param colour The default colour to use for most geoms. Can be any valid R colour
#'   specification (name, hex code, or rgb()).
#' @param colour2 The alternative colour to use for geoms specified in `colour2_geoms`.
#'   If NULL (default), uses the value of `colour`.
#' @param colour2_geoms Character vector of geom names that should use `colour2`
#'   instead of the default `colour`. Defaults to "bar".
#' @param fill The default fill colour to use for most geoms. Can be any valid R colour
#'   specification.
#' @param fill2 The alternative fill colour to use for geoms specified in `fill2_geoms`.
#'   If NULL (default), uses the value of `fill`.
#' @param fill2_geoms Character vector of geom names that should use `fill2`
#'   instead of the default `fill`. Defaults to "bar".
#'
#' @details
#' This function is useful for:
#' - Applying a consistent colour scheme across all plot elements
#' - Highlighting specific geom types with different colours
#' - Creating themed visualizations with custom colour palettes
#'
#' The `col` parameter is provided as a convenience to set both outline and fill
#' colours simultaneously, which is often desired for a cohesive appearance.
#'
#' @return
#' Invisibly returns the previous theme settings.
#'
#' @examples
#' # Set all geoms to use blue
#' update_geom_col(col = "blue")
#'
#' # Different colors for bars vs other geoms
#' update_geom_col(
#'   colour = "black",
#'   colour2 = "red",
#'   colour2_geoms = c("bar", "col"),
#'   fill = "gray80",
#'   fill2 = "pink",
#'   fill2_geoms = c("bar", "col")
#' )
#'
#' # Highlight violin plots with different colors
#' update_geom_col(
#'   colour = "darkgreen",
#'   colour2 = "orange",
#'   colour2_geoms = "violin",
#'   fill = "lightgreen",
#'   fill2 = "yellow",
#'   fill2_geoms = "violin"
#' )
#'
#' @seealso
#' \code{\link[ggplot2]{theme}}, \code{\link[ggplot2]{update_theme}}
#'
#' @export
update_geom_col <- function(
    col = blue,
    colour = col,
    colour2 = NULL,
    colour2_geoms = c("area", "bar", "col", "density", "map", "polygon",
                      "rect", "ribbon", "tile", "violin", "bin2d", "hex",
                      "raster", "contour_filled", "density_2d_filled",
                      "boxplot", "crossbar"),
    fill = col,
    fill2 = NULL,
    fill2_geoms = c("area", "bar", "col", "density", "map", "polygon",
                    "rect", "ribbon", "tile", "violin", "bin2d", "hex",
                    "raster", "contour_filled", "density_2d_filled",
                    "boxplot", "crossbar")) {

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

  # Apply colour and fill to all geoms
  for (geom in all_geoms) {
    geom_name <- paste0("geom.", gsub("_", "", geom))

    # Determine which colour and fill to use
    geom_colour <- if (geom %in% colour2_geoms && !is.null(colour2)) colour2 else colour
    geom_fill <- if (geom %in% fill2_geoms && !is.null(fill2)) fill2 else fill

    theme_args[[geom_name]] <- ggplot2::element_geom(
      colour = geom_colour,
      fill = geom_fill
    )
  }

  # Use inject and splice to pass the arguments
  rlang::inject(ggplot2::update_theme(!!!theme_args))
}
