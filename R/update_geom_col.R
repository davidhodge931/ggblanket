#' Update the default colour/fill
#'
#' @description
#' Updates the active theme to apply consistent colour/fill styling.
#' Excludes "text", "label", "hline", and "vline".
#'
#' @param col Convenience parameter that sets both `colour` and `fill`.
#'
#' @return An updated ggplot2 theme.
#'
#' @seealso
#' \code{\link[ggplot2]{theme}}, \code{\link[ggplot2]{update_theme}}
#'
#' @noRd
update_geom_col <- function(col = blue) {

  ###########
  colour = col
  colour2 = scales::col_darker(col)
  colour2_geoms = c("area", "bar", "col", "density", "map", "polygon",
                    "rect", "ribbon", "smooth", "tile", "violin", "bin2d", "hex",
                    "raster", "contour_filled", "density_2d_filled")
  fill = col
  fill2 = NULL
  fill2_geoms = c("area", "bar", "col", "density", "map", "polygon",
                  "rect", "ribbon", "smooth", "tile", "violin", "bin2d", "hex",
                  "raster", "contour_filled", "density_2d_filled")
  ###########

  # Get all geom names from ggplot2
  all_geoms <- c("contour", "count", "curve",
                 "dotplot", "density2d", "errorbar", "freqpoly", "function",
                 "jitter", "line", "linerange", "path", "point", "pointrange",
                 "qq", "quantile", "rug", "segment", "smooth", "spoke", "step",
                 "area", "bar", "col", "density", "map", "polygon", "rect",
                 "ribbon", "tile", "violin",
                 "boxplot", "crossbar",
                 "bin2d", "hex", "raster", "contour_filled", "density_2d_filled")

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
