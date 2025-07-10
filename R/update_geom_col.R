#' Update the default colour/fill
#'
#' @description
#' Updates the active theme to apply consistent colour/fill styling.
#'
#' @param colour Default colour for most geoms.
#' @param colour_polygon Colour for border geoms.
#' @param colour_box Colour for the boxplot geom.
#' @param colour_font Colour for text/label geoms. If NULL, derived from axis text colour.
#' @param colour_reference_line Colour for reference line geoms. If NULL, derived from axis line colour.
#' @param fill Default fill for most geoms.
#' @param fill_polygon Fill for border geoms.
#' @param fill_box Fill for the boxplot geom.
#' @param fill_font Fill for text/label geoms. If NULL, derived from panel background.
#' @param ... Additional arguments (not used).
#'
#' @return An updated ggplot2 theme.
#'
#' @noRd
update_geom_col <- function(
    colour = col,
    colour_polygon = col_squared(colour),
    colour_box = colour,
    colour_font = NULL,
    colour_reference_line = NULL,
    fill = col,
    fill_polygon = fill,
    fill_box = fill,
    fill_font = NULL,
    ...
) {

  # Get current theme for NULL defaults
  current_theme <- ggplot2::get_theme()

  # Handle font colour/fill defaults
  colour_font <- colour_font %||%
    current_theme$axis.text.x$colour %||%
    current_theme$axis.text.y$colour %||%
    current_theme$axis.text$colour %||%
    current_theme$text$colour %||%
    "black"

  fill_font <- fill_font %||%
    current_theme$panel.background$fill %||%
    "white"

  # Handle reference line colour defaults
  colour_reference_line <- colour_reference_line %||%
    current_theme$axis.line.x.bottom$colour %||%
    current_theme$axis.line.x.top$colour %||%
    current_theme$axis.line.y.left$colour %||%
    current_theme$axis.line.y.right$colour %||%
    current_theme$axis.line.x$colour %||%
    current_theme$axis.line.y$colour %||%
    current_theme$axis.line$colour %||%
    "black"

  # Define geom categories
  border_geoms <- c("area", "bar", "col", "crossbar", "density",
                     "map", "polygon", "rect", "ribbon", "smooth", "sf", "tile",
                     "violin", "raster", "contour_filled", "density2d_filled",
                     "bin2d", "hex")

  box_geoms <- c("boxplot")

  font_geoms <- c("text", "label")

  reference_line_geoms <- c("abline", "hline", "vline")

  # Get all geom names
  all_geoms <- c("contour", "count", "curve", "dotplot", "density2d", "errorbar",
                 "freqpoly", "function", "jitter", "line", "linerange", "path",
                 "point", "pointrange", "qq", "quantile", "rug", "segment", "smooth",
                 "spoke", "step", "area", "bar", "col", "density", "map", "polygon",
                 "rect", "ribbon", "tile", "violin", "boxplot", "crossbar", "bin2d",
                 "hex", "raster", "contour_filled", "density2d_filled", "histogram",
                 "text", "label", "abline", "hline", "vline")

  # Build named list of theme elements
  theme_args <- list()

  # Apply colour and fill to all geoms
  for (geom in all_geoms) {
    geom_name <- paste0("geom.", gsub("_", "", geom))

    # Determine which colour and fill to use
    if (geom %in% font_geoms) {
      if (geom == "text") {
        theme_args[[geom_name]] <- ggplot2::element_geom(colour = colour_font)
      } else { # label
        theme_args[[geom_name]] <- ggplot2::element_geom(
          colour = colour_font,
          fill = fill_font
        )
      }
    } else if (geom %in% reference_line_geoms) {
      theme_args[[geom_name]] <- ggplot2::element_geom(colour = colour_reference_line)
    } else if (geom %in% border_geoms) {
      theme_args[[geom_name]] <- ggplot2::element_geom(
        colour = colour_polygon,
        fill = fill_polygon
      )
    } else if (geom %in% box_geoms) {
      theme_args[[geom_name]] <- ggplot2::element_geom(
        colour = colour_box,
        fill = fill_box
      )
    } else {
      theme_args[[geom_name]] <- ggplot2::element_geom(
        colour = colour,
        fill = fill
      )
    }
  }

  # Use inject and splice to pass the arguments
  rlang::inject(ggplot2::update_theme(!!!theme_args))
}
