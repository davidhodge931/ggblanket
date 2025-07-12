#' Update the default colour/fill
#'
#' @description
#' Updates the active theme to apply consistent colour/fill styling.
#'
#' @param col Default colour/fill base for most geoms.
#' @param colour Default colour for most geoms.
#' @param colour_border Colour for border geoms. Excludes boxplot.
#' @param colour_font Colour for text/label geoms. If NULL, derived from axis text colour.
#' @param colour_reference_line Colour for reference line geoms. If NULL, derived from axis line colour.
#' @param fill Default fill for most geoms.
#' @param fill_border Fill for border geoms. Excludes boxplot.
#' @param fill_font Fill for text/label geoms. If NULL, derived from panel background.
#' @param ... Additional arguments (not used).
#'
#' @return An updated ggplot2 theme.
#'
#' @noRd
update_geom_col <- function(
    col = "#8991A1",
    colour = NULL,
    colour_border = NULL,
    colour_font = NULL,
    colour_reference_line = NULL,
    fill = NULL,
    fill_border = NULL,
    fill_font = NULL,
    ...
) {
  # Get current theme first
  current_theme <- ggplot2::get_theme()

  # Handle defaults inside the function
  if (is.null(colour)) colour <- col

  # Get the colour_border
  if (is.null(colour_border)) {
    if (is_panel_background_dark(theme = current_theme)) {
      colour_border <- col_screen(colour)
    } else {
      colour_border <- col_multiply(colour)
    }
  }

  if (is.null(fill)) fill <- col
  if (is.null(fill_border)) fill_border <- fill

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
  border_polygon_geoms <- c("area", "bar", "boxplot", "col", "crossbar", "density",
                     "map", "polygon", "rect", "ribbon", "smooth", "sf", "tile",
                     "violin", "raster", "contour_filled", "density2d_filled",
                     "bin2d", "hex")

  border_point_geoms <- c("point", "jitter", "count", "qq")

  font_geoms <- c("text", "label")

  reference_line_geoms <- c("abline", "hline", "vline")

  # Get all geom names
  all_geoms <- c("contour", "count", "curve", "dotplot", "density2d", "errorbar",
                 "freqpoly", "function", "jitter", "line", "linerange", "path",
                 "point", "pointrange", "qq", "quantile", "rug", "segment", "smooth",
                 "spoke", "step", "area", "bar", "boxplot", "col", "density", "map",
                 "polygon", "rect", "ribbon", "tile", "violin", "crossbar", "bin2d",
                 "hex", "raster", "contour_filled", "density2d_filled",
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
    }
    else if (geom %in% reference_line_geoms) {
      theme_args[[geom_name]] <- ggplot2::element_geom(colour = colour_reference_line)
    }
    else if (geom %in% c(border_polygon_geoms, border_point_geoms)) {
      theme_args[[geom_name]] <- ggplot2::element_geom(
        colour = colour_border,
        fill = fill_border
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
