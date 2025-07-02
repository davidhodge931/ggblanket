#' Update the col, colour and fill for geoms
#'
#' @description
#' Update the col, colour and fill defaults for ggplot2 geoms using a hierarchical system.
#' More specific parameters override more general ones, allowing fine-grained control
#' over different geom categories.
#'
#' @param col A default hex code for the colour and fill of all geoms.
#' @param colour A default hex code for the colour of most geoms. Overrides `col` for colour.
#' @param colour_border A default hex code for the colour of border-style geoms (area, bar, polygon, etc.). Overrides `colour` and `col`.
#' @param colour_box A default hex code for the colour of box-style geoms (boxplot, crossbar). Overrides `colour` and `col`.
#' @param colour_sf A default hex code for the colour of sf geoms. Overrides `colour` and `col`.
#' @param fill A default hex code for the fill of most geoms. Overrides `col` for fill.
#' @param fill_border A default hex code for the fill of border-style geoms. Overrides `fill` and `col`.
#' @param fill_box A default hex code for the fill of box-style geoms. Overrides `fill` and `col`.
#' @param fill_sf A default hex code for the fill of sf geoms. Overrides `fill` and `col`.
#' @param ... Provided to require argument naming and support trailing commas.
#'
#' @details
#' The function uses a hierarchical priority system where more specific parameters
#' override more general ones:
#'
#' For colour properties:
#' 1. `colour_sf`, `colour_box`, `colour_border` (most specific, for geom categories)
#' 2. `colour` (general colour override)
#' 3. `col` (applies to both colour and fill)
#' 4. `blue` ("#357BA2FF") as final fallback
#'
#' For fill properties:
#' 1. `fill_sf`, `fill_box`, `fill_border` (most specific, for geom categories)
#' 2. `fill` (general fill override)
#' 3. `col` (applies to both colour and fill)
#' 4. `blue` ("#357BA2FF") as final fallback
#'
#' Geom categories:
#' - **sf**: geom_sf
#' - **box**: geom_boxplot, geom_crossbar
#' - **border**: geoms with both colour and fill where the colour defines a border
#'   (e.g., geom_area, geom_bar, geom_polygon, geom_ribbon, geom_tile, geom_violin)
#' - **other**: all remaining geoms (e.g., geom_point, geom_line, geom_path)
#'
#' @return An updated ggplot2 theme with modified geom defaults
#' @export
#'
#' @examples
#' # Use default blue colour for all geoms
#' weave_geom_col()
#'
#' # Set a general col for all geoms
#' weave_geom_col(col = "#E74C3C")
#'
#' # Override colour and fill separately
#' weave_geom_col(colour = "#3498DB", fill = "#2ECC71")
#'
#' # Set general col, but override fill
#' weave_geom_col(col = "#9B59B6", fill = "#E67E22")
#'
#' # Fine-grained control: different colors for different geom types
#' weave_geom_col(
#'   col = "#333333",           # fallback for everything
#'   colour_border = "#666666", # borders of areas, bars, etc.
#'   fill_border = "#CCCCCC",   # fills of areas, bars, etc.
#'   colour_box = "#0066CC",    # boxplot outlines
#'   fill_box = "#CCE5FF"       # boxplot fills
#' )
#'
#' # Reset to package defaults
#' weave_geom_col()
weave_geom_col <- function(col = NULL,
                           colour = NULL,
                           colour_border = NULL,
                           colour_box = NULL,
                           colour_sf = NULL,
                           fill = NULL,
                           fill_border = NULL,
                           fill_box = NULL,
                           fill_sf = NULL,
                           ...) {

  # Determine from most specific to most general
  colour_sf <- colour_sf %||% colour %||% col %||% blue
  fill_sf <- fill_sf %||% fill %||% col %||% blue

  colour_box <- colour_box %||% colour %||% col %||% blue
  fill_box <- fill_box %||% fill %||% col %||% blue

  colour_border <- colour_border %||% colour %||% col %||% blue
  fill_border <- fill_border %||% fill %||% col %||% blue

  colour <- colour %||% col %||% blue
  fill <- fill %||% col %||% blue

  ggplot2::update_theme(
    # sf
    geom.sf = ggplot2::element_geom(colour = colour_sf, fill = fill_sf),

    # box
    geom.boxplot = ggplot2::element_geom(colour = colour_box, fill = fill_box),
    geom.crossbar = ggplot2::element_geom(colour = colour_box, fill = fill_box),

    # border
    geom.area = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.bar = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.bin2d = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.col = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.contour_filled = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.density = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.density_2d_filled = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.dotplot = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.hex = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.map = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.polygon = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.raster = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.rect = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.ribbon = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.smooth = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.tile = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.violin = ggplot2::element_geom(colour = colour_border, fill = fill_border),

    # other
    geom.contour = ggplot2::element_geom(colour = colour),
    geom.count = ggplot2::element_geom(colour = colour, fill = colour),
    geom.curve = ggplot2::element_geom(colour = colour),
    geom.density2d = ggplot2::element_geom(colour = colour),
    geom.errorbar = ggplot2::element_geom(colour = colour),
    geom.freqpoly = ggplot2::element_geom(colour = colour),
    geom.function = ggplot2::element_geom(colour = colour),
    geom.jitter = ggplot2::element_geom(colour = colour, fill = colour),
    geom.line = ggplot2::element_geom(colour = colour),
    geom.linerange = ggplot2::element_geom(colour = colour),
    geom.path = ggplot2::element_geom(colour = colour),
    geom.point = ggplot2::element_geom(colour = colour, fill = colour),
    geom.pointrange = ggplot2::element_geom(colour = colour, fill = colour),
    geom.qq = ggplot2::element_geom(colour = colour, fill = colour),
    geom.qq_line = ggplot2::element_geom(colour = colour),
    geom.quantile = ggplot2::element_geom(colour = colour),
    geom.rug = ggplot2::element_geom(colour = colour),
    geom.segment = ggplot2::element_geom(colour = colour),
    geom.spoke = ggplot2::element_geom(colour = colour),
    geom.step = ggplot2::element_geom(colour = colour),
  )
}
