#' Update the colour/fill for geoms
#'
#' @description
#' Update the colour/fill defaults for ggplot2 geoms using a hierarchical system.
#' More specific parameters override more general ones, allowing fine-grained control
#' over different geom categories.
#'
#' @param col A default hex code for both colour and fill of all geoms.
#' @param ... Provided to require argument naming and support trailing commas.
#' @param colour A default hex code for the colour of all geoms. Overrides `col` for colour.
#' @param colour_border The outline colour for polygon geoms (excluding boxplot, crossbar,
#'   and sf). Controls the colour of the border line itself. Overrides `colour` and `col`.
#' @param colour_box The outline colour for boxplot and crossbar geoms specifically.
#'   Overrides `colour` and `col`.
#' @param fill A default hex code for the fill of all geoms. Overrides `col` for fill.
#' @param fill_border The interior fill colour for polygon geoms (excluding boxplot,
#'   crossbar, and sf). This sets the fill colour inside the shape, not the border
#'   line colour. Overrides `fill` and `col`.
#' @param fill_box The interior fill colour for boxplot and crossbar geoms specifically.
#'   Overrides `fill` and `col`.
#'
#' @details
#' The function uses a hierarchical priority system where more specific parameters
#' override more general ones:
#'
#' For colour properties:
#' 1. `colour_box`, `colour_border` (most specific, for geom categories)
#' 2. `colour` (general colour override)
#' 3. `col` (applies to both colour and fill)
#' 4. `"#357BA2FF"` (blue) as final fallback
#'
#' For fill properties:
#' 1. `fill_box`, `fill_border` (most specific, for geom categories)
#' 2. `fill` (general fill override)
#' 3. `col` (applies to both colour and fill)
#' 4. `"#357BA2FF"` (blue) as final fallback
#'
#' @section Geom Categories:
#' Geoms are grouped into categories for consistent styling:
#' \describe{
#'   \item{**border**}{Polygon geoms (excluding boxplot, crossbar, and sf) where
#'     colour defines the outline and fill defines the interior: area, bar, bin2d,
#'     col, contour_filled, density, density_2d_filled, dotplot, hex, map, polygon,
#'     raster, rect, ribbon, smooth, tile, violin}
#'   \item{**box**}{Specifically boxplot and crossbar geoms only}
#'   \item{**other**}{All remaining geoms (e.g., point, line, path). For these geoms,
#'     colour and fill are typically the same value.}
#' }
#'
#' @return An updated ggplot2 theme with modified geom defaults
#' @export
#'
#' @examples
#' # Use default blue colour/fill for all geoms
#' update_geom_col()
#'
#' # Set a general colour/fill for all geoms
#' update_geom_col(col = "#E74C3C")
#'
#' # Set different colours for outline vs fill
#' update_geom_col(colour = "#3498DB", fill = "#E8F4F8")
#'
#' # Remove borders from polygons (by setting same colour for border and fill)
#' update_geom_col(
#'   col = "#2ECC71",
#'   colour_border = "#2ECC71",  # Same as fill = invisible border
#'   fill_border = "#2ECC71"
#' )
#'
#' # Fine-grained control: different colors for different geom types
#' update_geom_col(
#'   col = "#333333",           # fallback for everything
#'   colour_border = "#666666", # outline of areas, bars, etc.
#'   fill_border = "#CCCCCC",   # interior of areas, bars, etc.
#'   colour_box = "#0066CC",    # boxplot outlines
#'   fill_box = "#CCE5FF"       # boxplot interiors
#' )
#'
#' # Style for a clean look: darker outlines, lighter fills
#' update_geom_col(
#'   colour = colorspace::darken("#3498DB", 0.3),
#'   fill = colorspace::lighten("#3498DB", 0.4)
#' )
update_geom_col <- function(col = NULL,
                           ...,
                           colour = NULL,
                           colour_border = NULL,
                           colour_box = NULL,
                           fill = NULL,
                           fill_border = NULL,
                           fill_box = NULL
                           ) {

  # Determine from most specific to most general
  colour_box <- colour_box %||% colour %||% col %||% blue
  fill_box <- fill_box %||% fill %||% col %||% blue

  colour_border <- colour_border %||% colour %||% col %||% blue
  fill_border <- fill_border %||% fill %||% col %||% blue

  colour <- colour %||% col %||% blue
  fill <- fill %||% col %||% blue

  ggplot2::update_theme(
    # box
    geom.boxplot = ggplot2::element_geom(colour = colour_box, fill = fill_box),
    geom.crossbar = ggplot2::element_geom(colour = colour_box, fill = fill_box),

    # border
    geom.area = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.bar = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.bin2d = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.col = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.contour.filled = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.density = ggplot2::element_geom(colour = colour_border, fill = fill_border),
    geom.density2d.filled = ggplot2::element_geom(colour = colour_border, fill = fill_border),
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
    geom.dotplot = ggplot2::element_geom(colour = colour, fill = fill),
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
