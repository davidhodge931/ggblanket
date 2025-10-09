#' Set ggblanket defaults
#'
#' Configure global ggplot2 theme settings for consistent styling across plots.
#' Sets base theme, default geom aesthetics, color palettes, and border styling.
#' Should typically be called once at the start of a script or session.
#'
#' @param theme A ggplot2 theme object to use as the base theme. If `NULL`,
#'   the current theme is retained.
#'
#' @section Geom Defaults:
#' @param fill Default fill color for geoms. Defaults to ocean (dark panels) or
#'   blue (light panels).
#' @param shape Default shape for point geoms. Default is 21 (filled circle).
#' @param linewidth Default line width for line geoms. Default is 0.66.
#' @param size Default size for point geoms. Default is 1.5.
#' @param stroke Default stroke width for point borders. Default is 0.5.
#'
#' @section Palettes:
#' @param fill_palette Color palette(s) for fill and colour scales. Can be:
#'   * `NULL` (default): Uses `pal_hue()` for discrete and viridis "mako" for continuous
#'   * Length 1: Discrete palette (continuous defaults to viridis "mako")
#'   * Length 2: `list(discrete_palette, continuous_palette)`
#' @param shape_palette Shape palette for discrete shape scales.
#'   Default is `c(21, 24, 22, 23, 25)`.
#' @param linetype_palette Linetype palette for discrete linetype scales.
#'   Default is `1:6`.
#'
#' @section Borders:
#' @param border_geoms Character vector of geom names to apply border styling to.
#'   If `NULL` (default), applies to: area, bin2d, bar, boxplot, col, contour_filled,
#'   crossbar, density, density2d_filled, hex, map, polygon, raster, rect, ribbon,
#'   sf, tile, violin.
#' @param border_linewidth Line width for borders on specified geoms. Default is 0.25.
#' @param border_pop Function to modify fill colors for borders. Default lightens
#'   colors on dark panels and darkens on light panels. Applied to both single
#'   colors and palette functions.
#'
#' @return Invisibly returns `NULL`. Called for side effects.
#'
#' @examples
#' \dontrun{
#' # Set ggblanket defaults at start of script
#' set_blanket()
#'
#' # With custom base theme
#' set_blanket(theme = theme_minimal())
#'
#' # Custom fill color and palettes
#' set_blanket(
#'   fill = "steelblue",
#'   fill_palette = list(
#'     scales::brewer_pal(palette = "Set2"),
#'     scales::viridis_pal()
#'   )
#' )
#'
#' # Apply borders to specific geoms only
#' set_blanket(
#'   border_geoms = c("bar", "col", "tile"),
#'   border_linewidth = 0.5
#' )
#' }
#'
#' @export
set_blanket <- function(
    #base
  theme = NULL,
  #geom
  fill = ifelse(is_panel_dark(), ocean, blue),
  shape = 21,
  linewidth = 0.66,
  size = 1.5,
  stroke = 0.5,
  #palette
  fill_palette = NULL,
  shape_palette = scales::pal_manual(c(21, 24, 22, 23, 25)),
  linetype_palette = scales::pal_manual(1:6),
  #border
  border_geoms = NULL,
  border_linewidth = 0.25,
  border_pop = \(x) ifelse(is_panel_dark(), scales::col_lighter(x), scales::col_darker(x))
) {

  #base
  base_theme <- if (!rlang::is_null(theme)) theme else ggplot2::theme_get()

  #geom - get all geom names
  all_geoms <- ls(pattern = "^Geom", envir = asNamespace("ggplot2"))
  all_geoms <- tolower(sub("^Geom", "", all_geoms))
  all_geoms <- all_geoms[all_geoms != ""]  # Remove empty string

  #border
  if (rlang::is_null(border_geoms)) {
    # Geoms with filled areas that benefit from thinner borders
    border_geoms <- c(
      "area", "bar", "bin2d", "boxplot", "col", "contourfilled",
      "crossbar", "density", "density2d", "density2dfilled", "dotplot",
      "hex", "map", "polygon", "raster", "rect", "ribbon", "sf",
      "tile", "violin"
    )
  }

  # Apply geom defaults to all geoms, with conditional linewidth
  geom_theme <- rlang::set_names(
    lapply(all_geoms, \(geom) {
      # Use border_linewidth for border geoms, otherwise use default linewidth
      linewidth <- if (geom %in% border_geoms) border_linewidth else linewidth

      ggplot2::element_geom(
        fill = fill,
        colour = border_pop(fill),
        shape = shape,
        linewidth = linewidth,
        borderwidth = linewidth,
        size = size,
        stroke = stroke,
      )
    }),
    paste0("geom.", all_geoms)
  )

  #palette
  if (rlang::is_null(fill_palette)) {
    fill_palette_d <- scales::pal_hue()
    fill_palette_c <- direction_contrast(scales::pal_viridis(option = "mako", begin = 0.1, end = 0.9))
  }
  else if (length(fill_palette) == 1) {
    fill_palette_d <- unlist(fill_palette)
    fill_palette_c <- direction_contrast(scales::pal_viridis(option = "mako", begin = 0.1, end = 0.9))
  }
  else if (length(fill_palette) == 2) {
    fill_palette_d <- fill_palette[[1]]
    fill_palette_c <- fill_palette[[2]]
  }

  # Apply border_pop based on whether palette is a function or vector
  if (is.function(fill_palette_d)) {
    colour_palette_d <- function(x) border_pop(fill_palette_d(x))
  } else {
    colour_palette_d <- border_pop(fill_palette_d)
  }

  if (is.function(fill_palette_c)) {
    colour_palette_c <- function(x) border_pop(fill_palette_c(x))
  } else {
    colour_palette_c <- border_pop(fill_palette_c)
  }

  #set theme
  ggplot2::set_theme(
    base_theme +
      ggplot2::theme(
        palette.colour.discrete = colour_palette_d,
        palette.fill.discrete = fill_palette_d,
        palette.colour.continuous = colour_palette_c,
        palette.fill.continuous = fill_palette_c,
        palette.shape.discrete = shape_palette,
        palette.linetype.discrete = linetype_palette,
        !!!geom_theme
      )
  )

  invisible(NULL)
}
