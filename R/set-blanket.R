#' Set the style
#'
#' @description
#' Set a consistent style. Most users will only need
#' `theme`, `col`, `col_palette_discrete`, `col_palette_continuous` and `aspect_*` arguments.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param theme A ggplot2 theme (e.g. [theme_lighter()] or [theme_darker()]).
#' @param col A default hex code for the colour and fill of most geoms.
#' @param col_palette_discrete For a discrete colour/fill scale, a character vector or a `scales::pal_*` function.
#' @param col_palette_continuous For a continuous colour/fill scale, a character vector or a `scales::pal_*` function.
#' @param col_palette_ordinal For a ordinal colour/fill scale, a `scales::pal_*` function. If NULL, determined from `col_palette_continuous`.
#' @param col_na A NA colour/fill value.
#' @param colour_border_transform A function with input of the `col` or `col_palette`.
#' @param fill_border_transform A function with input of the `col` or `col_palette`.
#' @param shape A default shape for point geoms. Must be an integer between 0 and 25.
#' @param shape_palette_d For shape scales, a numeric vector of shape codes.
#' @param shape_na A NA shape value.
#' @param linetype A default linetype for most geoms.
#' @param linetype_palette_d For linetype scales, a character vector or a `scales::pal_*` function.
#' @param linewidth A default linewidth for geoms. A number.
#' @param linewidth_border A default linewidth for geoms that have a border. A number.
#' @param size A default size for point geoms.
#' @param stroke A default stroke for point geoms.
#' @param axis_line_aspect `"transparent"`, `"blank"` or `"keep"` of how to treat the y axis line for an `"x"` `aspect`, and vice versa.
#' @param axis_ticks_aspect `"transparent"`, `"blank"` or `"keep"` of how to treat the y axis ticks for an `"x"` `aspect`, and vice versa.
#' @param panel_grid_aspect `"transparent"`, `"blank"` or `"keep"` of how to treat the x panel grid for an `"x"` `aspect`, and vice versa.
#' @param panel_heights The height of the panels. E.g. `grid::unit(5, "cm")`.
#' @param panel_widths The width of the panels. E.g. `grid::unit(7.5, "cm")`.
#'
#' @return Invisibly returns NULL. Sets global styling options as a side effect.
#'
#' @seealso
#' [theme_lighter()], [theme_darker()] for theme options
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(ggblanket)
#'
#' # Use col_palette_discrete for both colour and fill
#' set_blanket(
#'   col_palette_discrete = c("#E69F00", "#56B4E9", "#009E73")
#' )
#'
set_blanket <- function(
    ...,
    theme = theme_greyer(),
    col = ifelse(is_panel_dark(), "#357BA2FF", "#4797C3FF"),
    col_palette_discrete = scales::pal_hue(),
    col_palette_continuous = direction_contrast(scales::pal_viridis(option = "mako")),
    col_palette_ordinal = NULL,
    col_na = "#A6A6A6FF",
    colour_border_transform = \(x) {
      if (is_panel_dark()) {
        blend_screen(x)
      } else {
        blend_multiply(x)
      }
    },
    fill_border_transform = NULL,
    shape = 21,
    shape_palette_d = c(21, 24, 22, 23, 25),
    shape_na = 4,
    linetype = 1,
    linetype_palette_d = 1:6,
    linewidth = 0.66,
    linewidth_border = 0.25,
    size = 1.5,
    stroke = 0.5,
    axis_line_aspect = "transparent",
    axis_ticks_aspect = "transparent",
    panel_grid_aspect = "transparent",
    panel_heights = NULL,
    panel_widths = NULL
) {
  # Set the theme first
  ggplot2::set_theme(theme)

  # Then everything else
  update_geom_col(col = col)
  update_geom_shape(shape = shape)
  update_geom_linetype(linetype = linetype)
  update_geom_linewidth(linewidth = linewidth)
  update_geom_size(size = size)
  update_geom_stroke(stroke = stroke)

  update_geom_palettes(
    col_palette_discrete = col_palette_discrete,
    col_palette_continuous = col_palette_continuous,
    col_palette_ordinal = col_palette_ordinal,
    col_na = col_na,
    shape_palette_d = shape_palette_d,
    shape_na = shape_na,
    linetype_palette_d = linetype_palette_d
  )

  update_geom_border(
    colour_border_transform = colour_border_transform,
    fill_border_transform = fill_border_transform,
    linewidth_border = linewidth_border
  )

  update_geom_font()

  update_geom_reference_line()

  update_aspect(
    axis_line_aspect = axis_line_aspect,
    axis_ticks_aspect = axis_ticks_aspect,
    panel_grid_aspect = panel_grid_aspect
  )

  update_panel_dimensions(
    panel_heights = panel_heights,
    panel_widths = panel_widths
  )
}
