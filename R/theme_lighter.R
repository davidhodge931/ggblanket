#' Lighter theme
#'
#' @description A lighter complete theme.
#'
#' @param ... Provided to require argument naming, support trailing commas etc.
#' @param base_size The base size of the text theme element. Defaults to 11.
#' @param base_family The base family of the text theme element. Defaults to "".
#' @param base_colour The base colour of the text theme element.
#' @param legend_position The position of the legend. Either "right", "top" or "bottom".
#' @param legend_axis_line_colour The colour of the legend.axis.line theme element.
#' @param legend_axis_line_linewidth The linewidth of the legend.axis.line theme element.
#' @param legend_background_fill The fill (and colour) of the legend.background theme element.
#' @param legend_key_fill The fill (and colour) of the legend.key theme element.
#' @param legend_ticks_colour The colour of the legend.ticks theme element.
#' @param legend_ticks_linewidth The linewidth of the legend.ticks theme element.
#' @param legend_ticks_length The legend.ticks.length theme element.
#' @param axis_line_colour The colour of the axis.line theme element.
#' @param axis_line_linewidth The linewidth of the axis.line theme element.
#' @param axis_ticks_colour The colour of the axis.ticks theme element.
#' @param axis_ticks_linewidth The linewidth of the axis.ticks theme element.
#' @param axis_ticks_length The length of the axis.ticks.length theme element.
#' @param panel_heights The height of the panels.
#' @param panel_widths The width of the panels.
#' @param panel_grid_colour The colour of the panel.grid theme element.
#' @param panel_grid_linewidth The linewidth of the panel.grid theme element.
#' @param panel_background_fill The fill (and colour) of the panel.background theme element.
#' @param plot_background_fill The fill (and colour) of the plot.background theme element.
#'
#' @return A ggplot theme.
#' @export
#'
#' @examples
#' library(palmerpenguins)
#' library(ggplot2)
#'
#' set_blanket()
#'
#' penguins |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = species,
#'     theme = theme_lighter()
#'   )
#'
#' penguins |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = species,
#'     theme = theme_lighter(legend_position = "top")
#'   )
#'
#' penguins |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = species,
#'     theme = theme_lighter(legend_position = "bottom")
#'   )
#'
theme_lighter <- function(
    ...,
    base_size = 11,
    base_family = "",
    base_colour = "#121B24FF",
    legend_position = "right",
    legend_axis_line_colour = plot_background_fill,
    legend_axis_line_linewidth = axis_line_linewidth,
    legend_background_fill = plot_background_fill,
    legend_key_fill = plot_background_fill,
    legend_ticks_colour = legend_axis_line_colour,
    legend_ticks_linewidth = legend_axis_line_linewidth,
    legend_ticks_length = ggplot2::rel(c(0.175, 0)),
    axis_line_colour = "#121B24FF",
    axis_line_linewidth = 0.25,
    axis_ticks_colour = axis_line_colour,
    axis_ticks_linewidth = axis_line_linewidth,
    axis_ticks_length = grid::unit(11 / 3, "pt"),
    panel_heights = NULL,
    panel_widths = NULL,
    panel_grid_colour = "#F6F8FAFF",
    panel_grid_linewidth = 1.33,
    panel_background_fill = "#FFFFFFFF",
    plot_background_fill = "#FFFFFFFF"
) {
  theme_internal(
    base_size = base_size,
    base_family = base_family,
    base_colour = base_colour,
    plot_caption_colour = colorspace::lighten(base_colour, 0.1),
    axis_line_colour = axis_line_colour,
    axis_line_linewidth = axis_line_linewidth,
    axis_ticks_colour = axis_ticks_colour,
    axis_ticks_linewidth = axis_ticks_linewidth,
    axis_ticks_length = axis_ticks_length,
    legend_position = legend_position,
    legend_axis_line_colour = legend_axis_line_colour,
    legend_axis_line_linewidth = legend_axis_line_linewidth,
    legend_background_fill = legend_background_fill,
    legend_key_fill = legend_key_fill,
    legend_ticks_colour = legend_ticks_colour,
    panel_grid_colour = panel_grid_colour,
    panel_grid_linewidth = panel_grid_linewidth,
    panel_background_fill = panel_background_fill,
    plot_background_fill = plot_background_fill
  )
}

