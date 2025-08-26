#' Darker theme
#'
#' @description A darker complete theme. Legend place includes many legend elements including position.
#'
#' @inheritParams theme_lighter
#'
#' @return A ggplot theme.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' set_blanket(theme = theme_darker(legend_place = "top"))
#'
#' palmerpenguins::penguins |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = species,
#'   )
#'
theme_darker <- function(
    ...,
    text_size = 10,
    text_family = "",
    text_colour = "#C8D7DFFF",
    legend_place = "right",
    legend_axis_line_colour = plot_background_fill,
    legend_axis_line_linewidth = axis_line_linewidth,
    legend_background_fill = plot_background_fill,
    legend_key_fill = plot_background_fill,
    legend_ticks_colour = legend_axis_line_colour,
    legend_ticks_linewidth = legend_axis_line_linewidth,
    legend_ticks_length = grid::unit(c(2.75, 0), "pt"),
    axis_line_colour = "#C8D7DFFF",
    axis_line_linewidth = 0.25,
    axis_ticks_colour = axis_line_colour,
    axis_ticks_linewidth = axis_line_linewidth,
    axis_ticks_length = ggplot2::rel(0.66),
    plot_background_fill = "#00040AFF",
    panel_background_fill = "#0E131EFF",
    panel_grid_colour = "#00040AFF",
    panel_grid_linetype = 1,
    panel_grid_linewidth = 0.5,
    panel_grid_minor_linetype = 0,
    panel_grid_minor_linewidth = ggplot2::rel(0.5)
) {
  theme_lighter(
    ...,
    text_size = text_size,
    text_family = text_family,
    text_colour = text_colour,
    legend_place = legend_place,
    legend_axis_line_colour = legend_axis_line_colour,
    legend_axis_line_linewidth = legend_axis_line_linewidth,
    legend_background_fill = legend_background_fill,
    legend_key_fill = legend_key_fill,
    legend_ticks_colour = legend_ticks_colour,
    legend_ticks_linewidth = legend_ticks_linewidth,
    legend_ticks_length = legend_ticks_length,
    axis_line_colour = axis_line_colour,
    axis_line_linewidth = axis_line_linewidth,
    axis_ticks_colour = axis_ticks_colour,
    axis_ticks_linewidth = axis_ticks_linewidth,
    axis_ticks_length = axis_ticks_length,
    plot_background_fill = plot_background_fill,
    panel_background_fill = panel_background_fill,
    panel_grid_colour = panel_grid_colour,
    panel_grid_linetype = panel_grid_linetype,
    panel_grid_linewidth = panel_grid_linewidth,
    panel_grid_minor_linetype = panel_grid_minor_linetype,
    panel_grid_minor_linewidth = panel_grid_minor_linewidth
  )
}
