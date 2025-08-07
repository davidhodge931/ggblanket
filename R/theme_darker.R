#' Darker theme
#'
#' @description A darker complete theme.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param base_size The base size of the text theme element. Defaults to 10.
#' @param base_family The base family of the text theme element. Defaults to "".
#' @param base_colour The base colour of the text theme element.
#' @param base_face The base face of the text theme element. Defaults to "plain".
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
#' @param plot_background_fill The fill (and colour) of the plot.background theme element.
#' @param panel_background_fill The fill (and colour) of the panel.background theme element.
#' @param panel_grid_colour The colour of the panel.grid theme element.
#' @param panel_grid_linewidth The linewidth of the panel.grid theme element.
#' @param panel_grid_minor_linetype The linetype of the panel.grid.minor theme element. Defaults to 0.
#' @param panel_grid_minor_linewidth The linewidth of the panel.grid.minor theme element.
#' @param panel_heights The height of the panels.
#' @param panel_widths The width of the panels.
#' @param title_size The size of the plot.title theme element.
#' @param title_family The family of the plot.title theme element.
#' @param title_colour The colour of the plot.title theme element.
#' @param title_face The face of the plot.title theme element.
#' @param subtitle_size The size of the plot.subtitle theme element.
#' @param subtitle_family The family of the plot.subtitle theme element.
#' @param subtitle_colour The colour of the plot.subtitle theme element.
#' @param subtitle_face The face of the plot.subtitle theme element.
#' @param caption_size The size of the plot.caption theme element.
#' @param caption_family The family of the plot.caption theme element.
#' @param caption_colour The colour of the plot.caption theme element.
#' @param caption_face The face of the plot.caption theme element.
#' @param caption_hjust The horizontal adjustment of the plot.caption theme element.
#'
#' @return A ggplot theme.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' set_blanket(theme = theme_darker(legend_position = "top"))
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
  base_size = 10,
  base_family = "",
  base_colour = "#C8D7DFFF",
  base_face = "plain",
  legend_position = "right",
  legend_axis_line_colour = plot_background_fill,
  legend_axis_line_linewidth = axis_line_linewidth,
  legend_background_fill = plot_background_fill,
  legend_key_fill = plot_background_fill,
  legend_ticks_colour = legend_axis_line_colour,
  legend_ticks_linewidth = legend_axis_line_linewidth,
  legend_ticks_length = ggplot2::rel(c(0.175, 0)),
  axis_line_colour = "#C8D7DFFF",
  axis_line_linewidth = 0.25,
  axis_ticks_colour = axis_line_colour,
  axis_ticks_linewidth = axis_line_linewidth,
  axis_ticks_length = grid::unit(11 / 3, "pt"),
  plot_background_fill = "#00040AFF",
  panel_background_fill = "#050D1BFF",
  panel_grid_colour = "#00040AFF",
  panel_grid_linewidth = 1.33,
  panel_grid_minor_linetype = 0,
  panel_grid_minor_linewidth = ggplot2::rel(0.5),
  panel_heights = NULL,
  panel_widths = NULL,
  title_size = ggplot2::rel(1.1),
  title_family = base_family,
  title_colour = base_colour,
  title_face = "bold",
  subtitle_size = ggplot2::rel(1),
  subtitle_family = base_family,
  subtitle_colour = base_colour,
  subtitle_face = "plain",
  caption_size = ggplot2::rel(0.85),
  caption_family = base_family,
  caption_colour = ifelse(
    is_panel_light(),
    col_screen(base_colour),
    col_multiply(base_colour)
  ),
  caption_face = "plain",
  caption_hjust = 0
) {
  theme_lighter(
    ...,
    base_size = base_size,
    base_family = base_family,
    base_colour = base_colour,
    base_face = base_face,
    legend_position = legend_position,
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
    panel_grid_linewidth = panel_grid_linewidth,
    panel_grid_minor_linetype = panel_grid_minor_linetype,
    panel_grid_minor_linewidth = panel_grid_minor_linewidth,
    panel_heights = panel_heights,
    panel_widths = panel_widths,
    title_size = title_size,
    title_family = title_family,
    title_colour = title_colour,
    title_face = title_face,
    subtitle_size = subtitle_size,
    subtitle_family = subtitle_family,
    subtitle_colour = subtitle_colour,
    subtitle_face = subtitle_face,
    caption_size = caption_size,
    caption_family = caption_family,
    caption_colour = caption_colour,
    caption_face = caption_face,
    caption_hjust = caption_hjust
  )
}
