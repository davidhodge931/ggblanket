#' Darker theme
#'
#' @description A darker complete theme.
#'
#' @inheritParams theme_lighter
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
  axis_ticks_length = ggplot2::rel(0.66),
  plot_background_fill = "#00040AFF",
  panel_background_fill = "#050D1BFF",
  panel_grid_colour = "#00040AFF",
  panel_grid_linetype = 1,
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
    blend_screen(base_colour),
    blend_multiply(base_colour)
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
    panel_grid_linetype = panel_grid_linetype,
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
