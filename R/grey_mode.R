#' Grey mode theme family
#'
#' @description
#' A grey mode family of functions:
#' * `grey_mode_r()` with legend on right
#' * `grey_mode_t()` with legend on top
#' * `grey_mode_b()` with legend on bottom
#'
#' @param ... Provided to force user argument naming etc.
#' @param base_size The base size of the text theme element. Defaults to 11.
#' @param base_family The base family of the text theme element. Defaults to "".
#' @param base_colour The base colour of the text theme element.
#' @param axis_line_colour The colour of the axis.line theme element.
#' @param axis_line_linewidth The linewidth of the axis.line theme element.
#' @param axis_ticks_colour The colour of the axis.ticks theme element.
#' @param axis_ticks_linewidth The linewidth of the axis.ticks theme element.
#' @param axis_ticks_length_x The length of the axis.ticks.length.x theme element.
#' @param axis_ticks_length_y The length of the axis.ticks.length.y theme element.
#' @param panel_grid_colour The colour of the panel.grid theme element.
#' @param panel_grid_linewidth The linewidth of the panel.grid theme element.
#' @param panel_background_fill The fill (and colour) of the panel.background theme element.
#' @param plot_background_fill The fill (and colour) of the plot.background theme element.
#' @param legend_axis_line_colour The colour of the legend.axis.line theme element.
#' @param legend_axis_line_linewidth The linewidth of the legend.axis.line theme element.
#' @param legend_background_fill The fill (and colour) of the legend.background theme element.
#' @param legend_key_fill The fill (and colour) of the legend.key theme element.
#' @param legend_ticks_colour The colour of the legend.ticks theme element.
#' @param legend_ticks_linewidth The linewidth of the legend.ticks theme element.
#' @param legend_ticks_length The legend.ticks.length theme element.
#' @param orientation The orientation of the plot. Either "x" or "y". Defaults to NULL. Not intended for use with the mode argument of gg_* functions.
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
#'     mode = grey_mode_r()
#'   )
#'
#' penguins |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = species,
#'     mode = grey_mode_t()
#'   )
#'
#' penguins |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = species,
#'     mode = grey_mode_b()
#'   )
#'
grey_mode_r <- function (
    ...,
    base_size = 11,
    base_family = "",
    base_colour = "#121B24FF",
    axis_line_colour = "#121B24FF",
    axis_line_linewidth = 0.33,
    axis_ticks_colour = axis_line_colour,
    axis_ticks_linewidth = axis_line_linewidth,
    axis_ticks_length_x = grid::unit(base_size / 3, "pt"),
    axis_ticks_length_y = grid::unit(base_size / 4, "pt"),
    panel_grid_colour = "#F6F8FAFF",
    panel_grid_linewidth = 1.33,
    panel_background_fill ="#FCFDFEFF",
    plot_background_fill = "#F6F8FAFF",
    legend_axis_line_colour = plot_background_fill,
    legend_axis_line_linewidth = 0.33,
    legend_background_fill = plot_background_fill,
    legend_key_fill = plot_background_fill,
    legend_ticks_colour = legend_axis_line_colour,
    legend_ticks_linewidth = legend_axis_line_linewidth,
    legend_ticks_length = ggplot2::rel(c(0.175, 0)),
    orientation = NULL) {

  flex_mode_r(
    base_size = base_size,
    base_family = base_family,
    base_colour = base_colour,
    base_face = "plain",
    plot_title_size = ggplot2::rel(1.1),
    plot_title_family = base_family,
    plot_title_face = "bold",
    plot_title_colour = base_colour,
    plot_subtitle_size = ggplot2::rel(1),
    plot_subtitle_family = base_family,
    plot_subtitle_face = "plain",
    plot_subtitle_colour = base_colour,
    plot_caption_size = ggplot2::rel(0.85),
    plot_caption_family = base_family,
    plot_caption_face = "plain",
    plot_caption_colour = colorspace::lighten(base_colour, 0.1),
    plot_caption_hjust = 0,

    axis_line_colour = axis_line_colour,
    axis_line_linewidth = axis_line_linewidth,
    axis_ticks_colour = axis_ticks_colour,
    axis_ticks_linewidth = axis_ticks_linewidth,
    axis_ticks_length_x = axis_ticks_length_x,
    axis_ticks_length_y = axis_ticks_length_y,
    panel_grid_colour = panel_grid_colour,
    panel_grid_linewidth = panel_grid_linewidth,
    panel_background_fill = panel_background_fill,
    plot_background_fill = plot_background_fill,
    legend_axis_line_colour = legend_axis_line_colour,
    legend_axis_line_linewidth = legend_axis_line_linewidth,
    legend_background_fill = legend_background_fill,
    legend_key_fill = legend_key_fill,
    legend_ticks_colour = legend_ticks_colour,
    legend_ticks_linewidth = legend_ticks_linewidth,
    legend_ticks_length = legend_ticks_length,
    orientation = orientation)
}

#' @rdname grey_mode_r
#' @export
grey_mode_t <- function (
    ...,
    base_size = 11,
    base_family = "",
    base_colour = "#121B24FF",
    axis_line_colour = "#121B24FF",
    axis_line_linewidth = 0.33,
    axis_ticks_colour = axis_line_colour,
    axis_ticks_linewidth = axis_line_linewidth,
    axis_ticks_length_x = grid::unit(base_size / 3, "pt"),
    axis_ticks_length_y = grid::unit(base_size / 4, "pt"),
    panel_grid_colour = "#F6F8FAFF",
    panel_grid_linewidth = 1.33,
    panel_background_fill ="#FCFDFEFF",
    plot_background_fill = "#F6F8FAFF",
    legend_axis_line_colour = plot_background_fill,
    legend_axis_line_linewidth = 0.33,
    legend_background_fill = plot_background_fill,
    legend_key_fill = plot_background_fill,
    legend_ticks_colour = legend_axis_line_colour,
    legend_ticks_linewidth = legend_axis_line_linewidth,
    legend_ticks_length = ggplot2::rel(c(0.175, 0)),
    orientation = NULL) {

  flex_mode_t(
    base_size = base_size,
    base_family = base_family,
    base_colour = base_colour,
    base_face = "plain",
    plot_title_size = ggplot2::rel(1.1),
    plot_title_family = base_family,
    plot_title_face = "bold",
    plot_title_colour = base_colour,
    plot_subtitle_size = ggplot2::rel(1),
    plot_subtitle_family = base_family,
    plot_subtitle_face = "plain",
    plot_subtitle_colour = base_colour,
    plot_caption_size = ggplot2::rel(0.85),
    plot_caption_family = base_family,
    plot_caption_face = "plain",
    plot_caption_colour = colorspace::lighten(base_colour, 0.1),
    plot_caption_hjust = 0,

    axis_line_colour = axis_line_colour,
    axis_line_linewidth = axis_line_linewidth,
    axis_ticks_colour = axis_ticks_colour,
    axis_ticks_linewidth = axis_ticks_linewidth,
    axis_ticks_length_x = axis_ticks_length_x,
    axis_ticks_length_y = axis_ticks_length_y,
    panel_grid_colour = panel_grid_colour,
    panel_grid_linewidth = panel_grid_linewidth,
    panel_background_fill = panel_background_fill,
    plot_background_fill = plot_background_fill,
    legend_axis_line_colour = legend_axis_line_colour,
    legend_axis_line_linewidth = legend_axis_line_linewidth,
    legend_background_fill = legend_background_fill,
    legend_key_fill = legend_key_fill,
    legend_ticks_colour = legend_ticks_colour,
    legend_ticks_linewidth = legend_ticks_linewidth,
    legend_ticks_length = legend_ticks_length,
    orientation = orientation)
}

#' @rdname grey_mode_r
#' @export
grey_mode_b <- function (
    ...,
    base_size = 11,
    base_family = "",
    base_colour = "#121B24FF",
    axis_line_colour = "#121B24FF",
    axis_line_linewidth = 0.33,
    axis_ticks_colour = axis_line_colour,
    axis_ticks_linewidth = axis_line_linewidth,
    axis_ticks_length_x = grid::unit(base_size / 3, "pt"),
    axis_ticks_length_y = grid::unit(base_size / 4, "pt"),
    panel_grid_colour = "#F6F8FAFF",
    panel_grid_linewidth = 1.33,
    panel_background_fill ="#FCFDFEFF",
    plot_background_fill = "#F6F8FAFF",
    legend_axis_line_colour = plot_background_fill,
    legend_axis_line_linewidth = 0.33,
    legend_background_fill = plot_background_fill,
    legend_key_fill = plot_background_fill,
    legend_ticks_colour = legend_axis_line_colour,
    legend_ticks_linewidth = legend_axis_line_linewidth,
    legend_ticks_length = ggplot2::rel(c(0.175, 0)),
    orientation = NULL) {

  flex_mode_b(
    base_size = base_size,
    base_family = base_family,
    base_colour = base_colour,
    base_face = "plain",
    plot_title_size = ggplot2::rel(1.1),
    plot_title_family = base_family,
    plot_title_face = "bold",
    plot_title_colour = base_colour,
    plot_subtitle_size = ggplot2::rel(1),
    plot_subtitle_family = base_family,
    plot_subtitle_face = "plain",
    plot_subtitle_colour = base_colour,
    plot_caption_size = ggplot2::rel(0.85),
    plot_caption_family = base_family,
    plot_caption_face = "plain",
    plot_caption_colour = colorspace::lighten(base_colour, 0.1),
    plot_caption_hjust = 0,

    axis_line_colour = axis_line_colour,
    axis_line_linewidth = axis_line_linewidth,
    axis_ticks_colour = axis_ticks_colour,
    axis_ticks_linewidth = axis_ticks_linewidth,
    axis_ticks_length_x = axis_ticks_length_x,
    axis_ticks_length_y = axis_ticks_length_y,
    panel_grid_colour = panel_grid_colour,
    panel_grid_linewidth = panel_grid_linewidth,
    panel_background_fill = panel_background_fill,
    plot_background_fill = plot_background_fill,
    legend_axis_line_colour = legend_axis_line_colour,
    legend_axis_line_linewidth = legend_axis_line_linewidth,
    legend_background_fill = legend_background_fill,
    legend_key_fill = legend_key_fill,
    legend_ticks_colour = legend_ticks_colour,
    legend_ticks_linewidth = legend_ticks_linewidth,
    legend_ticks_length = legend_ticks_length,
    orientation = orientation)
}
