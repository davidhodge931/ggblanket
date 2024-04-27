#' Dark mode theme with right legend
#'
#' @description Dark mode theme with right legend using `darkness` and `linewidthness`.
#'
#' @param base_size The base size of the text theme element. Defaults to 11.
#' @param base_family The base family of the text theme element. Defaults to "".
#' @param base_colour The base colour of the text theme element.
#' @param axis_line_colour The colour of the axis.line theme element.
#' @param axis_line_linewidth The linewidth of the axis.line theme element.
#' @param axis_ticks_colour The colour of the axis.ticks theme element.
#' @param axis_ticks_linewidth The linewidth of the axis.ticks theme element.
#' @param axis_ticks_length_x_pt The length of the axis.ticks.length.x theme element in points.
#' @param axis_ticks_length_y_pt The length of the axis.ticks.length.y theme element in points.
#' @param legend_ticks_colour The colour of the legend.ticks theme element.
#' @param legend_ticks_linewidth The linewidth of the legend.ticks theme element.
#' @param panel_grid_colour The colour of the panel.grid theme element.
#' @param panel_grid_linewidth The linewidth of the panel.grid theme element.
#' @param panel_background_fill The fill (and colour) of the panel.background theme element.
#' @param plot_background_fill The fill (and colour) of the plot.background theme element.
#' @param legend_background_fill The fill (and colour) of the legend.background theme element.
#' @param legend_key_fill The fill (and colour) of the legend.key theme element.
#' @param orientation The orientation of the plot. Either "x" or "y". Defaults to NULL. Not intended for use with the mode argument of gg_* functions.
#' @param ... Provided to support trailing commas only.
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
#'     mode = dark_mode_r()
#'   )
#'
dark_mode_r <- function (
    base_size = 11,
    base_family = "",
    base_colour = "#c8d7dfff",
    axis_line_colour = "#c8d7dfff",
    axis_line_linewidth = 0.33,
    axis_ticks_colour = axis_line_colour,
    axis_ticks_linewidth = axis_line_linewidth,
    axis_ticks_length_x_pt = base_size / 3,
    axis_ticks_length_y_pt = base_size / 4,
    panel_grid_colour = "#00040aff",
    panel_grid_linewidth = 1.33,
    panel_background_fill = "#050d1bff",
    plot_background_fill = "#00040aff",
    legend_background_fill = plot_background_fill,
    legend_key_fill = plot_background_fill,
    legend_ticks_colour = plot_background_fill,
    legend_ticks_linewidth = 0.33,
    orientation = NULL,
    ...
) {

  foundation_mode_r(
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
    plot_caption_colour = scales::alpha(base_colour, 0.75),
    plot_caption_hjust = 0,

    axis_line_colour = axis_line_colour,
    axis_line_linewidth = axis_line_linewidth,
    axis_ticks_colour = axis_ticks_colour,
    axis_ticks_linewidth = axis_ticks_linewidth,
    axis_ticks_length_x_pt = axis_ticks_length_x_pt,
    axis_ticks_length_y_pt = axis_ticks_length_y_pt,
    legend_ticks_colour = legend_ticks_colour,
    legend_ticks_linewidth = legend_ticks_linewidth,
    panel_grid_colour = panel_grid_colour,
    panel_grid_linewidth = panel_grid_linewidth,
    panel_background_fill = panel_background_fill,
    plot_background_fill = plot_background_fill,
    legend_background_fill = legend_background_fill,
    legend_key_fill = legend_key_fill,
    orientation = orientation,
    ...
  )
}

#' Dark mode theme with top legend
#'
#' @description Dark mode theme with top legend using `darkness` and `linewidthness`.
#'
#' @inheritParams dark_mode_r
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
#'     mode = dark_mode_t()
#'   )
#'
dark_mode_t <- function (
    base_size = 11,
    base_family = "",
    base_colour = "#c8d7dfff",
    axis_line_colour = "#c8d7dfff",
    axis_line_linewidth = 0.33,
    axis_ticks_colour = axis_line_colour,
    axis_ticks_linewidth = axis_line_linewidth,
    axis_ticks_length_x_pt = base_size / 3,
    axis_ticks_length_y_pt = base_size / 4,
    panel_grid_colour = "#00040aff",
    panel_grid_linewidth = 1.33,
    panel_background_fill = "#050d1bff",
    plot_background_fill = "#00040aff",
    legend_background_fill = plot_background_fill,
    legend_key_fill = plot_background_fill,
    legend_ticks_colour = plot_background_fill,
    legend_ticks_linewidth = 0.33,
    orientation = NULL,
    ...
) {

  foundation_mode_t(
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
    plot_caption_colour = scales::alpha(base_colour, 0.75),
    plot_caption_hjust = 0,

    axis_line_colour = axis_line_colour,
    axis_line_linewidth = axis_line_linewidth,
    axis_ticks_colour = axis_ticks_colour,
    axis_ticks_linewidth = axis_ticks_linewidth,
    axis_ticks_length_x_pt = axis_ticks_length_x_pt,
    axis_ticks_length_y_pt = axis_ticks_length_y_pt,
    legend_ticks_colour = legend_ticks_colour,
    legend_ticks_linewidth = legend_ticks_linewidth,
    panel_grid_colour = panel_grid_colour,
    panel_grid_linewidth = panel_grid_linewidth,
    panel_background_fill = panel_background_fill,
    plot_background_fill = plot_background_fill,
    legend_background_fill = legend_background_fill,
    legend_key_fill = legend_key_fill,
    orientation = orientation,
    ...
  )
}

#' Dark mode theme with bottom legend
#'
#' @description Dark mode theme with bottom legend using `darkness` and `linewidthness`.
#'
#' @inheritParams dark_mode_r
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
#'     mode = dark_mode_b()
#'   )
#'
dark_mode_b <- function (
    base_size = 11,
    base_family = "",
    base_colour = "#c8d7dfff",
    axis_line_colour = "#c8d7dfff",
    axis_line_linewidth = 0.33,
    axis_ticks_colour = axis_line_colour,
    axis_ticks_linewidth = axis_line_linewidth,
    axis_ticks_length_x_pt = base_size / 3,
    axis_ticks_length_y_pt = base_size / 4,
    panel_grid_colour = "#00040aff",
    panel_grid_linewidth = 1.33,
    panel_background_fill = "#050d1bff",
    plot_background_fill = "#00040aff",
    legend_background_fill = plot_background_fill,
    legend_key_fill = plot_background_fill,
    legend_ticks_colour = plot_background_fill,
    legend_ticks_linewidth = 0.33,
    orientation = NULL,
    ...
) {

  foundation_mode_b(
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
    plot_caption_colour = scales::alpha(base_colour, 0.75),
    plot_caption_hjust = 0,

    axis_line_colour = axis_line_colour,
    axis_line_linewidth = axis_line_linewidth,
    axis_ticks_colour = axis_ticks_colour,
    axis_ticks_linewidth = axis_ticks_linewidth,
    axis_ticks_length_x_pt = axis_ticks_length_x_pt,
    axis_ticks_length_y_pt = axis_ticks_length_y_pt,
    legend_ticks_colour = legend_ticks_colour,
    legend_ticks_linewidth = legend_ticks_linewidth,
    panel_grid_colour = panel_grid_colour,
    panel_grid_linewidth = panel_grid_linewidth,
    panel_background_fill = panel_background_fill,
    plot_background_fill = plot_background_fill,
    legend_background_fill = legend_background_fill,
    legend_key_fill = legend_key_fill,
    orientation = orientation,
    ...
  )
}

#' Dark mode theme with no legend
#'
#' @description Dark mode theme with no legend using `darkness` and `linewidthness`.
#'
#' @inheritParams dark_mode_r
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
#'   gg_jitter(
#'     x = species,
#'     y = body_mass_g,
#'     col = species,
#'     mode = dark_mode_n()
#'   )
#'
dark_mode_n <- function (
    base_size = 11,
    base_family = "",
    base_colour = "#c8d7dfff",
    axis_line_colour = "#c8d7dfff",
    axis_line_linewidth = 0.33,
    axis_ticks_colour = axis_line_colour,
    axis_ticks_linewidth = axis_line_linewidth,
    axis_ticks_length_x_pt = base_size / 3,
    axis_ticks_length_y_pt = base_size / 4,
    panel_grid_colour = "#00040aff",
    panel_grid_linewidth = 1.33,
    panel_background_fill = "#050d1bff",
    plot_background_fill = "#00040aff",
    legend_background_fill = plot_background_fill,
    legend_key_fill = plot_background_fill,
    legend_ticks_colour = plot_background_fill,
    legend_ticks_linewidth = 0.33,
    orientation = NULL,
    ...
) {

  foundation_mode_n(
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
    plot_caption_colour = scales::alpha(base_colour, 0.75),
    plot_caption_hjust = 0,

    axis_line_colour = axis_line_colour,
    axis_line_linewidth = axis_line_linewidth,
    axis_ticks_colour = axis_ticks_colour,
    axis_ticks_linewidth = axis_ticks_linewidth,
    axis_ticks_length_x_pt = axis_ticks_length_x_pt,
    axis_ticks_length_y_pt = axis_ticks_length_y_pt,
    legend_ticks_colour = legend_ticks_colour,
    legend_ticks_linewidth = legend_ticks_linewidth,
    panel_grid_colour = panel_grid_colour,
    panel_grid_linewidth = panel_grid_linewidth,
    panel_background_fill = panel_background_fill,
    plot_background_fill = plot_background_fill,
    legend_background_fill = legend_background_fill,
    legend_key_fill = legend_key_fill,
    orientation = orientation,
    ...
  )
}
