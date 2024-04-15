#' Grey mode theme with right legend
#'
#' @description Grey mode theme with right legend using `greyness` and `linewidthness`.
#'
#' @inheritParams light_mode_r
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
grey_mode_r <- function (
    base_size = 11,
    base_family = "",
    base_colour = greyness[1],
    axis_line_colour = greyness[2],
    axis_line_linewidth = linewidthness[1],
    axis_ticks_colour = axis_line_colour,
    axis_ticks_linewidth = axis_line_linewidth,
    axis_ticks_length_x_pt = base_size / 3,
    axis_ticks_length_y_pt = base_size / 4,
    panel_grid_colour = greyness[3],
    panel_grid_linewidth = linewidthness[2],
    panel_background_fill = greyness[4],
    plot_background_fill = greyness[5],
    legend_background_fill = plot_background_fill,
    legend_key_fill = plot_background_fill,
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
    panel_grid_colour = panel_grid_colour,
    panel_grid_linewidth = panel_grid_linewidth,
    panel_background_fill = panel_background_fill,
    plot_background_fill = plot_background_fill,
    legend_background_fill = legend_background_fill,
    legend_key_fill = legend_key_fill,
    ...
  )
}

#' Grey mode theme with top legend
#'
#' @description Grey mode theme with top legend using `greyness` and `linewidthness`.
#'
#' @inheritParams light_mode_r
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
#'     mode = grey_mode_t()
#'   )
#'
grey_mode_t <- function (
    base_size = 11,
    base_family = "",
    base_colour = greyness[1],
    axis_line_colour = greyness[2],
    axis_line_linewidth = linewidthness[1],
    axis_ticks_colour = axis_line_colour,
    axis_ticks_linewidth = axis_line_linewidth,
    axis_ticks_length_x_pt = base_size / 3,
    axis_ticks_length_y_pt = base_size / 4,
    panel_grid_colour = greyness[3],
    panel_grid_linewidth = linewidthness[2],
    panel_background_fill = greyness[4],
    plot_background_fill = greyness[5],
    legend_background_fill = plot_background_fill,
    legend_key_fill = plot_background_fill,
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
    panel_grid_colour = panel_grid_colour,
    panel_grid_linewidth = panel_grid_linewidth,
    panel_background_fill = panel_background_fill,
    plot_background_fill = plot_background_fill,
    legend_background_fill = legend_background_fill,
    legend_key_fill = legend_key_fill,
    ...
  )
}

#' Grey mode theme with bottom legend
#'
#' @description Grey mode theme with bottom legend using `greyness` and `linewidthness`.
#'
#' @inheritParams light_mode_r
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
#'     mode = grey_mode_b()
#'   )
#'
grey_mode_b <- function (
    base_size = 11,
    base_family = "",
    base_colour = greyness[1],
    axis_line_colour = greyness[2],
    axis_line_linewidth = linewidthness[1],
    axis_ticks_colour = axis_line_colour,
    axis_ticks_linewidth = axis_line_linewidth,
    axis_ticks_length_x_pt = base_size / 3,
    axis_ticks_length_y_pt = base_size / 4,
    panel_grid_colour = greyness[3],
    panel_grid_linewidth = linewidthness[2],
    panel_background_fill = greyness[4],
    plot_background_fill = greyness[5],
    legend_background_fill = plot_background_fill,
    legend_key_fill = plot_background_fill,
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
    panel_grid_colour = panel_grid_colour,
    panel_grid_linewidth = panel_grid_linewidth,
    panel_background_fill = panel_background_fill,
    plot_background_fill = plot_background_fill,
    legend_background_fill = legend_background_fill,
    legend_key_fill = legend_key_fill,
    ...
  )
}

#' Grey mode theme with no legend
#'
#' @description Grey mode theme with no legend using `greyness` and `linewidthness`.
#'
#' @inheritParams light_mode_r
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
#'     mode = grey_mode_n()
#'   )
#'
grey_mode_n <- function (
    base_size = 11,
    base_family = "",
    base_colour = greyness[1],
    axis_line_colour = greyness[2],
    axis_line_linewidth = linewidthness[1],
    axis_ticks_colour = axis_line_colour,
    axis_ticks_linewidth = axis_line_linewidth,
    axis_ticks_length_x_pt = base_size / 3,
    axis_ticks_length_y_pt = base_size / 4,
    panel_grid_colour = greyness[3],
    panel_grid_linewidth = linewidthness[2],
    panel_background_fill = greyness[4],
    plot_background_fill = greyness[5],
    legend_background_fill = plot_background_fill,
    legend_key_fill = plot_background_fill,
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
    panel_grid_colour = panel_grid_colour,
    panel_grid_linewidth = panel_grid_linewidth,
    panel_background_fill = panel_background_fill,
    plot_background_fill = plot_background_fill,
    legend_background_fill = legend_background_fill,
    legend_key_fill = legend_key_fill,
    ...
  )
}
