#' Dark mode theme with right legend
#'
#' @description Dark mode theme with right legend using `darkness` and `linewidthness`.
#'
#' @param base_size The base size of the text. Defaults to 11. The title is 110% of this, caption 85% and tag 120%.
#' @param base_family The family of the text. Defaults to "".
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
    ...
) {

  get_mode_r(
    base_size = base_size,
    base_family = base_family,
    base_colour = darkness[1],
    axis_line_colour = darkness[2],
    axis_line_linewidth = linewidthness[1],
    panel_line_colour = darkness[3],
    panel_line_linewidth = linewidthness[2],
    panel_background_colour = darkness[4],
    plot_background_colour = darkness[5],
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
    ...
) {

  get_mode_t(
    base_size = base_size,
    base_family = base_family,
    base_colour = darkness[1],
    axis_line_colour = darkness[2],
    axis_line_linewidth = linewidthness[1],
    panel_line_colour = darkness[3],
    panel_line_linewidth = linewidthness[2],
    panel_background_colour = darkness[4],
    plot_background_colour = darkness[5],
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
    ...
) {

  get_mode_b(
    base_size = base_size,
    base_family = base_family,
    base_colour = darkness[1],
    axis_line_colour = darkness[2],
    axis_line_linewidth = linewidthness[1],
    panel_line_colour = darkness[3],
    panel_line_linewidth = linewidthness[2],
    panel_background_colour = darkness[4],
    plot_background_colour = darkness[5],
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
    ...
) {

  get_mode_n(
    base_size = base_size,
    base_family = base_family,
    base_colour = darkness[1],
    axis_line_colour = darkness[2],
    axis_line_linewidth = linewidthness[1],
    panel_line_colour = darkness[3],
    panel_line_linewidth = linewidthness[2],
    panel_background_colour = darkness[4],
    plot_background_colour = darkness[5],
    ...
  )
}
