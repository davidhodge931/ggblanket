#' Light mode theme with right legend
#'
#' @description Light mode theme with right legend using `lightness` and `linewidthness`.
#'
#' @param text_size The base size of the text. Defaults to 11. The title is 110% of this, caption 85% and tag 120%.
#' @param text_family The family of the text. Defaults to "".
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
#'     mode = light_mode_r()
#'   )
#'
light_mode_r <- function (
    text_size = 11,
    text_family = "",
    ...
  ) {

  get_mode_r(
    text_size = text_size,
    text_family = text_family,
    text_colour = lightness[1],
    axis_line_colour = lightness[2],
    axis_line_linewidth = linewidthness[1],
    panel_line_colour = lightness[3],
    panel_line_linewidth = linewidthness[2],
    panel_background_colour = lightness[4],
    plot_background_colour = lightness[5],
    ...
  )
}

#' Light mode theme with top legend
#'
#' @description Light mode theme with top legend using `lightness` and `linewidthness`.
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
#'     mode = light_mode_t()
#'   )
#'
light_mode_t <- function (
    text_size = 11,
    text_family = "",
    ...
) {

  get_mode_t(
    text_size = text_size,
    text_family = text_family,
    text_colour = lightness[1],
    axis_line_colour = lightness[2],
    axis_line_linewidth = linewidthness[1],
    panel_line_colour = lightness[3],
    panel_line_linewidth = linewidthness[2],
    panel_background_colour = lightness[4],
    plot_background_colour = lightness[5],
    ...
  )
}

#' Light mode theme with bottom legend
#'
#' @description Light mode theme with bottom legend using `lightness` and `linewidthness`.
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
#'     mode = light_mode_b()
#'   )
#'
light_mode_b <- function (
    text_size = 11,
    text_family = "",
    ...
) {

  get_mode_b(
    text_size = text_size,
    text_family = text_family,
    text_colour = lightness[1],
    axis_line_colour = lightness[2],
    axis_line_linewidth = linewidthness[1],
    panel_line_colour = lightness[3],
    panel_line_linewidth = linewidthness[2],
    panel_background_colour = lightness[4],
    plot_background_colour = lightness[5],
    ...
  )
}

#' Light mode theme with no legend
#'
#' @description Light mode theme with no legend using `lightness` and `linewidthness`.
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
#'     mode = light_mode_n()
#'   )
#'
light_mode_n <- function (
    text_size = 11,
    text_family = "",
    ...
) {

  get_mode_n(
    text_size = text_size,
    text_family = text_family,
    text_colour = lightness[1],
    axis_line_colour = lightness[2],
    axis_line_linewidth = linewidthness[1],
    panel_line_colour = lightness[3],
    panel_line_linewidth = linewidthness[2],
    panel_background_colour = lightness[4],
    plot_background_colour = lightness[5],
    ...
  )
}
