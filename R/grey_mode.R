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
    text_size = 11,
    text_family = "",
    text_colour = greyness[1],
    ...
) {

  get_mode_r(
    text_size = text_size,
    text_family = text_family,
    text_colour = text_colour,
    axis_line_colour = greyness[2],
    axis_line_linewidth = linewidthness[1],
    panel_line_colour = greyness[3],
    panel_line_linewidth = linewidthness[2],
    panel_background_colour = greyness[4],
    plot_background_colour = greyness[5],
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
    text_size = 11,
    text_family = "",
    text_colour = greyness[1],
    ...
) {

  get_mode_t(
    text_size = text_size,
    text_family = text_family,
    text_colour = text_colour,
    axis_line_colour = greyness[2],
    axis_line_linewidth = linewidthness[1],
    panel_line_colour = greyness[3],
    panel_line_linewidth = linewidthness[2],
    panel_background_colour = greyness[4],
    plot_background_colour = greyness[5],
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
    text_size = 11,
    text_family = "",
    text_colour = greyness[1],
    ...
) {

  get_mode_b(
    text_size = text_size,
    text_family = text_family,
    text_colour = text_colour,
    axis_line_colour = greyness[2],
    axis_line_linewidth = linewidthness[1],
    panel_line_colour = greyness[3],
    panel_line_linewidth = linewidthness[2],
    panel_background_colour = greyness[4],
    plot_background_colour = greyness[5],
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
    text_size = 11,
    text_family = "",
    text_colour = greyness[1],
    ...
) {

  get_mode_n(
    text_size = text_size,
    text_family = text_family,
    text_colour = text_colour,
    axis_line_colour = greyness[2],
    axis_line_linewidth = linewidthness[1],
    panel_line_colour = greyness[3],
    panel_line_linewidth = linewidthness[2],
    panel_background_colour = greyness[4],
    plot_background_colour = greyness[5],
    ...
  )
}
