#' @title Light mode theme with right legend
#'
#' @description Light mode theme for a ggplot visualisation with legend at right. It uses the colours from `lightness`.
#'
#' @inheritParams base_mode_r
#'
#' @return A ggplot mode.
#' @export
#'
#' @examples
#' library(palmerpenguins)
#' library(ggplot2)
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
    base_size = 11,
    base_family = "",
    x_title = TRUE,
    y_title = TRUE) {

  base_mode_r(
    base_size = base_size,
    base_family = base_family,
    x_title = x_title,
    y_title = y_title,
    col_pal = c(
      "text" = lightness[1],
      "axis_line" = lightness[1],
      "panel_background" = lightness[2],
      "plot_background" = lightness[2],
      "panel_grid" = lightness[3])
  )
}

#' @title Light mode theme with top legend
#'
#' @description Light mode theme for a ggplot visualisation with top legend. It uses the colours from `lightness`.
#'
#' @inheritParams base_mode_t
#'
#' @return A ggplot mode.
#' @export
#'
#' @examples
#' library(palmerpenguins)
#' library(ggplot2)
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
    base_size = 11,
    base_family = "",
    x_title = TRUE,
    y_title = TRUE) {

  base_mode_t(
    base_size = base_size,
    base_family = base_family,
    x_title = x_title,
    y_title = y_title,
    col_pal = c(
      "text" = lightness[1],
      "axis_line" = lightness[1],
      "panel_background" = lightness[2],
      "plot_background" = lightness[2],
      "panel_grid" = lightness[3])
  )
}

#' @title Light mode theme with bottom legend
#'
#' @description Light mode theme for a ggplot visualisation with bottom legend. It uses the colours from `lightness`.
#'
#' @inheritParams base_mode_b
#'
#' @return A ggplot mode.
#' @export
#'
#' @examples
#' library(palmerpenguins)
#' library(ggplot2)
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
    base_size = 11,
    base_family = "",
    x_title = TRUE,
    y_title = TRUE) {

  base_mode_b(
    base_size = base_size,
    base_family = base_family,
    x_title = x_title,
    y_title = y_title,
    col_pal = c(
      "text" = lightness[1],
      "axis_line" = lightness[1],
      "panel_background" = lightness[2],
      "plot_background" = lightness[2],
      "panel_grid" = lightness[3])
  )
}

#' @title Light mode theme with no legend
#'
#' @description Light mode theme for a ggplot visualisation with no legend. It uses the colours from `lightness`.
#'
#' @inheritParams base_mode_n
#'
#' @return A ggplot mode.
#' @export
#'
#' @examples
#' library(palmerpenguins)
#' library(ggplot2)
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
    base_size = 11,
    base_family = "",
    x_title = TRUE,
    y_title = TRUE) {

  base_mode_n(
    base_size = base_size,
    base_family = base_family,
    x_title = x_title,
    y_title = y_title,
    col_pal = c(
      "text" = lightness[1],
      "axis_line" = lightness[1],
      "panel_background" = lightness[2],
      "plot_background" = lightness[2],
      "panel_grid" = lightness[3])
  )
}
