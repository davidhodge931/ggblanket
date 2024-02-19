#' @title Dark mode theme with right legend
#'
#' @description Dark mode theme for a ggplot visualisation with legend at right. It uses the colours from `darkness`.
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
#'     mode = dark_mode_r()
#'   )
#'
dark_mode_r <- function (
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
      "text" = darkness[2],
      "axis_line" = darkness[2],
      "panel_background" = darkness[3],
      "plot_background" = darkness[1],
      "panel_grid" = darkness[1])
  )
}

#' @title Dark mode theme with top legend
#'
#' @description Dark mode theme for a ggplot visualisation with top legend. It uses the colours from `darkness`.
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
#'     mode = dark_mode_t()
#'   )
#'
dark_mode_t <- function (
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
      "text" = darkness[2],
      "axis_line" = darkness[2],
      "panel_background" = darkness[3],
      "plot_background" = darkness[1],
      "panel_grid" = darkness[1])
  )
}

#' @title Dark mode theme with bottom legend
#'
#' @description Dark mode theme for a ggplot visualisation with bottom legend. It uses the colours from `darkness`.
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
#'     mode = dark_mode_b()
#'   )
#'
dark_mode_b <- function (
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
      "text" = darkness[2],
      "axis_line" = darkness[2],
      "panel_background" = darkness[3],
      "plot_background" = darkness[1],
      "panel_grid" = darkness[1])
  )
}

#' @title Dark mode theme with no legend
#'
#' @description Dark mode theme for a ggplot visualisation with no legend. It uses the colours from `darkness`.
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
#'     mode = dark_mode_n()
#'   )
#'
dark_mode_n <- function (
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
      "text" = darkness[2],
      "axis_line" = darkness[2],
      "panel_background" = darkness[3],
      "plot_background" = darkness[1],
      "panel_grid" = darkness[1])
  )
}
