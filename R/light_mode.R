#' @title Light mode theme with right legend
#'
#' @description Light mode theme for a ggplot visualisation with legend at right. It uses the colours from `lightness`.
#'
#' @inheritParams base_mode_r
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
    base_size = 11,
    base_family = "") {

  base_mode_r(
    base_size = base_size,
    base_family = base_family,
    base_pal = c(
      "text" = lightness[2],
      "line" = lightness[2],
      "panel" = lightness[3],
      "plot" = lightness[3],
      "grid" = lightness[1])
  )
}

#' @title Light mode theme with top legend
#'
#' @description Light mode theme for a ggplot visualisation with top legend. It uses the colours from `lightness`.
#'
#' @inheritParams base_mode_t
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
    base_size = 11,
    base_family = "") {

  base_mode_t(
    base_size = base_size,
    base_family = base_family,
    base_pal = c(
      "text" = lightness[2],
      "line" = lightness[2],
      "panel" = lightness[3],
      "plot" = lightness[3],
      "grid" = lightness[1])
  )
}

#' @title Light mode theme with bottom legend
#'
#' @description Light mode theme for a ggplot visualisation with bottom legend. It uses the colours from `lightness`.
#'
#' @inheritParams base_mode_b
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
    base_size = 11,
    base_family = "") {

  base_mode_b(
    base_size = base_size,
    base_family = base_family,
    base_pal = c(
      "text" = lightness[2],
      "line" = lightness[2],
      "panel" = lightness[3],
      "plot" = lightness[3],
      "grid" = lightness[1])
  )
}

#' @title Light mode theme with no legend
#'
#' @description Light mode theme for a ggplot visualisation with no legend. It uses the colours from `lightness`.
#'
#' @inheritParams base_mode_n
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
    base_size = 11,
    base_family = "") {

  base_mode_n(
    base_size = base_size,
    base_family = base_family,
    base_pal = c(
      "text" = lightness[2],
      "line" = lightness[2],
      "panel" = lightness[3],
      "plot" = lightness[3],
      "grid" = lightness[1])
  )
}
