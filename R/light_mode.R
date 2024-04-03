#' Light mode theme with right legend
#'
#' @description Light mode theme with right legend using `lightness` and `linewidthness`.
#'
#' @inheritParams flexi_mode_r
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
    size = 11,
    family = "") {

  flexi_mode_r(
    size = size,
    family = family,
    col_pal = lightness,
    linewidth_pal = linewidthness
  )
}

#' Light mode theme with top legend
#'
#' @description Light mode theme with top legend using `lightness` and `linewidthness`.
#'
#' @inheritParams flexi_mode_t
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
    size = 11,
    family = "") {

  flexi_mode_t(
    size = size,
    family = family,
    col_pal = lightness,
    linewidth_pal = linewidthness
  )
}

#' Light mode theme with bottom legend
#'
#' @description Light mode theme with bottom legend using `lightness` and `linewidthness`.
#'
#' @inheritParams flexi_mode_b
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
    size = 11,
    family = "") {

  flexi_mode_b(
    size = size,
    family = family,
    col_pal = lightness,
    linewidth_pal = linewidthness
  )
}

#' Light mode theme with no legend
#'
#' @description Light mode theme with no legend using `lightness` and `linewidthness`.
#'
#' @inheritParams flexi_mode_n
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
    size = 11,
    family = "") {

  flexi_mode_n(
    size = size,
    family = family,
    col_pal = lightness,
    linewidth_pal = linewidthness
  )
}
