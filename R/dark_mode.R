#' Dark mode with right legend
#'
#' @description Dark mode with right legend using `darkness`and `linewidthness`.
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
#'     mode = dark_mode_r()
#'   )
#'
dark_mode_r <- function (
    size = 11,
    family = "") {

  flexi_mode_r(
    size = size,
    family = family,
    col_palette = darkness,
    linewidth_palette = linewidthness
  )
}

#' Dark mode with top legend
#'
#' @description Dark mode with top legend using `darkness`and `linewidthness`.
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
#'     mode = dark_mode_t()
#'   )
#'
dark_mode_t <- function (
    size = 11,
    family = "") {

  flexi_mode_t(
    size = size,
    family = family,
    col_palette = darkness,
    linewidth_palette = linewidthness
  )
}

#' Dark mode with bottom legend
#'
#' @description Dark mode with bottom legend using `darkness`and `linewidthness`.
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
#'     mode = dark_mode_b()
#'   )
#'
dark_mode_b <- function (
    size = 11,
    family = "") {

  flexi_mode_b(
    size = size,
    family = family,
    col_palette = darkness,
    linewidth_palette = linewidthness
  )
}

#' Dark mode with no legend
#'
#' @description Dark mode with no legend using `darkness`and `linewidthness`.
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
#'     mode = dark_mode_n()
#'   )
#'
dark_mode_n <- function (
    size = 11,
    family = "") {

  flexi_mode_n(
    size = size,
    family = family,
    col_palette = darkness,
    linewidth_palette = linewidthness
  )
}
