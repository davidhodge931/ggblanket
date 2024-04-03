#' Dark mode theme with right legend
#'
#' @description Dark mode theme for a ggplot visualisation with legend at right. It uses the colours from `darkness`.
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
#'     mode = dark_mode_r()
#'   )
#'
dark_mode_r <- function (
    base_size = 11,
    base_family = "") {

  base_mode_r(
    base_size = base_size,
    base_family = base_family,
    base_pal = c(
      "text" = as.vector(darkness["text"]),
      "line" = as.vector(darkness["line"]),
      "panel" = as.vector(darkness["panel"]),
      "plot" = as.vector(darkness["plot"]),
      "grid" = as.vector(darkness["grid"]))
  )
}

#' Dark mode theme with top legend
#'
#' @description Dark mode theme for a ggplot visualisation with top legend. It uses the colours from `darkness`.
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
#'     mode = dark_mode_t()
#'   )
#'
dark_mode_t <- function (
    base_size = 11,
    base_family = "") {

  base_mode_t(
    base_size = base_size,
    base_family = base_family,
    base_pal = c(
      "text" = as.vector(darkness["text"]),
      "line" = as.vector(darkness["line"]),
      "panel" = as.vector(darkness["panel"]),
      "plot" = as.vector(darkness["plot"]),
      "grid" = as.vector(darkness["grid"]))
  )
}

#' Dark mode theme with bottom legend
#'
#' @description Dark mode theme for a ggplot visualisation with bottom legend. It uses the colours from `darkness`.
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
#'     mode = dark_mode_b()
#'   )
#'
dark_mode_b <- function (
    base_size = 11,
    base_family = "") {

  base_mode_b(
    base_size = base_size,
    base_family = base_family,
    base_pal = c(
      "text" = as.vector(darkness["text"]),
      "line" = as.vector(darkness["line"]),
      "panel" = as.vector(darkness["panel"]),
      "plot" = as.vector(darkness["plot"]),
      "grid" = as.vector(darkness["grid"]))
  )
}

#' Dark mode theme with no legend
#'
#' @description Dark mode theme for a ggplot visualisation with no legend. It uses the colours from `darkness`.
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
#'     mode = dark_mode_n()
#'   )
#'
dark_mode_n <- function (
    base_size = 11,
    base_family = "") {

  base_mode_n(
    base_size = base_size,
    base_family = base_family,
    base_pal = c(
      "text" = as.vector(darkness["text"]),
      "line" = as.vector(darkness["line"]),
      "panel" = as.vector(darkness["panel"]),
      "plot" = as.vector(darkness["plot"]),
      "grid" = as.vector(darkness["grid"]))
    )
}
