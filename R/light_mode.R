#' @title Light mode theme with right top legend
#'
#' @description Light mode theme for a ggplot visualisation with legend at right top. It uses the colours `"#121B24"`, `"#FFFFFF"`, and `"#F6F8FA"`.
#'
#' @inheritParams mode_rt
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
#'     mode = light_mode_rt()
#'   )
#'
light_mode_rt <- function (
    base_size = 11,
    base_family = "") {

  mode_rt(
    base_size = base_size,
    base_family = base_family,
    col_pal = lightness
  )
}

#' @title Light mode theme with right legend
#'
#' @description Light mode theme for a ggplot visualisation with right legend. It uses the colours `"#121B24"`, `"#FFFFFF"`, and `"#F6F8FA"`.
#'
#' @inheritParams mode_r
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
    base_family = "") {

  mode_r(
    base_size = base_size,
    base_family = base_family,
    col_pal = lightness
  )
}

#' @title Light mode theme with bottom legend
#'
#' @description Light mode theme for a ggplot visualisation with bottom legend. It uses the colours `"#121B24"`, `"#FFFFFF"`, and `"#F6F8FA"`.
#'
#' @inheritParams mode_b
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
    base_family = "") {

  mode_b(
    base_size = base_size,
    base_family = base_family,
    col_pal = lightness
  )
}

#' @title Light mode theme with top legend
#'
#' @description Light mode theme for a ggplot visualisation with top legend. It uses the colours `"#121B24"`, `"#FFFFFF"`, and `"#F6F8FA"`.
#'
#' @inheritParams mode_t
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
    base_family = "") {

  mode_t(
    base_size = base_size,
    base_family = base_family,
    col_pal = lightness
  )
}

#' @title Light mode theme with no legend
#'
#' @description Light mode theme for a ggplot visualisation with no legend. It uses the colours `"#121B24"`, `"#FFFFFF"`, and `"#F6F8FA"`.
#'
#' @inheritParams mode_n
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
    base_family = "") {

  mode_t(
    base_size = base_size,
    base_family = base_family,
    col_pal = lightness
  )
}


