#' @title Dark mode theme with right top legend
#'
#' @description Dark mode theme for a ggplot visualisation with legend at right top. It uses the colours `"#c8d7df"`, `"#010a1a"`, and `"#00040a"`.
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
#'     mode = dark_mode_rt()
#'   )
#'
dark_mode_rt <- function (
    base_size = 11,
    base_family = "") {

  mode_rt(
    base_size = base_size,
    base_family = base_family,
    col_pal = darkness
  )
}

#' @title Dark mode theme with right legend
#'
#' @description Dark mode theme for a ggplot visualisation with right legend. It uses the colours `"#c8d7df"`, `"#010a1a"`, and `"#00040a"`.
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
#'     mode = dark_mode_r()
#'   )
#'
dark_mode_r <- function (
    base_size = 11,
    base_family = "") {

  mode_r(
    base_size = base_size,
    base_family = base_family,
    col_pal = darkness
  )
}

#' @title Dark mode theme with bottom legend
#'
#' @description Dark mode theme for a ggplot visualisation with bottom legend. It uses the colours `"#c8d7df"`, `"#010a1a"`, and `"#00040a"`.
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
#'     mode = dark_mode_b()
#'   )
#'
dark_mode_b <- function (
    base_size = 11,
    base_family = "") {

  mode_b(
    base_size = base_size,
    base_family = base_family,
    col_pal = darkness
  )
}

#' @title Dark mode theme with top legend
#'
#' @description Dark mode theme for a ggplot visualisation with top legend. It uses the colours `"#c8d7df"`, `"#010a1a"`, and `"#00040a"`.
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
#'     mode = dark_mode_t()
#'   )
#'
dark_mode_t <- function (
    base_size = 11,
    base_family = "") {

  mode_t(
    base_size = base_size,
    base_family = base_family,
    col_pal = darkness
  )
}

#' @title Dark mode theme with no legend
#'
#' @description Dark mode theme for a ggplot visualisation with no legend. It uses the colours `"#c8d7df"`, `"#010a1a"`, and `"#00040a"`.
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
#'     mode = dark_mode_n()
#'   )
#'
dark_mode_n <- function (
    base_size = 11,
    base_family = "") {

  mode_t(
    base_size = base_size,
    base_family = base_family,
    col_pal = darkness
  )
}
