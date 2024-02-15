#' @title Grey mode theme with right top legend
#'
#' @description Grey mode theme for a ggplot visualisation with legend at right top. It uses the colours from `greyness`.
#'
#' @inheritParams base_mode
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
#'     mode = grey_mode_rt()
#'   )
#'
grey_mode_rt <- function (
    base_size = 11,
    base_family = "") {

  base_mode_rt(
    base_size = base_size,
    base_family = base_family,
    col_pal = c(
      "text" = greyness[1],
      "axis_line" = greyness[1],
      "panel_background" = greyness[2],
      "plot_background" = greyness[3],
      "panel_grid" = greyness[3])
  )
}

#' @title Grey mode theme with right legend
#'
#' @description Grey mode theme for a ggplot visualisation with right legend. It uses the colours from `greyness`.
#'
#' @inheritParams base_mode
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
#'     mode = grey_mode_r()
#'   )
#'
grey_mode_r <- function (
    base_size = 11,
    base_family = "") {

  base_mode_r(
    base_size = base_size,
    base_family = base_family,
    col_pal = c(
      "text" = greyness[1],
      "axis_line" = greyness[1],
      "panel_background" = greyness[2],
      "plot_background" = greyness[3],
      "panel_grid" = greyness[3])
  )
}

#' @title Grey mode theme with top legend
#'
#' @description Grey mode theme for a ggplot visualisation with top legend. It uses the colours from `greyness`.
#'
#' @inheritParams base_mode
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
#'     mode = grey_mode_t()
#'   )
#'
grey_mode_t <- function (
    base_size = 11,
    base_family = "") {

  base_mode_t(
    base_size = base_size,
    base_family = base_family,
    col_pal = c(
      "text" = greyness[1],
      "axis_line" = greyness[1],
      "panel_background" = greyness[2],
      "plot_background" = greyness[3],
      "panel_grid" = greyness[3])
  )
}

#' @title Grey mode theme with bottom legend
#'
#' @description Grey mode theme for a ggplot visualisation with bottom legend. It uses the colours from `greyness`.
#'
#' @inheritParams base_mode
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
#'     mode = grey_mode_b()
#'   )
#'
grey_mode_b <- function (
    base_size = 11,
    base_family = "") {

  base_mode_b(
    base_size = base_size,
    base_family = base_family,
    col_pal = c(
      "text" = greyness[1],
      "axis_line" = greyness[1],
      "panel_background" = greyness[2],
      "plot_background" = greyness[3],
      "panel_grid" = greyness[3])
  )
}

#' @title Grey mode theme with no legend
#'
#' @description Grey mode theme for a ggplot visualisation with no legend. It uses the colours from `greyness`.
#'
#' @inheritParams base_mode
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
#'     mode = grey_mode_n()
#'   )
#'
grey_mode_n <- function (
    base_size = 11,
    base_family = "") {

  base_mode_n(
    base_size = base_size,
    base_family = base_family,
    col_pal = c(
      "text" = greyness[1],
      "axis_line" = greyness[1],
      "panel_background" = greyness[2],
      "plot_background" = greyness[3],
      "panel_grid" = greyness[3])
  )
}


