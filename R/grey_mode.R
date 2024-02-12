#' @title Grey ggplot theme with right top legend
#'
#' @description Grey theme for a ggplot visualisation with legend at right top. It uses the colours `"#121b24"`, `"#fcfdfe"`, and `"#f6f8fa"`.
#'
#' @inheritParams mode_rt
#'
#' @return A ggplot theme.
#' @export
#'
#' @examples
#' library(palmerpenguins)
#' library(ggplot2)
#'
#' #for a plot
#' penguins |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = species,
#'     theme = grey_mode_rt()
#'   )
#'
#' #set globally
#' \dontrun{
#'   theme_set(grey_mode_rt())
#'
#'   penguins |>
#'     gg_point(
#'       x = flipper_length_mm,
#'       y = body_mass_g,
#'       col = species
#'     )
#' }
#'
grey_mode_rt <- function (
    base_size = 11,
    base_family = "") {

  mode_rt(
    base_size = base_size,
    base_family = base_family,
    col_pal = greyness
  )
}

#' @title Grey ggplot theme with right legend
#'
#' @description Grey theme for a ggplot visualisation with right legend. It uses the colours `"#121b24"`, `"#fcfdfe"`, and `"#f6f8fa"`.
#'
#' @inheritParams mode_r
#'
#' @return A ggplot theme.
#' @export
#'
#' @examples
#' library(palmerpenguins)
#' library(ggplot2)
#'
#' #for a plot
#' penguins |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = species,
#'     theme = grey_mode_r()
#'   )
#'
#' #set globally
#' \dontrun{
#'   theme_set(grey_mode_r())
#'
#'   penguins |>
#'     gg_point(
#'       x = flipper_length_mm,
#'       y = body_mass_g,
#'       col = species
#'     )
#' }
#'
grey_mode_r <- function (
    base_size = 11,
    base_family = "") {

  mode_r(
    base_size = base_size,
    base_family = base_family,
    col_pal = greyness
  )
}

#' @title Grey ggplot theme with bottom legend
#'
#' @description Grey theme for a ggplot visualisation with bottom legend. It uses the colours `"#121b24"`, `"#fcfdfe"`, and `"#f6f8fa"`.
#'
#' @inheritParams mode_b
#'
#' @return A ggplot theme.
#' @export
#'
#' @examples
#' library(palmerpenguins)
#' library(ggplot2)
#'
#' #for a plot
#' penguins |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = species,
#'     theme = grey_mode_b()
#'   )
#'
#' #set globally
#' \dontrun{
#'   theme_set(grey_mode_b())
#'
#'   penguins |>
#'     gg_point(
#'       x = flipper_length_mm,
#'       y = body_mass_g,
#'       col = species
#'     )
#' }
#'
grey_mode_b <- function (
    base_size = 11,
    base_family = "") {

  mode_b(
    base_size = base_size,
    base_family = base_family,
    col_pal = greyness
  )
}

#' @title Grey ggplot theme with top legend
#'
#' @description Grey theme for a ggplot visualisation with top legend. It uses the colours `"#121b24"`, `"#fcfdfe"`, and `"#f6f8fa"`.
#'
#' @inheritParams mode_t
#'
#' @return A ggplot theme.
#' @export
#'
#' @examples
#' library(palmerpenguins)
#' library(ggplot2)
#'
#' #for a plot
#' penguins |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = species,
#'     theme = grey_mode_t()
#'   )
#'
#' #set globally
#' \dontrun{
#'   theme_set(grey_mode_t())
#'
#'   penguins |>
#'     gg_point(
#'       x = flipper_length_mm,
#'       y = body_mass_g,
#'       col = species
#'     )
#' }
#'
grey_mode_t <- function (
    base_size = 11,
    base_family = "") {

  mode_t(
    base_size = base_size,
    base_family = base_family,
    col_pal = greyness
  )
}

#' @title Grey ggplot theme with no legend
#'
#' @description Grey theme for a ggplot visualisation with no legend. It uses the colours `"#121b24"`, `"#fcfdfe"`, and `"#f6f8fa"`.
#'
#' @inheritParams mode_n
#'
#' @return A ggplot theme.
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
#'     theme = grey_mode_n()
#'   )
#'
grey_mode_n <- function (
    base_size = 11,
    base_family = "") {

  mode_n(
    base_size = base_size,
    base_family = base_family,
    col_pal = greyness
  )
}
