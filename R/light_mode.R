#' @title Light ggplot theme with right top legend
#'
#' @description Light theme for a ggplot visualisation with legend at right top. It uses the colours `"#121b24"`, `"#fcfdfe"`, and `"#e6ecf2"`.
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
#' #set for a plot
#' penguins |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = species,
#'     theme = light_mode_rt()
#'   )
#'
#' #set globally
#' \dontrun{
#'   theme_set(light_mode_rt())
#'
#'   penguins |>
#'     gg_point(
#'       x = flipper_length_mm,
#'       y = body_mass_g,
#'       col = species
#'     )
#' }
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

#' @title Light ggplot theme with right legend
#'
#' @description Light theme for a ggplot visualisation with right legend. It uses the colours `"#121b24"`, `"#fcfdfe"`, and `"#e6ecf2"`.
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
#' #set for a plot
#' penguins |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = species,
#'     theme = light_mode_r()
#'   )
#'
#' #set globally
#' \dontrun{
#'   theme_set(light_mode_r())
#'
#'   penguins |>
#'     gg_point(
#'       x = flipper_length_mm,
#'       y = body_mass_g,
#'       col = species
#'     )
#' }
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

#' @title Light ggplot theme with bottom legend
#'
#' @description Light theme for a ggplot visualisation with bottom legend. It uses the colours `"#121b24"`, `"#fcfdfe"`, and `"#e6ecf2"`.
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
#' #set for a plot
#' penguins |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = species,
#'     theme = light_mode_b()
#'   )
#'
#' #set globally
#' \dontrun{
#'   theme_set(light_mode_b())
#'
#'   penguins |>
#'     gg_point(
#'       x = flipper_length_mm,
#'       y = body_mass_g,
#'       col = species
#'     )
#' }
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

#' @title Light ggplot theme with top legend
#'
#' @description Light theme for a ggplot visualisation with top legend. It uses the colours `"#121b24"`, `"#fcfdfe"`, and `"#e6ecf2"`.
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
#' #set for a plot
#' penguins |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = species,
#'     theme = light_mode_t()
#'   )
#'
#' #set globally
#' \dontrun{
#'   theme_set(light_mode_t())
#'
#'   penguins |>
#'     gg_point(
#'       x = flipper_length_mm,
#'       y = body_mass_g,
#'       col = species
#'     )
#' }
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

#' @title Light ggplot theme with inside legend
#'
#' @description Light theme for a ggplot visualisation with legend inside the panel. It uses the colours `"#121b24"`, `"#fcfdfe"`, and `"#e6ecf2"`.
#'
#' @inheritParams mode_i
#'
#' @return A ggplot theme.
#' @export
#'
#' @examples
#' library(palmerpenguins)
#' library(ggplot2)
#'
#' #set for a plot
#' penguins |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = species,
#'     theme = light_mode_i(legend_position_inside = c(0.15, 0.7))
#'   )
#'
#' #set globally
#' \dontrun{
#'   theme_set(light_mode_i(legend_position_inside = c(0.15, 0.7)))
#'
#'   penguins |>
#'     gg_point(
#'       x = flipper_length_mm,
#'       y = body_mass_g,
#'       col = species
#'     )
#' }
#'
light_mode_i <- function (
    base_size = 11,
    base_family = "",
    legend_position_inside = c(0.5, 0.5)) {

  mode_i(
    base_size = base_size,
    base_family = base_family,
    legend_position_inside = legend_position_inside,
    col_pal = lightness
  )
}
