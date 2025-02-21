#' Theme components to adjust to x orientation
#'
#' @param axis_line_rm `TRUE` or `FALSE` of whether to remove the relevant axis line.
#' @param axis_ticks_rm `TRUE` or `FALSE` of whether to remove the relevant axis ticks.
#' @param panel_grid_rm `TRUE` or `FALSE` of whether to remove the relevant panel grid.
#'
#' @return Theme components.
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(palmerpenguins)
#'
#' set_blanket()
#'
#' penguins |>
#'   ggplot() +
#'   geom_point(aes(x = flipper_length_mm, y = body_mass_g)) +
#'   scale_y_symmetric(penguins, body_mass_g) +
#'   theme_x_orientation() +
#'   coord_cartesian(clip = "off")
#'
theme_x_orientation <- function(
    axis_line_rm = TRUE,
    axis_ticks_rm = TRUE,
    panel_grid_rm = TRUE) {

  theme <- ggplot2::theme()
  if (axis_line_rm) theme <- theme + ggplot2::theme(axis.line.y = ggplot2::element_line(colour = "transparent"))
  if (axis_ticks_rm) theme <- theme + ggplot2::theme(axis.ticks.y = ggplot2::element_line(colour = "transparent"))
  if (panel_grid_rm) theme <- theme + ggplot2::theme(panel.grid.major.x = ggplot2::element_line(colour = "transparent")) +
      ggplot2::theme(panel.grid.minor.x = ggplot2::element_line(colour = "transparent"))

  theme
}

#' Theme components to adjust to y orientation
#'
#' @param axis_line_rm `TRUE` or `FALSE` of whether to remove the relevant axis line.
#' @param axis_ticks_rm `TRUE` or `FALSE` of whether to remove the relevant axis ticks.
#' @param panel_grid_rm `TRUE` or `FALSE` of whether to remove the relevant panel grid.
#'
#' @return Theme components.
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(palmerpenguins)
#'
#' set_blanket()
#'
#' penguins |>
#'   ggplot() +
#'   geom_jitter(aes(x = body_mass_g, y = species)) +
#'   scale_x_symmetric(penguins, body_mass_g) +
#'   theme_y_orientation() +
#'   theme(axis.ticks.y = element_line_transparent()) +
#'   coord_cartesian(clip = "off") +
#'   labs(x = "Body mass g", y = "Species")
#'
theme_y_orientation <- function(
    axis_line_rm = TRUE,
    axis_ticks_rm = TRUE,
    panel_grid_rm = TRUE) {

  theme <- ggplot2::theme()
  if (axis_line_rm) theme <- theme + ggplot2::theme(axis.line.x = ggplot2::element_line(colour = "transparent"))
  if (axis_ticks_rm) theme <- theme + ggplot2::theme(axis.ticks.x = ggplot2::element_line(colour = "transparent"))
  if (panel_grid_rm) theme <- theme + ggplot2::theme(panel.grid.major.y = ggplot2::element_line(colour = "transparent")) +
      ggplot2::theme(panel.grid.minor.y = ggplot2::element_line(colour = "transparent"))

  theme
}

#' A transparent line element
#'
#' @param ... Passed to [element_line()][ggplot2::element_line()].
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(palmerpenguins)
#'
#' set_blanket()
#'
#' penguins |>
#'   ggplot() +
#'   geom_jitter(aes(x = body_mass_g, y = species)) +
#'   scale_x_symmetric(penguins, body_mass_g) +
#'   theme_y_orientation() +
#'   theme(axis.ticks.y = element_line_transparent()) +
#'   coord_cartesian(clip = "off") +
#'   labs(x = "Body mass g", y = "Species")
#'
element_line_transparent <- function(...) {
  ggplot2::element_line(colour = "transparent", ...)
}
