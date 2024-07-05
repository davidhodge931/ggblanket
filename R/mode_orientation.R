#' A transparent line element
#'
#' @description
#' A short-cut for `ggplot2::element_line(colour = "transparent")`.
#'
#' Assists in 'removing' a line element from the theme, regardless of how it is specified in the theme hierarchy.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(palmerpenguins)
#'
#' penguins |>
#'   gg_point(x = flipper_length_mm, y = body_mass_g) +
#'   light_mode_r() +
#'   theme(axis.line.y = element_line_transparent()) +
#'   theme(axis.ticks.y = element_line_transparent()) +
#'   theme(panel.grid.major.y = element_line_transparent())
#'
element_line_transparent <- function() {
  ggplot2::element_line(colour = "transparent")
}

#' Orientate a mode to "x"
#'
#' @description Theme components to add to a mode used used outside of a `gg_*` context.
#'
#' @return ggplot2 theme components.
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(palmerpenguins)
#'
#' penguins |>
#'   ggplot() +
#'   geom_point(aes(x = flipper_length_mm, y = body_mass_g)) +
#'   light_mode_r() +
#'   mode_orientation_to_x()
#'
mode_orientation_to_x <- function() {
  ggplot2::theme(
    panel.grid.major.x = element_line_transparent(),
    panel.grid.minor.x = element_line_transparent(),
    axis.line.y = element_line_transparent(),
    axis.ticks.y = element_line_transparent()
  )
}

#' Orientate a mode to "y"
#'
#' @description Theme components to add to a mode used used outside of a `gg_*` context.
#'
#' @return ggplot2 theme components.
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(palmerpenguins)
#'
#' penguins |>
#'   ggplot() +
#'   geom_bar(aes(y = island)) +
#'   light_mode_r() +
#'   mode_orientation_to_y()
#'
mode_orientation_to_y <- function() {
  ggplot2::theme(
    panel.grid.major.y = element_line_transparent(),
    panel.grid.minor.y = element_line_transparent(),
    axis.line.x = element_line_transparent(),
    axis.ticks.x = element_line_transparent(),
    axis.ticks.y = element_line_transparent()
  )
}
