#' A transparent line element
#'
#' @description
#' A short-cut for ggplot2::element_line(colour = "transparent").
#'
#' Assists in 'removing' a line element from the theme, regardless of how it is specified in the theme hierarchy.
#'
#' @noRd
#'
#' @examples
#' library(ggplot2)
#' library(palmerpenguins)
#'
#' penguins |>
#' gg_point(x = flipper_length_mm, y = body_mass_g) +
#'  light_mode_r() +
#'  theme(axis.line.y = element_line_transparent()) +
#'  theme(axis.ticks.y = element_line_transparent()) +
#'  theme(panel.grid.major.y = element_line_transparent())
#'
element_line_transparent <- function() {
  ggplot2::element_line(colour = "transparent")
}

#' Orientate a theme used outside of a `gg_*` context
#'
#' @description
#' Add theme components to a theme used outside of a `gg_*` context
#' * `theme_orientation_to_x()` Orientate the plot to the x axis.
#' * `theme_orientation_to_y()` Orientate the plot to the y axis.
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
#'   theme_orientation_to_x()
#'
#'
#' penguins |>
#'   ggplot() +
#'   geom_bar(aes(y = island)) +
#'   light_mode_r() +
#'   theme_orientation_to_y()
#'
theme_orientation_to_x <- function() {
  ggplot2::theme(
    panel.grid.major.x = element_line_transparent(),
    panel.grid.minor.x = element_line_transparent(),
    axis.line.y = element_line_transparent(),
    axis.ticks.y = element_line_transparent()
  )
}

#' @rdname theme_orientation_to_x
#' @export
theme_orientation_to_y <- function() {
  ggplot2::theme(
    panel.grid.major.y = element_line_transparent(),
    panel.grid.minor.y = element_line_transparent(),
    axis.line.x = element_line_transparent(),
    axis.ticks.x = element_line_transparent(),
    axis.ticks.y = element_line_transparent()
  )
}
