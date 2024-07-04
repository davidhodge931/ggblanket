#' Orientate a mode to "x"
#'
#' @description Theme components to add to a mode used used outside of a `gg_*` context.
#'
#' @param axis_ticks_x TRUE or FALSE of whether to show x axis ticks.
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
mode_orientation_to_x <- function(axis_ticks_x = FALSE) {
  theme <- ggplot2::theme(
    panel.grid.major.x = ggplot2::element_line(colour = "transparent"),
    panel.grid.minor.x = ggplot2::element_line(colour = "transparent"),
    axis.line.y = ggplot2::element_line(colour = "transparent"),
    axis.ticks.y = ggplot2::element_line(colour = "transparent")
  )

  if (!axis_ticks_x) {
    theme <- theme +
      ggplot2::theme(
      axis.ticks.x = ggplot2::element_line(colour = "transparent")
    )
  }

  return(theme)
}

#' Orientate a mode to "y"
#'
#' @description Theme components to add to a mode used used outside of a `gg_*` context.
#'
#' @param axis_ticks_y TRUE or FALSE of whether to show y axis ticks.
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
mode_orientation_to_y <- function(axis_ticks_y = FALSE) {
  theme <- ggplot2::theme(
    panel.grid.major.y = ggplot2::element_line(colour = "transparent"),
    panel.grid.minor.y = ggplot2::element_line(colour = "transparent"),
    axis.line.x = ggplot2::element_line(colour = "transparent"),
    axis.ticks.x = ggplot2::element_line(colour = "transparent")
  )

  if (!axis_ticks_y) {
    theme <- theme +
      ggplot2::theme(
        axis.ticks.y = ggplot2::element_line(colour = "transparent")
      )
  }

  return(theme)
}
