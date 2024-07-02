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
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(),
    axis.line.y.left = ggplot2::element_blank(),
    axis.line.y.right = ggplot2::element_blank(),
    axis.ticks.y.left = ggplot2::element_blank(),
    axis.ticks.y.right = ggplot2::element_blank(),
    axis.minor.ticks.y.left = ggplot2::element_blank(),
    axis.minor.ticks.y.right = ggplot2::element_blank()
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
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank(),
    axis.line.x.top = ggplot2::element_blank(),
    axis.line.x.bottom = ggplot2::element_blank(),
    axis.ticks.x.top = ggplot2::element_blank(),
    axis.ticks.x.bottom = ggplot2::element_blank(),
    axis.minor.ticks.x.top = ggplot2::element_blank(),
    axis.minor.ticks.x.bottom = ggplot2::element_blank()
  )
}
