#' Annotated panel background rectangles
#'
#' @description Add background rectangles to highlight specific regions of a plot.
#' Useful for indicating periods of uncertainty, different phases, or other regions of interest.
#' It requires a `coord` of `ggplot2::coord_cartesian(clip = "off")`.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param x A vector of length 2. Defaults to c(-Inf, Inf).
#' @param y A vector of length 2. Defaults to c(-Inf, Inf).
#' @param fill The fill colour of the rectangle. Defaults to "#8991A1FF".
#' @param alpha The transparency of the rectangle. Defaults to 0.25.
#' @param colour The border colour of the rectangle. Defaults to "transparent".
#' @param linewidth The border linewidth of the rectangle. Defaults to 0.
#' @param linetype The border linetype of the rectangle. Defaults to 1.
#'
#' @return A list containing an annotation layer.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' palmerpenguins::penguins |>
#'   gg_blanket(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = species,
#'   ) +
#'   annotate_panel_background(
#'     x = c(225, Inf),
#'   ) +
#'   geom_point()
#'
annotate_panel_background <- function(
    ...,
    x = c(-Inf, Inf),
    y = c(-Inf, Inf),
    fill = "#8991A1FF",
    alpha = 0.25,
    colour = "transparent",
    linewidth = 0,
    linetype = 1
) {
  # Validate x and y arguments
  if (length(x) != 2) {
    rlang::abort("x must be a vector of length 2: c(xmin, xmax)")
  }

  if (length(y) != 2) {
    rlang::abort("y must be a vector of length 2: c(ymin, ymax)")
  }

  if (x[1] > x[2]) {
    rlang::abort("x[1] (xmin) must be less than or equal to x[2] (xmax)")
  }

  if (y[1] > y[2]) {
    rlang::abort("y[1] (ymin) must be less than or equal to y[2] (ymax)")
  }

  # Create rectangle annotation
  stamp <- list(
    ggplot2::annotate(
      geom = "rect",
      xmin = x[1],
      xmax = x[2],
      ymin = y[1],
      ymax = y[2],
      fill = fill,
      colour = colour,
      linewidth = linewidth,
      linetype = linetype,
      alpha = alpha
    )
  )

  return(stamp)
}
