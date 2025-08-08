#' Annotate a panel background rectangle
#'
#' @description Create a annotated rectangle in the panel background.
#'
#' Useful for easily showing periods of uncertainty, different phases, or other regions of interest.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param xmin A value of length 1.
#' @param xmax A value of length 1.
#' @param ymin A value of length 1.
#' @param ymax A value of length 1.
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
#' p <- palmerpenguins::penguins |>
#'   gg_blanket(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = species,
#'   )
#'
#' p +
#'   annotate_panel_background(
#'     xmin = 225,
#'   ) +
#'   geom_point()
#'
#' p +
#'   annotate_panel_background(
#'     xmin = I(0.9),
#'   ) +
#'   geom_point()
#'
annotate_panel_background <- function(
    ...,
    xmin = -Inf,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf,
    fill = "#8991A1FF",
    alpha = 0.25,
    colour = "transparent",
    linewidth = 0,
    linetype = 1
) {

  # Create rectangle annotation
  stamp <- list(
    ggplot2::annotate(
      geom = "rect",
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = fill,
      colour = colour,
      linewidth = linewidth,
      linetype = linetype,
      alpha = alpha
    )
  )

  return(stamp)
}


