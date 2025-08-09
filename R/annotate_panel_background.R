#' Annotate panel background rectangle
#'
#' @description Create an annotated rectangle in the panel background.
#'
#' This function is designed to work with a theme that is globally set, so that the annotated rectangle can be made consistent by default.
#'
#' Useful for easily showing periods of uncertainty, different phases, or other regions of interest.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param xmin A value of length 1. Defaults to -Inf.
#' @param xmax A value of length 1. Defaults to Inf.
#' @param ymin A value of length 1. Defaults to -Inf.
#' @param ymax A value of length 1. Defaults to Inf.
#' @param fill The fill colour of the rectangle. Inherits from current theme panel.background if NULL.
#' @param alpha The transparency of the rectangle. Defaults to 0.2.
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
    fill = NULL,
    alpha = 0.2,
    colour = NULL,
    linewidth = NULL,
    linetype = NULL
) {
  # Get current theme
  current_theme <- ggplot2::theme_get()

  # Try to inherit from panel.background if fill not specified
  if (is.null(fill)) {
    panel_bg <- ggplot2::calc_element("panel.background", current_theme, skip_blank = TRUE)
    if (!is.null(panel_bg) && !inherits(panel_bg, "element_blank")) {
      # Use a contrasting color if panel background exists
      # If panel is light, use darker shade; if dark, use lighter shade
      bg_fill <- panel_bg$fill %||% "white"
      # Simple approach: use a grey that contrasts
      if (bg_fill == "white" || bg_fill == "#FFFFFF") {
        fill <- "grey80"
      } else if (bg_fill == "black" || bg_fill == "#000000") {
        fill <- "grey20"
      } else {
        # For other colors, just use a neutral grey
        fill <- "grey70"
      }
    } else {
      # Default if no panel background
      fill <- "grey80"
    }
  }

  # Set defaults for other properties
  colour <- colour %||% "transparent"
  linewidth <- linewidth %||% 0
  linetype <- linetype %||% 1

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
