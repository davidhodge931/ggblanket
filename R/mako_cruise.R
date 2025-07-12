#' Generate a Mako Colour Palette with Adaptive Direction
#'
#' Returns a vector of colours from the Mako colour palette (from `viridisLite`),
#' with automatic direction adjustment depending on whether the panel background
#' is dark or light. This ensures the palette maintains appropriate contrast.
#'
#' @param n Number of colours to generate. Default is 20.
#' @param begin The (0–1) value at which to begin the colour scale. Default is 0.05.
#' @param end The (0–1) value at which to end the colour scale. Default is 0.95.
#' @param option A character string indicating the palette option to use.
#'   `"G"` refers to the Mako palette.
#' @param rev Logical. If `TRUE`, reverses the behaviour of the direction
#'   adjustment based on panel background. Default is `FALSE`.
#'
#' @return A character vector of colour values in hexadecimal format.
#' @export
#'
#' @examples
#' mako_cruise()
#' mako_cruise(n = 5)
#' mako_cruise(rev = TRUE)
mako_cruise <- function(
    n = 20,
    begin = 0.05,
    end = 0.95,
    option = "G",
    rev = FALSE) {

  if (!rev) {
    viridisLite::viridis(
      n = n,
      begin = begin,
      end = end,
      option = option,
      direction = ifelse(is_panel_background_dark(), -1, 1),
    )
  }
  else {
    viridisLite::viridis(
      n = n,
      begin = begin,
      end = end,
      option = option,
      direction = ifelse(is_panel_background_dark(), 1, -1),
    )
  }
}

#' Palette Function for Mako with Adaptive Direction
#'
#' Returns a palette function (for use in scales) based on the Mako colour palette,
#' with automatic direction adjustment depending on the panel background. This
#' version is compatible with `scales::scale_*` functions and is useful in ggplot2.
#'
#' @param begin The (0–1) value at which to begin the colour scale. Default is 0.05.
#' @param end The (0–1) value at which to end the colour scale. Default is 0.95.
#' @param option A character string indicating the palette option to use.
#'   `"G"` refers to the Mako palette.
#' @param rev Logical. If `TRUE`, reverses the behaviour of the direction
#'   adjustment based on panel background. Default is `FALSE`.
#'
#' @returns A function that takes a single argument `n` and returns a vector
#'   of `n` colours.
#' @export
#'
#' @examples
#' pal <- pal_mako_cruise()(5)
pal_mako_cruise <- function(
  begin = 0.05,
  end = 0.95,
  option = "G",
  rev = FALSE) {

    if (!rev) {
      scales::pal_viridis(
        begin = begin,
        end = end,
        option = option,
        direction = ifelse(is_panel_background_dark(), -1, 1),
      )
    }
    else {
      scales::pal_viridis(
        begin = begin,
        end = end,
        option = option,
        direction = ifelse(is_panel_background_dark(), 1, -1),
      )
    }
  }
