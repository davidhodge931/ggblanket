#' Viridis Colours with Adaptive Direction
#'
#' Returns a vector of viridis colours with automatic direction adjustment
#' depending on the panel background. This is the vector form that directly
#' returns colours rather than a palette function.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param option A character string indicating the palette option to use.
#' @param n The number of colours to return.
#' @param begin The (0–1) value at which to begin the colour scale. Default is 0.
#' @param end The (0–1) value at which to end the colour scale. Default is 1.
#' @param rev Logical. If `TRUE`, reverses the behaviour of the direction
#'   adjustment based on panel background. Default is `FALSE`.
#'
#' @returns A character vector of `n` hex colour codes.
#' @export
#'
#' @examples
#' viridis_by_panel(option = "magma", n = 5, begin = 0.1, end = 0.9)
#' viridis_by_panel(option = "plasma", n = 3)
viridis_by_panel <- function(
  n,
  option = "viridis",
  begin = 0,
  end = 1,
  ...,
  rev = FALSE
) {
  direction <- if (!rev) {
    ifelse(is_panel_light(), -1, 1)
  } else {
    ifelse(is_panel_light(), 1, -1)
  }

  scales::viridis_pal(
    option = option,
    begin = begin,
    end = end,
    direction = direction
  )(n)
}

#' Palette Function for viridis with Adaptive Direction
#'
#' Returns a palette function (for use in scales) based on the viridis colour palette,
#' with automatic direction adjustment depending on the panel background. This
#' version is compatible with `scales::scale_*` functions and is useful in ggplot2.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param option A character string indicating the palette option to use.
#' @param begin The (0–1) value at which to begin the colour scale. Default is 0.
#' @param end The (0–1) value at which to end the colour scale. Default is 1.
#' @param rev Logical. If `TRUE`, reverses the behaviour of the direction
#'   adjustment based on panel background. Default is `FALSE`.
#'
#' @returns A function that takes a single argument `n` and returns a vector
#'   of `n` colours.
#' @export
#'
#' @examples
#' pal <- pal_viridis_by_panel(option = "magma", begin = 0.1, end = 0.9)(5)
pal_viridis_by_panel <- function(
  ...,
  option = "viridis",
  begin = 0,
  end = 1,
  rev = FALSE
) {
  if (!rev) {
    scales::pal_viridis(
      option = option,
      begin = begin,
      end = end,
      direction = ifelse(is_panel_light(), -1, 1),
    )
  } else {
    scales::pal_viridis(
      option = option,
      begin = begin,
      end = end,
      direction = ifelse(is_panel_light(), 1, -1),
    )
  }
}
