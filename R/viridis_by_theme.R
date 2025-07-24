#' Generate a viridis Colour Palette with Adaptive Direction
#'
#' Returns a vector of colours from the viridis colour palette (from `viridisLite`),
#' with automatic direction adjustment depending on whether the panel background
#' is dark or light. This ensures the palette maintains appropriate contrast.
#'
#' @param n Number of colours to generate. Required.
#' @param begin The (0–1) value at which to begin the colour scale. Default is 0.
#' @param end The (0–1) value at which to end the colour scale. Default is 1.
#' @param option A character string indicating the palette option to use.
#' @param rev Logical. If `TRUE`, reverses the behaviour of the direction
#'   adjustment based on panel background. Default is `FALSE`.
#' @param theme A ggplot2 theme object. If NULL (default), uses the current theme
#'        from `ggplot2::theme_get()`.
#'
#' @return A character vector of colour values in hexadecimal format.
#' @export
#'
#' @examples
#' viridis_by_theme()
#' viridis_by_theme(n = 5, option = "A")
#' viridis_by_theme(n = 256, rev = TRUE)
viridis_by_theme <- function(
    n = 256,
    begin = 0,
    end = 1,
    option = "D",
    rev = FALSE,
    theme = NULL) {

  # Get theme if not provided
  if (is.null(theme)) {
    theme <- ggplot2::theme_get()
  }

  if (rev) {
    viridisLite::viridis(
      n = n,
      begin = begin,
      end = end,
      option = option,
      direction = ifelse(is_theme_dark(theme = theme), -1, 1),
    )
  }
  else {
    viridisLite::viridis(
      n = n,
      begin = begin,
      end = end,
      option = option,
      direction = ifelse(is_theme_dark(theme = theme), 1, -1),
    )
  }
}

#' Palette Function for viridis with Adaptive Direction
#'
#' Returns a palette function (for use in scales) based on the viridis colour palette,
#' with automatic direction adjustment depending on the panel background. This
#' version is compatible with `scales::scale_*` functions and is useful in ggplot2.
#'
#' @param begin The (0–1) value at which to begin the colour scale. Default is 0.
#' @param end The (0–1) value at which to end the colour scale. Default is 1.
#' @param option A character string indicating the palette option to use.
#' @param rev Logical. If `TRUE`, reverses the behaviour of the direction
#'   adjustment based on panel background. Default is `FALSE`.
#'
#' @returns A function that takes a single argument `n` and returns a vector
#'   of `n` colours.
#' @export
#'
#' @examples
#' pal <- pal_viridis_by_theme()(5)
pal_viridis_by_theme <- function(
    begin = 0,
    end = 1,
    option = "D",
    rev = FALSE) {

  if (rev) {
    scales::pal_viridis(
      begin = begin,
      end = end,
      option = option,
      direction = ifelse(is_theme_dark(), -1, 1),
    )
  }
  else {
    scales::pal_viridis(
      begin = begin,
      end = end,
      option = option,
      direction = ifelse(is_theme_dark(), 1, -1),
    )
  }
}
