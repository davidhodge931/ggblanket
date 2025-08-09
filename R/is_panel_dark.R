#' Check if theme panel background is dark
#'
#' @description
#' Determines whether the current ggplot2 theme has a dark or light panel background
#' by examining its luminance.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param theme A ggplot2 theme object. If NULL (default), uses the current theme
#'        from `ggplot2::theme_get()`.
#'
#' @return TRUE if dark (luminance <= 50) and FALSE otherwise.
#'
#' @export
is_panel_dark <- function(..., theme = NULL) {
  # Get theme if not provided
  if (rlang::is_null(theme)) {
    theme <- ggplot2::theme_get()
  }

  # Get panel background colour from theme
  col <- ggplot2::calc_element(theme = theme, element = "panel.background")$fill

  # Use is_col_dark to check if the panel colour is dark
  is_col_dark(col)
}

#' Check if theme panel background is light
#'
#' @description
#' Determines whether the current ggplot2 theme has a light panel background
#' by examining its luminance.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param theme A ggplot2 theme object. If NULL (default), uses the current theme
#'        from `ggplot2::theme_get()`.
#'
#' @return TRUE if light (luminance > 50) and FALSE otherwise.
#'
#' @export
is_panel_light <- function(..., theme = NULL) {
  # Get theme if not provided
  if (rlang::is_null(theme)) {
    theme <- ggplot2::theme_get()
  }

  # Get panel background colour from theme
  col <- ggplot2::calc_element(theme = theme, element = "panel.background")$fill

  # Use is_col_light to check if the panel colour is light
  is_col_light(col)
}
