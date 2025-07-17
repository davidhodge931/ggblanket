#' Check if theme panel background is dark
#'
#' @description
#' Determines whether the current ggplot2 theme has a dark or light panel background
#' by examining the luminance of the panel background colour.
#'
#' @param theme A ggplot2 theme object. If NULL (default), uses the current theme
#'        from `ggplot2::theme_get()`.
#'
#' @return Logical value: TRUE if panel background is dark (luminance <= 50),
#'         FALSE if light (luminance > 50).
#'
#' @export
#'
is_panel_dark <- function(theme = NULL) {
  # Get theme if not provided
  if (is.null(theme)) {
    theme <- ggplot2::theme_get()
  }

  # Get panel background colour from theme
  panel_colour <- theme$panel.background$fill %||% "white"

  # Use is_col_dark to check if the panel colour is dark
  is_col_dark(panel_colour)
}
