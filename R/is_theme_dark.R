#' Check if theme panel background is dark
#'
#' @description
#' Determines whether the current ggplot2 theme has a dark or light panel background
#' by examining the luminance of the panel background colour.
#'
#' @param theme A ggplot2 theme object. If NULL (default), uses the current theme
#'        from `ggplot2::theme_get()`.
#' @param element Theme element to use as basis for determining if theme is dark. Either "panel_background" (default) or "plot_background".
#'
#' @return TRUE if dark (luminance <= 50) and FALSE if light (luminance > 50).
#'
#' @export
#'
is_theme_dark <- function(theme = NULL, element = "panel_background") {
  # Get theme if not provided
  if (is.null(theme)) {
    theme <- ggplot2::theme_get()
  }

  # Get panel background colour from theme
  if (element == "panel_background") {
    col <- ggplot2::calc_element(theme = theme, element = "panel.background")$fill
  }
  else if (element == "plot_background") {
    col <- ggplot2::calc_element(theme = theme, element = "plot.background")$fill
  }

  # Use is_col_dark to check if the panel colour is dark
  is_col_dark(col)
}
