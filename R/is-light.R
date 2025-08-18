#' Check if a colour is light
#'
#' @description
#' Determines whether a colour is light by examining its luminance value.
#'
#' @param col A colour value. Can be a hex code, colour name, or any format
#'        accepted by farver. If NULL, returns FALSE.
#'
#' @return TRUE if light (luminance > 50) and FALSE otherwise.
#'
#' @export
#'
#' @examples
#' is_col_light("#0095A8FF")
#'
is_col_light <- function(col) {
  # Handle NULL or missing input
  if (rlang::is_null(col) || length(col) == 0) {
    return(FALSE)
  }

  # Calculate luminance of the colour
  col_luminance <- farver::get_channel(
    colour = col,
    channel = "l",
    space = "hcl"
  )

  # Return TRUE if high luminance
  col_luminance > 50
}

#' Check if theme panel background is light
#'
#' @description
#' Determines whether the current ggplot2 theme has a light or light panel background
#' by examining its luminance.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param theme A ggplot2 theme object. If NULL (default), uses the current theme
#'        from `ggplot2::theme_get()`.
#'
#' @return TRUE if light (luminance <= 50) and FALSE otherwise.
#'
#' @export
is_panel_light <- function(..., theme = NULL) {
  # Get theme if not provided
  if (rlang::is_null(theme)) {
    theme <- ggplot2::theme_get()
  }

  # Get panel background colour from theme
  col <- ggplot2::calc_element(theme = theme, element = "panel.background")$fill

  # Use .is_col_light to check if the panel colour is light
  is_col_light(col)
}
