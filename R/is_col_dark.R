#' Check if a colour is dark
#'
#' @description
#' Determines whether a colour is dark or light by examining its luminance value.
#'
#' @param col A colour value. Can be a hex code, colour name, or any format
#'        accepted by farver. If NULL, returns FALSE.
#'
#' @return Logical value: TRUE if colour is dark (luminance <= 50),
#'         FALSE if light (luminance > 50) or if col is NULL.
#'
#' @export
#'
#' @examples
#' is_col_dark("#000000")  # TRUE (black)
#' is_col_dark("#FFFFFF")  # FALSE (white)
#' is_col_dark("darkblue") # TRUE
#' is_col_dark("#121B24FF") # TRUE (dark blue-gray from theme_lighter)
#' is_col_dark("#C8D7DFFF") # FALSE (light blue-gray from theme_darker)
#'
is_col_dark <- function(col) {
  # Handle NULL or missing input
  if (is.null(col) || length(col) == 0) {
    return(FALSE)
  }

  # Calculate luminance of the colour
  col_luminance <- farver::get_channel(
    colour = col,
    channel = "l",
    space = "hcl"
  )

  # Return TRUE if dark (low luminance), FALSE if light
  col_luminance <= 50
}
