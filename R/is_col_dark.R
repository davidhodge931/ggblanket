#' Check if a colour is dark
#'
#' @description
#' Determines whether a colour is dark by examining its luminance value.
#'
#' @param col A colour value. Can be a hex code, colour name, or any format
#'        accepted by farver. If NULL, returns FALSE.
#'
#' @return TRUE if dark (luminance <= 50) and FALSE otherwise.
#'
#' @export
#'
#' @examples
#' is_col_dark("#0095A8FF")
#'
is_col_dark <- function(col) {
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

  # print(col_luminance)

  # Return TRUE if dark (low luminance), FALSE if light
  col_luminance <= 50
}

#' Check if a colour is light
#'
#' @description
#' Determines whether a colour is light by examining its luminance value.

#' @param col A colour value. Can be a hex code, colour name, or any format
#'        accepted by farver. If NULL, returns FALSE.
#'
#' @return TRUE if light (luminance > 50) and FALSE otherwise.
#'
#' @examples
#' is_col_light("#0095A8FF")
#'
#' @export
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

  col_luminance > 50
}
