#' Square colours using multiply blend mode
#'
#' @description
#' Takes colours and blends them with themselves using multiply mode, creating
#' a darker, more saturated effect. Works with both colour vectors and palette
#' functions.
#'
#' @param col A character vector of colours or a `scales::pal_*()` function
#'
#' @return
#' If input is a character vector, returns a character vector of squared colours.
#' If input is a function, returns a function that generates squared colours.
#'
#' @export
col_squared <- function(col) {
  if (is.function(col)) {
    # Return a function that squares the palette output
    function(n) {
      col_vctr <- col(n)
      col_multiply(col_vctr, col_vctr)
    }
  } else if (is.character(col)) {
    # Square the colour vector directly
    col_multiply(col, col)
  } else {
    stop("col must be either a character vector of colours or a palette function")
  }
}

#' Multiply blend two colours
#'
#' @description
#' Internal function that performs multiply blending between two colours.
#' This mimics the "multiply" blend mode from graphics software.
#'
#' @param col1 Character vector of colours
#' @param col2 Character vector of colours (same length as col1)
#'
#' @return Character vector of blended colours
#'
#' @noRd
col_multiply <- function(col1, col2) {
  # Convert colours to RGB
  rgb1 <- grDevices::col2rgb(col1) / 255
  rgb2 <- grDevices::col2rgb(col2) / 255

  # Apply multiply blend mode: result = colour1 * colour2
  blended_rgb <- rgb1 * rgb2

  # Convert back to hex colours
  grDevices::rgb(blended_rgb[1,], blended_rgb[2,], blended_rgb[3,])
}

