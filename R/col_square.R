#' Square colors using multiply blend mode
#'
#' @description
#' Takes colors and blends them with themselves using multiply mode, creating
#' a darker, more saturated effect. Works with both color vectors and palette
#' functions.
#'
#' @param col A character vector of colors or a `scales::pal_*()` function
#'
#' @return
#' If input is a character vector, returns a character vector of squared colors.
#' If input is a function, returns a function that generates squared colors.
#'
#' @export
col_square <- function(col) {
  if (is.function(col)) {
    # Return a function that squares the palette output
    function(n) {
      col_vctr <- col(n)
      col_multiply(col_vctr, col_vctr)
    }
  } else if (is.character(col)) {
    # Square the color vector directly
    col_multiply(col, col)
  } else {
    stop("col must be either a character vector of colors or a palette function")
  }
}

#' Multiply blend two colors
#'
#' @description
#' Internal function that performs multiply blending between two colors.
#' This mimics the "multiply" blend mode from graphics software.
#'
#' @param col1 Character vector of colors
#' @param col2 Character vector of colors (same length as col1)
#'
#' @return Character vector of blended colors
#'
#' @noRd
col_multiply <- function(col1, col2) {
  # Convert colors to RGB
  rgb1 <- grDevices::col2rgb(col1) / 255
  rgb2 <- grDevices::col2rgb(col2) / 255

  # Apply multiply blend mode: result = color1 * color2
  blended_rgb <- rgb1 * rgb2

  # Convert back to hex colors
  grDevices::rgb(blended_rgb[1,], blended_rgb[2,], blended_rgb[3,])
}

