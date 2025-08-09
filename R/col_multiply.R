#' Multiply blend colours
#'
#' @description
#' Blends two colours using multiply mode, creating a darker, more saturated effect.
#' Works with both colour vectors and palette functions. If only one colour is provided,
#' it blends the colour with itself (squaring effect).
#'
#' @param col A character vector of colours or a `scales::pal_*()` function
#' @param ... Require named arguments (and support trailing commas).
#' @param col2 A character vector of colours or a `scales::pal_*()` function, or NULL.
#'   If NULL (default), col is blended with itself.
#'
#' @return
#' If inputs are character vectors, returns a character vector of blended colours.
#' If either input is a function, returns a function that generates blended colours.
#'
#' @export
#' @examples
#' # Blend two colours
#' col_multiply("#FF0000", "#0000FF")
#'
#' # Square a colour (blend with itself)
#' col_multiply("#FF6600")
#'
#' # Work with vectors
#' col_multiply(c("#FF0000", "#00FF00"), c("#0000FF", "#FF00FF"))
#'
#' # Work with palette functions
#' pal <- col_multiply(scales::pal_viridis(), scales::pal_brewer(palette = "Set1"))
#' pal(5)
#'
#' # Square a palette function
#' pal_squared <- col_multiply(scales::pal_viridis())
#' pal_squared(5)
col_multiply <- function(col, ..., col2 = NULL) {
  # If col2 is NULL, use col
  if (rlang::is_null(col2)) {
    col2 <- col
  }

  # Handle different input combinations
  if (is.function(col) || is.function(col2)) {
    # Return a function that blends the palette outputs
    function(n) {
      # Get colours from functions or use directly
      col_vctr <- if (is.function(col)) col(n) else rep_len(col, n)
      col2_vctr <- if (is.function(col2)) col2(n) else rep_len(col2, n)

      # Perform multiply blend
      multiply_blend(col_vctr, col2_vctr)
    }
  } else if (is.character(col) && is.character(col2)) {
    # Blend the colour vectors directly
    multiply_blend(col, col2)
  } else {
    stop(
      "col and col2 must be either character vectors of colours or palette functions"
    )
  }
}

#' Internal multiply blend function
#'
#' @description
#' Internal function that performs multiply blending between two colours.
#' This mimics the "multiply" blend mode from graphics software.
#'
#' @param col Character vector of colours
#' @param col2 Character vector of colours
#'
#' @return Character vector of blended colours
#'
#' @noRd
multiply_blend <- function(col, col2) {
  # Ensure vectors have compatible lengths
  len <- max(length(col), length(col2))
  col <- rep_len(col, len)
  col2 <- rep_len(col2, len)

  # Convert colours to RGB (values 0-1)
  rgb1 <- grDevices::col2rgb(col) / 255
  rgb2 <- grDevices::col2rgb(col2) / 255

  # Apply multiply blend mode: result = colour1 * colour2
  blended_rgb <- rgb1 * rgb2

  # Convert back to hex colours
  grDevices::rgb(blended_rgb[1, ], blended_rgb[2, ], blended_rgb[3, ])
}
