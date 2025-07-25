#' Screen blend colours
#'
#' @description
#' Blends two colours using screen mode, creating a lighter, more luminous effect.
#' Works with both colour vectors and palette functions. If only one colour is provided,
#' it blends the colour with itself. Screen is the inverse of multiply - it lightens
#' rather than darkens.
#'
#' @param col1 A character vector of colours or a `scales::pal_*()` function
#' @param col2 A character vector of colours or a `scales::pal_*()` function, or NULL.
#'   If NULL (default), col1 is blended with itself.
#'
#' @return
#' If inputs are character vectors, returns a character vector of blended colours.
#' If either input is a function, returns a function that generates blended colours.
#'
#' @export
#' @examples
#' # Blend two colours
#' col_screen("#FF0000", "#0000FF")  # Red + Blue = Magenta
#'
#' # Screen a colour with itself
#' col_screen("#FF6600")
#'
#' # Work with vectors
#' col_screen(c("#FF0000", "#00FF00"), c("#0000FF", "#FF00FF"))
#'
#' # Work with palette functions
#' pal <- col_screen(scales::pal_viridis(), scales::pal_brewer(palette = "Set1"))
#' pal(5)
#'
#' # Screen a palette function with itself
#' pal_screened <- col_screen(scales::pal_viridis())
#' pal_screened(5)
col_screen <- function(col1, col2 = NULL) {
  # If col2 is NULL, use col1
  if (is.null(col2)) {
    col2 <- col1
  }

  # Handle different input combinations
  if (is.function(col1) || is.function(col2)) {
    # Return a function that blends the palette outputs
    function(n) {
      # Get colours from functions or use directly
      col1_vctr <- if (is.function(col1)) col1(n) else rep_len(col1, n)
      col2_vctr <- if (is.function(col2)) col2(n) else rep_len(col2, n)

      # Perform screen blend
      screen_blend(col1_vctr, col2_vctr)
    }
  } else if (is.character(col1) && is.character(col2)) {
    # Blend the colour vectors directly
    screen_blend(col1, col2)
  } else {
    stop(
      "col1 and col2 must be either character vectors of colours or palette functions"
    )
  }
}

#' Internal screen blend function
#'
#' @description
#' Internal function that performs screen blending between two colours.
#' This mimics the "screen" blend mode from graphics software.
#' Formula: result = 1 - (1 - color1) * (1 - color2)
#'
#' @param col1 Character vector of colours
#' @param col2 Character vector of colours
#'
#' @return Character vector of blended colours
#'
#' @noRd
screen_blend <- function(col1, col2) {
  # Ensure vectors have compatible lengths
  len <- max(length(col1), length(col2))
  col1 <- rep_len(col1, len)
  col2 <- rep_len(col2, len)

  # Convert colours to RGB (values 0-1)
  rgb1 <- grDevices::col2rgb(col1) / 255
  rgb2 <- grDevices::col2rgb(col2) / 255

  # Apply screen blend mode: result = 1 - (1 - color1) * (1 - color2)
  blended_rgb <- 1 - ((1 - rgb1) * (1 - rgb2))

  # Convert back to hex colours
  grDevices::rgb(blended_rgb[1, ], blended_rgb[2, ], blended_rgb[3, ])
}
