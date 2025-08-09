#' Screen blend colours
#'
#' @description
#' Blends two colours using screen mode, creating a lighter, more luminous effect.
#' Works with both colour vectors and palette functions. If only one colour is provided,
#' it blends the colour with itself. Screen is the inverse of multiply - it lightens
#' rather than darkens.
#'
#' @param col A character vector of colours or a `scales::pal_*()` function
#' @param col2 Optional second colour vector or palette function to blend with.
#'   If NULL (default), col is blended with itself.
#' @param ... Additional arguments (currently unused, reserved for future extensions)
#'
#' @return
#' If inputs are character vectors, returns a character vector of blended colours.
#' If either input is a function, returns a function that generates blended colours.
#'
#' @export
#' @examples
#' # Blend two colours
#' col_screen("#000080", "#800000")  # Dark blue + Dark red = Lighter purple
#'
#' # Screen a colour with itself (makes it lighter)
#' col_screen("#404040")  # Gray screened = Lighter gray
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
#'
#' # Works with transparency
#' col_screen("#00008080", "#80000080")  # 50% dark blue + 50% dark red
col_screen <- function(col, col2 = NULL, ...) {
  # If col2 is NULL, use col (self-screening)
  col2 <- col2 %||% col

  # Handle different input combinations
  if (is.function(col) || is.function(col2)) {
    # Return a function that blends the palette outputs
    function(n) {
      # Get colours from functions or use directly
      col_vctr <- if (is.function(col)) col(n) else rep_len(col, n)
      col2_vctr <- if (is.function(col2)) col2(n) else rep_len(col2, n)
      # Perform screen blend
      screen_blend(col_vctr, col2_vctr)
    }
  } else if (is.character(col) && is.character(col2)) {
    # Blend the colour vectors directly
    screen_blend(col, col2)
  } else {
    stop(
      "col and col2 must be either character vectors of colours or palette functions"
    )
  }
}

#' Internal screen blend function
#'
#' @description
#' Internal function that performs screen blending between two colours.
#' This mimics the "screen" blend mode from graphics software.
#' Formula: result = 1 - (1 - color1) * (1 - color2)
#' Supports alpha channel blending.
#'
#' @param col Character vector of colours
#' @param col2 Character vector of colours
#'
#' @return Character vector of blended colours
#'
#' @noRd
screen_blend <- function(col, col2) {
  # Ensure vectors have compatible lengths
  len <- max(length(col), length(col2))
  col <- rep_len(col, len)
  col2 <- rep_len(col2, len)

  # Convert colours to RGBA (values 0-1)
  rgba1 <- grDevices::col2rgb(col, alpha = TRUE) / 255
  rgba2 <- grDevices::col2rgb(col2, alpha = TRUE) / 255

  # Ensure we always have matrices (not vectors for single colors)
  if (!is.matrix(rgba1)) {
    rgba1 <- matrix(rgba1, nrow = 4, ncol = 1)
  }
  if (!is.matrix(rgba2)) {
    rgba2 <- matrix(rgba2, nrow = 4, ncol = 1)
  }

  # Apply screen blend mode to RGB channels: result = 1 - (1 - color1) * (1 - color2)
  blended_rgb <- 1 - ((1 - rgba1[1:3, , drop = FALSE]) * (1 - rgba2[1:3, , drop = FALSE]))

  # For alpha: use screen blend as well (makes it more opaque)
  # Alternative: could use multiply like in col_multiply for consistency
  blended_alpha <- 1 - ((1 - rgba1[4, ]) * (1 - rgba2[4, ]))

  # Convert back to hex colours with alpha
  grDevices::rgb(blended_rgb[1, ], blended_rgb[2, ], blended_rgb[3, ],
                 blended_alpha, maxColorValue = 1)
}
