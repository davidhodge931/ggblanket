#' Screen blend colours
#'
#' @description
#' Blends two colours using screen mode, creating a lighter, more luminous effect.
#' Works with both colour vectors and palette functions. If only one colour is provided,
#' it blends the colour with itself. Screen is the inverse of multiply - it lightens
#' rather than darkens.
#'
#' Note: This function currently only supports fully opaque colors. Colors with
#' alpha transparency will generate a warning and may produce unexpected results.
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
col_screen <- function(col, col2 = NULL, ...) {
  # Input validation
  if (missing(col)) stop("col argument is required")
  if (length(col) == 0) stop("col cannot be empty")

  # Warn about unused arguments
  if (length(list(...)) > 0) {
    warning("Additional arguments are ignored")
  }

  # Handle NULL col2 - use col for self-screening
  if (is.null(col2)) col2 <- col

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
#' This mimics the "screen" blend mode from graphics software for RGB channels.
#' Formula: result = 1 - (1 - color1) * (1 - color2)
#' Currently only supports fully opaque colors - alpha transparency will
#' generate a warning.
#'
#' @param col Character vector of colours
#' @param col2 Character vector of colours
#'
#' @return Character vector of blended colours (always fully opaque)
#'
#' @noRd
screen_blend <- function(col, col2) {
  # Input validation
  if (any(is.na(col)) || any(is.na(col2))) {
    warning("NA values in color inputs may produce unexpected results")
  }

  # Ensure vectors have compatible lengths
  len <- max(length(col), length(col2))
  col <- rep_len(col, len)
  col2 <- rep_len(col2, len)

  # Convert colours to RGBA matrices (values 0-1)
  rgba1 <- matrix(grDevices::col2rgb(col, alpha = TRUE) / 255, nrow = 4)
  rgba2 <- matrix(grDevices::col2rgb(col2, alpha = TRUE) / 255, nrow = 4)

  # Check for alpha transparency and warn
  has_alpha1 <- any(rgba1[4, ] < 1.0)
  has_alpha2 <- any(rgba2[4, ] < 1.0)

  if (has_alpha1 || has_alpha2) {
    warning("Colors with alpha transparency detected. This function currently only supports fully opaque colors and may produce unexpected results with transparency.")
  }

  # Apply screen blend mode to RGB channels: result = 1 - (1 - color1) * (1 - color2)
  blended_rgb <- 1 - ((1 - rgba1[1:3, , drop = FALSE]) * (1 - rgba2[1:3, , drop = FALSE]))

  # For opaque colors, set alpha to 1.0 (fully opaque)
  # This ensures consistent behavior regardless of input alpha values
  blended_alpha <- rep(1.0, ncol(rgba1))

  # Convert back to hex colours without alpha suffix
  grDevices::rgb(blended_rgb[1, ], blended_rgb[2, ], blended_rgb[3, ],
                 blended_alpha, maxColorValue = 1)
}
