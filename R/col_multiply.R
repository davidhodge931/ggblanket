#' #' Multiply blend colours
#' #'
#' #' @description
#' #' Blends two colours using multiply mode, creating a darker, more saturated effect.
#' #' Works with both colour vectors and palette functions. If only one colour is provided,
#' #' it blends the colour with itself (squaring effect).
#' #'
#' #' Note: This function currently only supports fully opaque colors. Colors with
#' #' alpha transparency will generate a warning and may produce unexpected results.
#' #'
#' #' @param col A character vector of colours or a `scales::pal_*()` function
#' #' @param col2 Optional second colour vector or palette function to blend with.
#' #'   If NULL (default), col is blended with itself (squared).
#' #' @param ... Additional arguments (currently unused, reserved for future extensions)
#' #'
#' #' @return
#' #' If inputs are character vectors, returns a character vector of blended colours.
#' #' If either input is a function, returns a function that generates blended colours.
#' #'
#' #' @export
#' #' @examples
#' #' # Blend two colours
#' #' col_multiply("#FF0000", "#0000FF")  # Red × Blue = Black
#' #'
#' #' # Square a colour (blend with itself)
#' #' col_multiply("#FF6600")  # Orange squared = Darker orange
#' #'
#' #' # Work with vectors
#' #' col_multiply(c("#FF0000", "#00FF00"), c("#0000FF", "#FF00FF"))
#' #'
#' #' # Work with palette functions
#' #' pal <- col_multiply(scales::pal_viridis(), scales::pal_brewer(palette = "Set1"))
#' #' pal(5)
#' #'
#' #' # Square a palette function
#' #' pal_squared <- col_multiply(scales::pal_viridis())
#' #' pal_squared(5)
#' col_multiply <- function(col, col2 = NULL, ...) {
#'   # Input validation
#'   if (missing(col)) stop("col argument is required")
#'   if (length(col) == 0) stop("col cannot be empty")
#'
#'   # Warn about unused arguments
#'   if (length(list(...)) > 0) {
#'     warning("Additional arguments are ignored")
#'   }
#'
#'   # Handle NULL col2 - use col for self-multiplication
#'   if (is.null(col2)) col2 <- col
#'
#'   # Handle different input combinations
#'   if (is.function(col) || is.function(col2)) {
#'     # Return a function that blends the palette outputs
#'     function(n) {
#'       # Get colours from functions or use directly
#'       col_vctr <- if (is.function(col)) col(n) else rep_len(col, n)
#'       col2_vctr <- if (is.function(col2)) col2(n) else rep_len(col2, n)
#'       # Perform multiply blend
#'       multiply_blend(col_vctr, col2_vctr)
#'     }
#'   } else if (is.character(col) && is.character(col2)) {
#'     # Blend the colour vectors directly
#'     multiply_blend(col, col2)
#'   } else {
#'     stop(
#'       "col and col2 must be either character vectors of colours or palette functions"
#'     )
#'   }
#' }
#'
#' #' Internal multiply blend function
#' #'
#' #' @description
#' #' Internal function that performs multiply blending between two colours.
#' #' This mimics the "multiply" blend mode from graphics software for RGB channels.
#' #' Currently only supports fully opaque colors - alpha transparency will
#' #' generate a warning.
#' #'
#' #' @param col Character vector of colours
#' #' @param col2 Character vector of colours
#' #'
#' #' @return Character vector of blended colours (always fully opaque)
#' #'
#' #' @noRd
#' multiply_blend <- function(col, col2) {
#'   # Input validation
#'   if (any(is.na(col)) || any(is.na(col2))) {
#'     warning("NA values in color inputs may produce unexpected results")
#'   }
#'
#'   # Ensure vectors have compatible lengths
#'   len <- max(length(col), length(col2))
#'   col <- rep_len(col, len)
#'   col2 <- rep_len(col2, len)
#'
#'   # Convert colours to RGBA matrices (values 0-1)
#'   rgba1 <- matrix(grDevices::col2rgb(col, alpha = TRUE) / 255, nrow = 4)
#'   rgba2 <- matrix(grDevices::col2rgb(col2, alpha = TRUE) / 255, nrow = 4)
#'
#'   # Check for alpha transparency and warn
#'   has_alpha1 <- any(rgba1[4, ] < 1.0)
#'   has_alpha2 <- any(rgba2[4, ] < 1.0)
#'
#'   if (has_alpha1 || has_alpha2) {
#'     warning("Colors with alpha transparency detected. This function currently only supports fully opaque colors and may produce unexpected results with transparency.")
#'   }
#'
#'   # Apply multiply blend mode to RGB channels: result = colour1 * colour2
#'   blended_rgb <- rgba1[1:3, , drop = FALSE] * rgba2[1:3, , drop = FALSE]
#'
#'   # For opaque colors, set alpha to 1.0 (fully opaque)
#'   # This ensures consistent behavior regardless of input alpha values
#'   blended_alpha <- rep(1.0, ncol(rgba1))
#'
#'   # Convert back to hex colours without alpha suffix
#'   grDevices::rgb(blended_rgb[1, ], blended_rgb[2, ], blended_rgb[3, ],
#'                  blended_alpha, maxColorValue = 1)
#' }
