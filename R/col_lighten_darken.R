#' Lighten blend colours with proper alpha compositing
#'
#' @description
#' Blends two colours using lighten mode with proper alpha handling, selecting
#' the lighter of the color values in each component. Implements the exact
#' lighten blend formula used by Cairo graphics engine. This should match the
#' behavior of R's graphics device compositing operators.
#'
#' Formula from Cairo documentation:
#' `aR = aA + aB * (1 - aA)`
#' `xR = (1/aR) * [(1 - aB) * xaA + (1 - aA) * xaB + aA * aB * f(xA, xB)]`
#' where `f(xA, xB) = max(xA, xB)` for lighten blend mode
#'
#' @param col A character vector of colours or a `scales::pal_*()` function
#' @param col2 Optional second colour vector or palette function to blend with.
#'   If NULL (default), col is blended with itself.
#' @param ... Require named arguments (and support trailing commas).
#'
#' @return
#' If inputs are character vectors, returns a character vector of blended colours.
#' If either input is a function, returns a function that generates blended colours.
#'
#' @export
#' @examples
#' # Blend two colours - takes the lighter component of each
#' col_lighten("#FF0000", "#00FF00")  # Red vs Green = Yellow
#' col_lighten("#404040", "#808080")  # Gray vs lighter gray = lighter gray
#'
#' # Lighten a colour with itself (no change)
#' col_lighten("#FF6600")  # Orange lightened with itself = same orange
#'
#' # Work with vectors
#' col_lighten(c("#FF0000", "#00FF00"), c("#0000FF", "#FF00FF"))
#'
#' # Works with transparency
#' col_lighten("#FF000080", "#0000FF80")  # Semi-transparent colors
col_lighten <- function(col, col2 = NULL, ...) {
  # Check for unnamed arguments
  dots <- list(...)
  if (length(dots) > 0) {
    if (any(names(dots) == "" | rlang::is_null(names(dots)))) {
      stop("All arguments must be named. Use col_lighten(col = ..., col2 = ..., ...)")
    }
    warning("Additional named arguments are ignored: ", paste(names(dots), collapse = ", "))
  }

  # Input validation
  if (missing(col)) stop("col argument is required")
  if (length(col) == 0) stop("col cannot be empty")

  # Handle NULL col2 - use col for self-lightening
  if (rlang::is_null(col2)) col2 <- col

  # Handle different input combinations
  if (is.function(col) || is.function(col2)) {
    # Return a function that blends the palette outputs
    function(n) {
      # Get colours from functions or use directly
      col_vctr <- if (is.function(col)) col(n) else rep_len(col, n)
      col2_vctr <- if (is.function(col2)) col2(n) else rep_len(col2, n)
      # Perform lighten blend
      lighten_blend(col_vctr, col2_vctr)
    }
  } else if (is.character(col) && is.character(col2)) {
    # Blend the colour vectors directly
    lighten_blend(col, col2)
  } else {
    stop(
      "col and col2 must be either character vectors of colours or palette functions"
    )
  }
}

#' Darken blend colours with proper alpha compositing
#'
#' @description
#' Blends two colours using darken mode with proper alpha handling, selecting
#' the darker of the color values in each component. Implements the exact
#' darken blend formula used by Cairo graphics engine. This should match the
#' behavior of R's graphics device compositing operators.
#'
#' Formula from Cairo documentation:
#' `aR = aA + aB * (1 - aA)`
#' `xR = (1/aR) * [(1 - aB) * xaA + (1 - aA) * xaB + aA * aB * f(xA, xB)]`
#' where `f(xA, xB) = min(xA, xB)` for darken blend mode
#'
#' @param col A character vector of colours or a `scales::pal_*()` function
#' @param col2 Optional second colour vector or palette function to blend with.
#'   If NULL (default), col is blended with itself.
#' @param ... Require named arguments (and support trailing commas).
#'
#' @return
#' If inputs are character vectors, returns a character vector of blended colours.
#' If either input is a function, returns a function that generates blended colours.
#'
#' @export
#' @examples
#' # Blend two colours - takes the darker component of each
#' col_darken("#FF00FF", "#FFFF00")  # Magenta vs Yellow = Red
#' col_darken("#808080", "#404040")  # Gray vs darker gray = darker gray
#'
#' # Darken a colour with itself (no change)
#' col_darken("#FF6600")  # Orange darkened with itself = same orange
#'
#' # Work with vectors
#' col_darken(c("#FF0000", "#00FF00"), c("#0000FF", "#FF00FF"))
#'
#' # Works with transparency
#' col_darken("#FF000080", "#0000FF80")  # Semi-transparent colors
col_darken <- function(col, col2 = NULL, ...) {
  # Check for unnamed arguments
  dots <- list(...)
  if (length(dots) > 0) {
    if (any(names(dots) == "" | rlang::is_null(names(dots)))) {
      stop("All arguments must be named. Use col_darken(col = ..., col2 = ..., ...)")
    }
    warning("Additional named arguments are ignored: ", paste(names(dots), collapse = ", "))
  }

  # Input validation
  if (missing(col)) stop("col argument is required")
  if (length(col) == 0) stop("col cannot be empty")

  # Handle NULL col2 - use col for self-darkening
  if (rlang::is_null(col2)) col2 <- col

  # Handle different input combinations
  if (is.function(col) || is.function(col2)) {
    # Return a function that blends the palette outputs
    function(n) {
      # Get colours from functions or use directly
      col_vctr <- if (is.function(col)) col(n) else rep_len(col, n)
      col2_vctr <- if (is.function(col2)) col2(n) else rep_len(col2, n)
      # Perform darken blend
      darken_blend(col_vctr, col2_vctr)
    }
  } else if (is.character(col) && is.character(col2)) {
    # Blend the colour vectors directly
    darken_blend(col, col2)
  } else {
    stop(
      "col and col2 must be either character vectors of colours or palette functions"
    )
  }
}

#' Internal lighten blend function
#'
#' @description
#' Internal function that performs lighten blending using the Cairo formula.
#' Handles alpha transparency properly using Porter-Duff compositing.
#'
#' @param col Character vector of colours (source)
#' @param col2 Character vector of colours (destination)
#'
#' @return Character vector of blended colours
#'
#' @noRd
lighten_blend <- function(col, col2) {
  # Input validation
  if (any(is.na(col)) || any(is.na(col2))) {
    warning("NA values in color inputs may produce unexpected results")
  }

  # Ensure vectors have compatible lengths
  len <- max(length(col), length(col2))
  col <- rep_len(col, len)
  col2 <- rep_len(col2, len)

  # Convert to RGBA matrices (values 0-1)
  # col = source (A), col2 = destination (B)
  rgba_A <- matrix(grDevices::col2rgb(col, alpha = TRUE) / 255, nrow = 4)
  rgba_B <- matrix(grDevices::col2rgb(col2, alpha = TRUE) / 255, nrow = 4)

  # Extract alpha values
  aA <- rgba_A[4, ]
  aB <- rgba_B[4, ]

  # Calculate result alpha using Cairo formula: aR = aA + aB * (1 - aA)
  aR <- aA + aB * (1 - aA)

  # Handle the case where aR = 0 (full transparency)
  # In this case, color doesn't matter, set to 0
  aR_safe <- ifelse(aR == 0, 1, aR)  # Avoid division by zero

  # Calculate result colors for each channel (R, G, B)
  result_rgb <- matrix(0, nrow = 3, ncol = length(aA))

  for (i in 1:3) {
    # Extract color values for this channel
    xA <- rgba_A[i, ]
    xB <- rgba_B[i, ]

    # Calculate premultiplied alpha values
    xaA <- xA * aA
    xaB <- xB * aB

    # Apply Cairo lighten blend formula:
    # xR = (1/aR) * [(1 - aB) * xaA + (1 - aA) * xaB + aA * aB * f(xA, xB)]
    # where f(xA, xB) = max(xA, xB) for lighten

    blend_function <- pmax(xA, xB)  # f(xA, xB) for lighten

    xR <- (1 / aR_safe) * (
      (1 - aB) * xaA +           # Contribution of source where dest is transparent
        (1 - aA) * xaB +           # Contribution of dest where source is transparent
        aA * aB * blend_function   # Actual blend where both are opaque
    )

    # Set color to 0 where alpha is 0 (full transparency)
    xR <- ifelse(aR == 0, 0, xR)

    # Clamp to [0, 1] range
    result_rgb[i, ] <- pmax(0, pmin(1, xR))
  }

  # Convert back to hex colors
  grDevices::rgb(result_rgb[1, ], result_rgb[2, ], result_rgb[3, ], aR,
                 maxColorValue = 1)
}

#' Internal darken blend function
#'
#' @description
#' Internal function that performs darken blending using the Cairo formula.
#' Handles alpha transparency properly using Porter-Duff compositing.
#'
#' @param col Character vector of colours (source)
#' @param col2 Character vector of colours (destination)
#'
#' @return Character vector of blended colours
#'
#' @noRd
darken_blend <- function(col, col2) {
  # Input validation
  if (any(is.na(col)) || any(is.na(col2))) {
    warning("NA values in color inputs may produce unexpected results")
  }

  # Ensure vectors have compatible lengths
  len <- max(length(col), length(col2))
  col <- rep_len(col, len)
  col2 <- rep_len(col2, len)

  # Convert to RGBA matrices (values 0-1)
  # col = source (A), col2 = destination (B)
  rgba_A <- matrix(grDevices::col2rgb(col, alpha = TRUE) / 255, nrow = 4)
  rgba_B <- matrix(grDevices::col2rgb(col2, alpha = TRUE) / 255, nrow = 4)

  # Extract alpha values
  aA <- rgba_A[4, ]
  aB <- rgba_B[4, ]

  # Calculate result alpha using Cairo formula: aR = aA + aB * (1 - aA)
  aR <- aA + aB * (1 - aA)

  # Handle the case where aR = 0 (full transparency)
  # In this case, color doesn't matter, set to 0
  aR_safe <- ifelse(aR == 0, 1, aR)  # Avoid division by zero

  # Calculate result colors for each channel (R, G, B)
  result_rgb <- matrix(0, nrow = 3, ncol = length(aA))

  for (i in 1:3) {
    # Extract color values for this channel
    xA <- rgba_A[i, ]
    xB <- rgba_B[i, ]

    # Calculate premultiplied alpha values
    xaA <- xA * aA
    xaB <- xB * aB

    # Apply Cairo darken blend formula:
    # xR = (1/aR) * [(1 - aB) * xaA + (1 - aA) * xaB + aA * aB * f(xA, xB)]
    # where f(xA, xB) = min(xA, xB) for darken

    blend_function <- pmin(xA, xB)  # f(xA, xB) for darken

    xR <- (1 / aR_safe) * (
      (1 - aB) * xaA +           # Contribution of source where dest is transparent
        (1 - aA) * xaB +           # Contribution of dest where source is transparent
        aA * aB * blend_function   # Actual blend where both are opaque
    )

    # Set color to 0 where alpha is 0 (full transparency)
    xR <- ifelse(aR == 0, 0, xR)

    # Clamp to [0, 1] range
    result_rgb[i, ] <- pmax(0, pmin(1, xR))
  }

  # Convert back to hex colors
  grDevices::rgb(result_rgb[1, ], result_rgb[2, ], result_rgb[3, ], aR,
                 maxColorValue = 1)
}
