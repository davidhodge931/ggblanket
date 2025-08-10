#' Multiply blend colours with proper alpha compositing
#'
#' @description
#' Blends colours using multiply mode with proper alpha handling, creating
#' a darker, more saturated effect. Implements the exact multiply blend formula
#' used by Cairo graphics engine. This should match the behavior of R's
#' graphics device compositing operators.
#'
#' Formula from Cairo documentation:
#' `aR = aA + aB * (1 - aA)`
#' `xR = (1/aR) * [(1 - aB) * xaA + (1 - aA) * xaB + aA * aB * f(xA, xB)]`
#' where `f(xA, xB) = xA * xB` for multiply blend mode
#'
#' @param ... Either one or two colour arguments:
#'   - If one argument: the colour is blended with itself (squared)
#'   - If two arguments: the first is blended with the second
#'   Each argument can be a character vector of colours or a `scales::pal_*()` function
#'
#' @return
#' If inputs are character vectors, returns a character vector of blended colours.
#' If any input is a function, returns a function that generates blended colours.
#'
#' @export
#' @examples
#' # Blend two colours
#' blend_multiply("#FF0000", "#0000FF")  # Red × Blue = Black
#'
#' # Square a colour (blend with itself)
#' blend_multiply("#FF6600")  # Orange squared = Darker orange
#'
#' # Work with vectors
#' blend_multiply(c("#FF0000", "#00FF00"), c("#0000FF", "#FF00FF"))
#'
#' # Works with transparency
#' blend_multiply("#FF000080", "#0000FF80")  # Semi-transparent colors
#'
#' # Works with palette functions
#' pal <- blend_multiply(scales::pal_viridis(), scales::pal_brewer("Set1"))
#' pal(5)  # Generate 5 blended colours
blend_multiply <- function(...) {
  dots <- list(...)

  # Validate number of arguments
  if (length(dots) == 0) {
    stop("At least one colour argument is required")
  } else if (length(dots) == 1) {
    # Self-multiplication
    col <- dots[[1]]
    col2 <- col
  } else if (length(dots) == 2) {
    # Blend two colours
    col <- dots[[1]]
    col2 <- dots[[2]]
  } else {
    stop("blend_multiply accepts at most 2 colour arguments, got ", length(dots))
  }

  # Validate inputs are not NULL
  if (is.null(col) || is.null(col2)) {
    stop("Colour arguments cannot be NULL")
  }

  # Handle different input combinations
  if (is.function(col) || is.function(col2)) {
    # Return a function that blends the palette outputs
    function(n) {
      # Get colours from functions or use directly
      col_vctr <- if (is.function(col)) col(n) else rep_len(col, n)
      col2_vctr <- if (is.function(col2)) col2(n) else rep_len(col2, n)
      # Perform multiply blend
      .blend_multiply(col_vctr, col2_vctr)
    }
  } else if (is.character(col) && is.character(col2)) {
    # Blend the colour vectors directly
    .blend_multiply(col, col2)
  } else {
    stop("Arguments must be either character vectors of colours or palette functions")
  }
}

#' Screen blend colours with proper alpha compositing
#'
#' @description
#' Blends colours using screen mode with proper alpha handling, creating
#' a lighter, more luminous effect. Implements the exact screen blend formula
#' used by Cairo graphics engine. This should match the behavior of R's
#' graphics device compositing operators.
#'
#' Formula from Cairo documentation:
#' `aR = aA + aB * (1 - aA)`
#' `xR = (1/aR) * [(1 - aB) * xaA + (1 - aA) * xaB + aA * aB * f(xA, xB)]`
#' where `f(xA, xB) = xA + xB - xA * xB` for screen blend mode
#'
#' @param ... Either one or two colour arguments:
#'   - If one argument: the colour is blended with itself
#'   - If two arguments: the first is blended with the second
#'   Each argument can be a character vector of colours or a `scales::pal_*()` function
#'
#' @return
#' If inputs are character vectors, returns a character vector of blended colours.
#' If any input is a function, returns a function that generates blended colours.
#'
#' @export
#' @examples
#' # Blend two colours
#' blend_screen("#000080", "#800000")  # Dark blue + Dark red = Lighter purple
#'
#' # Screen a colour with itself (makes it lighter)
#' blend_screen("#404040")  # Gray screened = Lighter gray
#'
#' # Work with vectors
#' blend_screen(c("#FF0000", "#00FF00"), c("#0000FF", "#FF00FF"))
#'
#' # Works with transparency
#' blend_screen("#80000080", "#00008080")  # Semi-transparent colors
blend_screen <- function(...) {
  dots <- list(...)

  # Validate number of arguments
  if (length(dots) == 0) {
    stop("At least one colour argument is required")
  } else if (length(dots) == 1) {
    # Self-screening
    col <- dots[[1]]
    col2 <- col
  } else if (length(dots) == 2) {
    # Blend two colours
    col <- dots[[1]]
    col2 <- dots[[2]]
  } else {
    stop("blend_screen accepts at most 2 colour arguments, got ", length(dots))
  }

  # Validate inputs are not NULL
  if (is.null(col) || is.null(col2)) {
    stop("Colour arguments cannot be NULL")
  }

  # Handle different input combinations
  if (is.function(col) || is.function(col2)) {
    # Return a function that blends the palette outputs
    function(n) {
      # Get colours from functions or use directly
      col_vctr <- if (is.function(col)) col(n) else rep_len(col, n)
      col2_vctr <- if (is.function(col2)) col2(n) else rep_len(col2, n)
      # Perform screen blend
      .blend_screen(col_vctr, col2_vctr)
    }
  } else if (is.character(col) && is.character(col2)) {
    # Blend the colour vectors directly
    .blend_screen(col, col2)
  } else {
    stop("Arguments must be either character vectors of colours or palette functions")
  }
}

#' Lighten blend colours with proper alpha compositing
#'
#' @description
#' Blends colours using lighten mode with proper alpha handling, selecting
#' the lighter of the color values in each component. Implements the exact
#' lighten blend formula used by Cairo graphics engine.
#'
#' Formula from Cairo documentation:
#' `aR = aA + aB * (1 - aA)`
#' `xR = (1/aR) * [(1 - aB) * xaA + (1 - aA) * xaB + aA * aB * f(xA, xB)]`
#' where `f(xA, xB) = max(xA, xB)` for lighten blend mode
#'
#' @param ... Either one or two colour arguments:
#'   - If one argument: the colour is blended with itself (no change)
#'   - If two arguments: the first is blended with the second
#'   Each argument can be a character vector of colours or a `scales::pal_*()` function
#'
#' @return
#' If inputs are character vectors, returns a character vector of blended colours.
#' If any input is a function, returns a function that generates blended colours.
#'
#' @export
#' @examples
#' # Blend two colours - takes the lighter component of each
#' blend_lighten("#FF0000", "#00FF00")  # Red vs Green = Yellow
#' blend_lighten("#404040", "#808080")  # Gray vs lighter gray = lighter gray
#'
#' # Lighten a colour with itself (no change)
#' blend_lighten("#FF6600")  # Orange lightened with itself = same orange
#'
#' # Work with vectors
#' blend_lighten(c("#FF0000", "#00FF00"), c("#0000FF", "#FF00FF"))
blend_lighten <- function(...) {
  dots <- list(...)

  # Validate number of arguments
  if (length(dots) == 0) {
    stop("At least one colour argument is required")
  } else if (length(dots) == 1) {
    # Self-lightening
    col <- dots[[1]]
    col2 <- col
  } else if (length(dots) == 2) {
    # Blend two colours
    col <- dots[[1]]
    col2 <- dots[[2]]
  } else {
    stop("blend_lighten accepts at most 2 colour arguments, got ", length(dots))
  }

  # Validate inputs are not NULL
  if (is.null(col) || is.null(col2)) {
    stop("Colour arguments cannot be NULL")
  }

  # Handle different input combinations
  if (is.function(col) || is.function(col2)) {
    # Return a function that blends the palette outputs
    function(n) {
      # Get colours from functions or use directly
      col_vctr <- if (is.function(col)) col(n) else rep_len(col, n)
      col2_vctr <- if (is.function(col2)) col2(n) else rep_len(col2, n)
      # Perform lighten blend
      .blend_lighten(col_vctr, col2_vctr)
    }
  } else if (is.character(col) && is.character(col2)) {
    # Blend the colour vectors directly
    .blend_lighten(col, col2)
  } else {
    stop("Arguments must be either character vectors of colours or palette functions")
  }
}

#' Darken blend colours with proper alpha compositing
#'
#' @description
#' Blends colours using darken mode with proper alpha handling, selecting
#' the darker of the color values in each component. Implements the exact
#' darken blend formula used by Cairo graphics engine.
#'
#' Formula from Cairo documentation:
#' `aR = aA + aB * (1 - aA)`
#' `xR = (1/aR) * [(1 - aB) * xaA + (1 - aA) * xaB + aA * aB * f(xA, xB)]`
#' where `f(xA, xB) = min(xA, xB)` for darken blend mode
#'
#' @param ... Either one or two colour arguments:
#'   - If one argument: the colour is blended with itself (no change)
#'   - If two arguments: the first is blended with the second
#'   Each argument can be a character vector of colours or a `scales::pal_*()` function
#'
#' @return
#' If inputs are character vectors, returns a character vector of blended colours.
#' If any input is a function, returns a function that generates blended colours.
#'
#' @export
#' @examples
#' # Blend two colours - takes the darker component of each
#' blend_darken("#FF00FF", "#FFFF00")  # Magenta vs Yellow = Red
#' blend_darken("#808080", "#404040")  # Gray vs darker gray = darker gray
#'
#' # Darken a colour with itself (no change)
#' blend_darken("#FF6600")  # Orange darkened with itself = same orange
#'
#' # Work with vectors
#' blend_darken(c("#FF0000", "#00FF00"), c("#0000FF", "#FF00FF"))
blend_darken <- function(...) {
  dots <- list(...)

  # Validate number of arguments
  if (length(dots) == 0) {
    stop("At least one colour argument is required")
  } else if (length(dots) == 1) {
    # Self-darkening
    col <- dots[[1]]
    col2 <- col
  } else if (length(dots) == 2) {
    # Blend two colours
    col <- dots[[1]]
    col2 <- dots[[2]]
  } else {
    stop("blend_darken accepts at most 2 colour arguments, got ", length(dots))
  }

  # Validate inputs are not NULL
  if (is.null(col) || is.null(col2)) {
    stop("Colour arguments cannot be NULL")
  }

  # Handle different input combinations
  if (is.function(col) || is.function(col2)) {
    # Return a function that blends the palette outputs
    function(n) {
      # Get colours from functions or use directly
      col_vctr <- if (is.function(col)) col(n) else rep_len(col, n)
      col2_vctr <- if (is.function(col2)) col2(n) else rep_len(col2, n)
      # Perform darken blend
      .blend_darken(col_vctr, col2_vctr)
    }
  } else if (is.character(col) && is.character(col2)) {
    # Blend the colour vectors directly
    .blend_darken(col, col2)
  } else {
    stop("Arguments must be either character vectors of colours or palette functions")
  }
}

#' Internal multiply blend function
#'
#' @description
#' Internal function that performs multiply blending using the Cairo formula.
#' Handles alpha transparency properly using Porter-Duff compositing.
#'
#' @param col Character vector of colours (source)
#' @param col2 Character vector of colours (destination)
#'
#' @return Character vector of blended colours
#'
#' @noRd
.blend_multiply <- function(col, col2) {
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

    # Apply Cairo multiply blend formula:
    # xR = (1/aR) * [(1 - aB) * xaA + (1 - aA) * xaB + aA * aB * f(xA, xB)]
    # where f(xA, xB) = xA * xB for multiply

    blend_function <- xA * xB  # f(xA, xB) for multiply

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

#' Internal screen blend function
#'
#' @description
#' Internal function that performs screen blending using the Cairo formula.
#' Handles alpha transparency properly using Porter-Duff compositing.
#'
#' @param col Character vector of colours (source)
#' @param col2 Character vector of colours (destination)
#'
#' @return Character vector of blended colours
#'
#' @noRd
.blend_screen <- function(col, col2) {
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

    # Apply Cairo screen blend formula:
    # xR = (1/aR) * [(1 - aB) * xaA + (1 - aA) * xaB + aA * aB * f(xA, xB)]
    # where f(xA, xB) = xA + xB - xA * xB for screen (inverse of multiply)

    blend_function <- xA + xB - xA * xB  # f(xA, xB) for screen

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
.blend_lighten <- function(col, col2) {
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
.blend_darken <- function(col, col2) {
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


#' Overlay blend colours with proper alpha compositing
#'
#' @description
#' Blends colours using overlay mode with proper alpha handling. Multiplies or
#' screens colors depending on the destination color, enhancing contrast.
#' Implements the exact overlay blend formula used by Cairo graphics engine.
#'
#' Formula from Cairo documentation:
#' `aR = aA + aB * (1 - aA)`
#' `xR = (1/aR) * [(1 - aB) * xaA + (1 - aA) * xaB + aA * aB * f(xA, xB)]`
#' where `f(xA, xB) = 2*xA*xB if xB <= 0.5, else 1 - 2*(1-xA)*(1-xB)`
#'
#' @param ... Either one or two colour arguments
#' @return Character vector of blended colours or a function
#' @export
blend_overlay <- function(...) {
  dots <- list(...)

  if (length(dots) == 0) {
    stop("At least one colour argument is required")
  } else if (length(dots) == 1) {
    col <- dots[[1]]
    col2 <- col
  } else if (length(dots) == 2) {
    col <- dots[[1]]
    col2 <- dots[[2]]
  } else {
    stop("blend_overlay accepts at most 2 colour arguments, got ", length(dots))
  }

  if (is.null(col) || is.null(col2)) {
    stop("Colour arguments cannot be NULL")
  }

  if (is.function(col) || is.function(col2)) {
    function(n) {
      col_vctr <- if (is.function(col)) col(n) else rep_len(col, n)
      col2_vctr <- if (is.function(col2)) col2(n) else rep_len(col2, n)
      .blend_overlay(col_vctr, col2_vctr)
    }
  } else if (is.character(col) && is.character(col2)) {
    .blend_overlay(col, col2)
  } else {
    stop("Arguments must be either character vectors of colours or palette functions")
  }
}

#' Color dodge blend colours
#'
#' @description
#' Brightens the destination color by a factor depending on the source color.
#' Formula: `f(xA, xB) = min(1, xB/(1-xA))` if xA < 1, else 1
#'
#' @param ... Either one or two colour arguments
#' @return Character vector of blended colours or a function
#' @export
blend_colour_dodge <- function(...) {
  dots <- list(...)

  if (length(dots) == 0) {
    stop("At least one colour argument is required")
  } else if (length(dots) == 1) {
    col <- dots[[1]]
    col2 <- col
  } else if (length(dots) == 2) {
    col <- dots[[1]]
    col2 <- dots[[2]]
  } else {
    stop("blend_colour_dodge accepts at most 2 colour arguments, got ", length(dots))
  }

  if (is.null(col) || is.null(col2)) {
    stop("Colour arguments cannot be NULL")
  }

  if (is.function(col) || is.function(col2)) {
    function(n) {
      col_vctr <- if (is.function(col)) col(n) else rep_len(col, n)
      col2_vctr <- if (is.function(col2)) col2(n) else rep_len(col2, n)
      .blend_colour_dodge(col_vctr, col2_vctr)
    }
  } else if (is.character(col) && is.character(col2)) {
    .blend_colour_dodge(col, col2)
  } else {
    stop("Arguments must be either character vectors of colours or palette functions")
  }
}

#' Color burn blend colours
#'
#' @description
#' Darkens the destination color by a factor depending on the source color.
#' Formula: `f(xA, xB) = 1 - min(1, (1-xB)/xA)` if xA > 0, else 0
#'
#' @param ... Either one or two colour arguments
#' @return Character vector of blended colours or a function
#' @export
blend_colour_burn <- function(...) {
  dots <- list(...)

  if (length(dots) == 0) {
    stop("At least one colour argument is required")
  } else if (length(dots) == 1) {
    col <- dots[[1]]
    col2 <- col
  } else if (length(dots) == 2) {
    col <- dots[[1]]
    col2 <- dots[[2]]
  } else {
    stop("blend_colour_burn accepts at most 2 colour arguments, got ", length(dots))
  }

  if (is.null(col) || is.null(col2)) {
    stop("Colour arguments cannot be NULL")
  }

  if (is.function(col) || is.function(col2)) {
    function(n) {
      col_vctr <- if (is.function(col)) col(n) else rep_len(col, n)
      col2_vctr <- if (is.function(col2)) col2(n) else rep_len(col2, n)
      .blend_colour_burn(col_vctr, col2_vctr)
    }
  } else if (is.character(col) && is.character(col2)) {
    .blend_colour_burn(col, col2)
  } else {
    stop("Arguments must be either character vectors of colours or palette functions")
  }
}

#' Hard light blend colours
#'
#' @description
#' Multiplies or screens colors, depending on the source color.
#' Similar to overlay but with source and destination roles reversed.
#'
#' @param ... Either one or two colour arguments
#' @return Character vector of blended colours or a function
#' @export
blend_hard_light <- function(...) {
  dots <- list(...)

  if (length(dots) == 0) {
    stop("At least one colour argument is required")
  } else if (length(dots) == 1) {
    col <- dots[[1]]
    col2 <- col
  } else if (length(dots) == 2) {
    col <- dots[[1]]
    col2 <- dots[[2]]
  } else {
    stop("blend_hard_light accepts at most 2 colour arguments, got ", length(dots))
  }

  if (is.null(col) || is.null(col2)) {
    stop("Colour arguments cannot be NULL")
  }

  if (is.function(col) || is.function(col2)) {
    function(n) {
      col_vctr <- if (is.function(col)) col(n) else rep_len(col, n)
      col2_vctr <- if (is.function(col2)) col2(n) else rep_len(col2, n)
      .blend_hard_light(col_vctr, col2_vctr)
    }
  } else if (is.character(col) && is.character(col2)) {
    .blend_hard_light(col, col2)
  } else {
    stop("Arguments must be either character vectors of colours or palette functions")
  }
}

#' Soft light blend colours
#'
#' @description
#' Darkens or lightens, depending on the source color.
#' Similar to overlay but with a softer effect.
#'
#' @param ... Either one or two colour arguments
#' @return Character vector of blended colours or a function
#' @export
blend_soft_light <- function(...) {
  dots <- list(...)

  if (length(dots) == 0) {
    stop("At least one colour argument is required")
  } else if (length(dots) == 1) {
    col <- dots[[1]]
    col2 <- col
  } else if (length(dots) == 2) {
    col <- dots[[1]]
    col2 <- dots[[2]]
  } else {
    stop("blend_soft_light accepts at most 2 colour arguments, got ", length(dots))
  }

  if (is.null(col) || is.null(col2)) {
    stop("Colour arguments cannot be NULL")
  }

  if (is.function(col) || is.function(col2)) {
    function(n) {
      col_vctr <- if (is.function(col)) col(n) else rep_len(col, n)
      col2_vctr <- if (is.function(col2)) col2(n) else rep_len(col2, n)
      .blend_soft_light(col_vctr, col2_vctr)
    }
  } else if (is.character(col) && is.character(col2)) {
    .blend_soft_light(col, col2)
  } else {
    stop("Arguments must be either character vectors of colours or palette functions")
  }
}

#' Difference blend colours
#'
#' @description
#' Takes the absolute difference of the destination and source colors.
#' Formula: `f(xA, xB) = abs(xB - xA)`
#'
#' @param ... Either one or two colour arguments
#' @return Character vector of blended colours or a function
#' @export
blend_difference <- function(...) {
  dots <- list(...)

  if (length(dots) == 0) {
    stop("At least one colour argument is required")
  } else if (length(dots) == 1) {
    col <- dots[[1]]
    col2 <- col
  } else if (length(dots) == 2) {
    col <- dots[[1]]
    col2 <- dots[[2]]
  } else {
    stop("blend_difference accepts at most 2 colour arguments, got ", length(dots))
  }

  if (is.null(col) || is.null(col2)) {
    stop("Colour arguments cannot be NULL")
  }

  if (is.function(col) || is.function(col2)) {
    function(n) {
      col_vctr <- if (is.function(col)) col(n) else rep_len(col, n)
      col2_vctr <- if (is.function(col2)) col2(n) else rep_len(col2, n)
      .blend_difference(col_vctr, col2_vctr)
    }
  } else if (is.character(col) && is.character(col2)) {
    .blend_difference(col, col2)
  } else {
    stop("Arguments must be either character vectors of colours or palette functions")
  }
}

#' Exclusion blend colours
#'
#' @description
#' Similar to difference but with lower contrast.
#' Formula: `f(xA, xB) = xA + xB - 2*xA*xB`
#'
#' @param ... Either one or two colour arguments
#' @return Character vector of blended colours or a function
#' @export
blend_exclusion <- function(...) {
  dots <- list(...)

  if (length(dots) == 0) {
    stop("At least one colour argument is required")
  } else if (length(dots) == 1) {
    col <- dots[[1]]
    col2 <- col
  } else if (length(dots) == 2) {
    col <- dots[[1]]
    col2 <- dots[[2]]
  } else {
    stop("blend_exclusion accepts at most 2 colour arguments, got ", length(dots))
  }

  if (is.null(col) || is.null(col2)) {
    stop("Colour arguments cannot be NULL")
  }

  if (is.function(col) || is.function(col2)) {
    function(n) {
      col_vctr <- if (is.function(col)) col(n) else rep_len(col, n)
      col2_vctr <- if (is.function(col2)) col2(n) else rep_len(col2, n)
      .blend_exclusion(col_vctr, col2_vctr)
    }
  } else if (is.character(col) && is.character(col2)) {
    .blend_exclusion(col, col2)
  } else {
    stop("Arguments must be either character vectors of colours or palette functions")
  }
}

# Internal implementation functions
# All follow the same Cairo formula structure with different f(xA, xB)

.blend_overlay <- function(col, col2) {
  if (any(is.na(col)) || any(is.na(col2))) {
    warning("NA values in color inputs may produce unexpected results")
  }

  len <- max(length(col), length(col2))
  col <- rep_len(col, len)
  col2 <- rep_len(col2, len)

  rgba_A <- matrix(grDevices::col2rgb(col, alpha = TRUE) / 255, nrow = 4)
  rgba_B <- matrix(grDevices::col2rgb(col2, alpha = TRUE) / 255, nrow = 4)

  aA <- rgba_A[4, ]
  aB <- rgba_B[4, ]
  aR <- aA + aB * (1 - aA)
  aR_safe <- ifelse(aR == 0, 1, aR)

  result_rgb <- matrix(0, nrow = 3, ncol = length(aA))

  for (i in 1:3) {
    xA <- rgba_A[i, ]
    xB <- rgba_B[i, ]
    xaA <- xA * aA
    xaB <- xB * aB

    # Overlay: multiply or screen based on destination
    blend_function <- ifelse(
      xB <= 0.5,
      2 * xA * xB,
      1 - 2 * (1 - xA) * (1 - xB)
    )

    xR <- (1 / aR_safe) * (
      (1 - aB) * xaA +
        (1 - aA) * xaB +
        aA * aB * blend_function
    )

    xR <- ifelse(aR == 0, 0, xR)
    result_rgb[i, ] <- pmax(0, pmin(1, xR))
  }

  grDevices::rgb(result_rgb[1, ], result_rgb[2, ], result_rgb[3, ], aR,
                 maxColorValue = 1)
}

.blend_colour_dodge <- function(col, col2) {
  if (any(is.na(col)) || any(is.na(col2))) {
    warning("NA values in color inputs may produce unexpected results")
  }

  len <- max(length(col), length(col2))
  col <- rep_len(col, len)
  col2 <- rep_len(col2, len)

  rgba_A <- matrix(grDevices::col2rgb(col, alpha = TRUE) / 255, nrow = 4)
  rgba_B <- matrix(grDevices::col2rgb(col2, alpha = TRUE) / 255, nrow = 4)

  aA <- rgba_A[4, ]
  aB <- rgba_B[4, ]
  aR <- aA + aB * (1 - aA)
  aR_safe <- ifelse(aR == 0, 1, aR)

  result_rgb <- matrix(0, nrow = 3, ncol = length(aA))

  for (i in 1:3) {
    xA <- rgba_A[i, ]
    xB <- rgba_B[i, ]
    xaA <- xA * aA
    xaB <- xB * aB

    # Color dodge
    blend_function <- ifelse(
      xA < 1,
      pmin(1, xB / (1 - xA)),
      1
    )

    xR <- (1 / aR_safe) * (
      (1 - aB) * xaA +
        (1 - aA) * xaB +
        aA * aB * blend_function
    )

    xR <- ifelse(aR == 0, 0, xR)
    result_rgb[i, ] <- pmax(0, pmin(1, xR))
  }

  grDevices::rgb(result_rgb[1, ], result_rgb[2, ], result_rgb[3, ], aR,
                 maxColorValue = 1)
}

.blend_colour_burn <- function(col, col2) {
  if (any(is.na(col)) || any(is.na(col2))) {
    warning("NA values in color inputs may produce unexpected results")
  }

  len <- max(length(col), length(col2))
  col <- rep_len(col, len)
  col2 <- rep_len(col2, len)

  rgba_A <- matrix(grDevices::col2rgb(col, alpha = TRUE) / 255, nrow = 4)
  rgba_B <- matrix(grDevices::col2rgb(col2, alpha = TRUE) / 255, nrow = 4)

  aA <- rgba_A[4, ]
  aB <- rgba_B[4, ]
  aR <- aA + aB * (1 - aA)
  aR_safe <- ifelse(aR == 0, 1, aR)

  result_rgb <- matrix(0, nrow = 3, ncol = length(aA))

  for (i in 1:3) {
    xA <- rgba_A[i, ]
    xB <- rgba_B[i, ]
    xaA <- xA * aA
    xaB <- xB * aB

    # Color burn
    blend_function <- ifelse(
      xA > 0,
      1 - pmin(1, (1 - xB) / xA),
      0
    )

    xR <- (1 / aR_safe) * (
      (1 - aB) * xaA +
        (1 - aA) * xaB +
        aA * aB * blend_function
    )

    xR <- ifelse(aR == 0, 0, xR)
    result_rgb[i, ] <- pmax(0, pmin(1, xR))
  }

  grDevices::rgb(result_rgb[1, ], result_rgb[2, ], result_rgb[3, ], aR,
                 maxColorValue = 1)
}

.blend_hard_light <- function(col, col2) {
  if (any(is.na(col)) || any(is.na(col2))) {
    warning("NA values in color inputs may produce unexpected results")
  }

  len <- max(length(col), length(col2))
  col <- rep_len(col, len)
  col2 <- rep_len(col2, len)

  rgba_A <- matrix(grDevices::col2rgb(col, alpha = TRUE) / 255, nrow = 4)
  rgba_B <- matrix(grDevices::col2rgb(col2, alpha = TRUE) / 255, nrow = 4)

  aA <- rgba_A[4, ]
  aB <- rgba_B[4, ]
  aR <- aA + aB * (1 - aA)
  aR_safe <- ifelse(aR == 0, 1, aR)

  result_rgb <- matrix(0, nrow = 3, ncol = length(aA))

  for (i in 1:3) {
    xA <- rgba_A[i, ]
    xB <- rgba_B[i, ]
    xaA <- xA * aA
    xaB <- xB * aB

    # Hard light: multiply or screen based on source (not dest)
    blend_function <- ifelse(
      xA <= 0.5,
      2 * xA * xB,
      1 - 2 * (1 - xA) * (1 - xB)
    )

    xR <- (1 / aR_safe) * (
      (1 - aB) * xaA +
        (1 - aA) * xaB +
        aA * aB * blend_function
    )

    xR <- ifelse(aR == 0, 0, xR)
    result_rgb[i, ] <- pmax(0, pmin(1, xR))
  }

  grDevices::rgb(result_rgb[1, ], result_rgb[2, ], result_rgb[3, ], aR,
                 maxColorValue = 1)
}

.blend_soft_light <- function(col, col2) {
  if (any(is.na(col)) || any(is.na(col2))) {
    warning("NA values in color inputs may produce unexpected results")
  }

  len <- max(length(col), length(col2))
  col <- rep_len(col, len)
  col2 <- rep_len(col2, len)

  rgba_A <- matrix(grDevices::col2rgb(col, alpha = TRUE) / 255, nrow = 4)
  rgba_B <- matrix(grDevices::col2rgb(col2, alpha = TRUE) / 255, nrow = 4)

  aA <- rgba_A[4, ]
  aB <- rgba_B[4, ]
  aR <- aA + aB * (1 - aA)
  aR_safe <- ifelse(aR == 0, 1, aR)

  result_rgb <- matrix(0, nrow = 3, ncol = length(aA))

  for (i in 1:3) {
    xA <- rgba_A[i, ]
    xB <- rgba_B[i, ]
    xaA <- xA * aA
    xaB <- xB * aB

    # Soft light with helper function g(x)
    g_xB <- ifelse(
      xB <= 0.25,
      ((16 * xB - 12) * xB + 4) * xB,
      sqrt(xB)
    )

    blend_function <- ifelse(
      xA <= 0.5,
      xB - (1 - 2 * xA) * xB * (1 - xB),
      xB + (2 * xA - 1) * (g_xB - xB)
    )

    xR <- (1 / aR_safe) * (
      (1 - aB) * xaA +
        (1 - aA) * xaB +
        aA * aB * blend_function
    )

    xR <- ifelse(aR == 0, 0, xR)
    result_rgb[i, ] <- pmax(0, pmin(1, xR))
  }

  grDevices::rgb(result_rgb[1, ], result_rgb[2, ], result_rgb[3, ], aR,
                 maxColorValue = 1)
}

.blend_difference <- function(col, col2) {
  if (any(is.na(col)) || any(is.na(col2))) {
    warning("NA values in color inputs may produce unexpected results")
  }

  len <- max(length(col), length(col2))
  col <- rep_len(col, len)
  col2 <- rep_len(col2, len)

  rgba_A <- matrix(grDevices::col2rgb(col, alpha = TRUE) / 255, nrow = 4)
  rgba_B <- matrix(grDevices::col2rgb(col2, alpha = TRUE) / 255, nrow = 4)

  aA <- rgba_A[4, ]
  aB <- rgba_B[4, ]
  aR <- aA + aB * (1 - aA)
  aR_safe <- ifelse(aR == 0, 1, aR)

  result_rgb <- matrix(0, nrow = 3, ncol = length(aA))

  for (i in 1:3) {
    xA <- rgba_A[i, ]
    xB <- rgba_B[i, ]
    xaA <- xA * aA
    xaB <- xB * aB

    # Difference: absolute difference
    blend_function <- abs(xB - xA)

    xR <- (1 / aR_safe) * (
      (1 - aB) * xaA +
        (1 - aA) * xaB +
        aA * aB * blend_function
    )

    xR <- ifelse(aR == 0, 0, xR)
    result_rgb[i, ] <- pmax(0, pmin(1, xR))
  }

  grDevices::rgb(result_rgb[1, ], result_rgb[2, ], result_rgb[3, ], aR,
                 maxColorValue = 1)
}

.blend_exclusion <- function(col, col2) {
  if (any(is.na(col)) || any(is.na(col2))) {
    warning("NA values in color inputs may produce unexpected results")
  }

  len <- max(length(col), length(col2))
  col <- rep_len(col, len)
  col2 <- rep_len(col2, len)

  rgba_A <- matrix(grDevices::col2rgb(col, alpha = TRUE) / 255, nrow = 4)
  rgba_B <- matrix(grDevices::col2rgb(col2, alpha = TRUE) / 255, nrow = 4)

  aA <- rgba_A[4, ]
  aB <- rgba_B[4, ]
  aR <- aA + aB * (1 - aA)
  aR_safe <- ifelse(aR == 0, 1, aR)

  result_rgb <- matrix(0, nrow = 3, ncol = length(aA))

  for (i in 1:3) {
    xA <- rgba_A[i, ]
    xB <- rgba_B[i, ]
    xaA <- xA * aA
    xaB <- xB * aB

    # Exclusion
    blend_function <- xA + xB - 2 * xA * xB

    xR <- (1 / aR_safe) * (
      (1 - aB) * xaA +
        (1 - aA) * xaB +
        aA * aB * blend_function
    )

    xR <- ifelse(aR == 0, 0, xR)
    result_rgb[i, ] <- pmax(0, pmin(1, xR))
  }

  grDevices::rgb(result_rgb[1, ], result_rgb[2, ], result_rgb[3, ], aR,
                 maxColorValue = 1)
}
