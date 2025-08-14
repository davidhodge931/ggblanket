#' Internal multiply blend implementation
#'
#' @param col1 Character vector of colours
#' @param col2 Character vector of colours
#' @return Character vector of multiply-blended colours
#' @noRd
.apply_blend_multiply <- function(col1, col2) {
  # Ensure vectors have compatible lengths
  len <- max(length(col1), length(col2))
  col1 <- rep_len(col1, len)
  col2 <- rep_len(col2, len)

  # Convert to RGB matrices - handle single color case
  rgb1 <- col2rgb(col1, alpha = TRUE)
  if (is.null(dim(rgb1)) || length(dim(rgb1)) == 1) {
    rgb1 <- matrix(rgb1, nrow = 4, ncol = 1)
  }
  rgb1 <- rgb1 / 255

  rgb2 <- col2rgb(col2, alpha = TRUE)
  if (is.null(dim(rgb2)) || length(dim(rgb2)) == 1) {
    rgb2 <- matrix(rgb2, nrow = 4, ncol = 1)
  }
  rgb2 <- rgb2 / 255

  # Extract alpha channels
  alpha1 <- rgb1[4, ]
  alpha2 <- rgb2[4, ]

  # Calculate result alpha using Porter-Duff formula
  # αr = α1 + α2·(1-α1)
  alpha_result <- alpha1 + alpha2 * (1 - alpha1)

  # Initialize result matrix
  n_colors <- ncol(rgb1)
  rgb_result <- matrix(0, nrow = 3, ncol = n_colors)

  for (i in 1:3) {
    # Get color channel values
    c1 <- rgb1[i, ]
    c2 <- rgb2[i, ]

    # Apply multiply blend formula with proper alpha compositing
    # For multiply: f(c1, c2) = c1 * c2
    # Result = (c1·α1·(1-α2) + c2·α2·(1-α1) + c1·c2·α1·α2) / αr

    # Handle case where result alpha is 0
    idx_valid <- alpha_result > 0

    result <- numeric(length(c1))
    result[idx_valid] <- (
      c1[idx_valid] * alpha1[idx_valid] * (1 - alpha2[idx_valid]) +  # Source only
        c2[idx_valid] * alpha2[idx_valid] * (1 - alpha1[idx_valid]) +  # Dest only
        c1[idx_valid] * c2[idx_valid] * alpha1[idx_valid] * alpha2[idx_valid]  # Multiply blend
    ) / alpha_result[idx_valid]

    # Set fully transparent pixels to 0
    result[!idx_valid] <- 0

    rgb_result[i, ] <- result
  }

  # Clamp values to [0, 1] - preserve matrix structure
  rgb_result[] <- pmax(0, pmin(1, rgb_result))
  alpha_result <- pmax(0, pmin(1, alpha_result))

  # Convert back to hex colours
  rgb(rgb_result[1, ], rgb_result[2, ], rgb_result[3, ], alpha_result)
}

#' Internal screen blend implementation
#'
#' @param col1 Character vector of colours
#' @param col2 Character vector of colours
#' @return Character vector of screen-blended colours
#' @noRd
.apply_blend_screen <- function(col1, col2) {
  # Ensure vectors have compatible lengths
  len <- max(length(col1), length(col2))
  col1 <- rep_len(col1, len)
  col2 <- rep_len(col2, len)

  # Convert to RGB matrices - handle single color case
  rgb1 <- col2rgb(col1, alpha = TRUE)
  if (is.null(dim(rgb1)) || length(dim(rgb1)) == 1) {
    rgb1 <- matrix(rgb1, nrow = 4, ncol = 1)
  }
  rgb1 <- rgb1 / 255

  rgb2 <- col2rgb(col2, alpha = TRUE)
  if (is.null(dim(rgb2)) || length(dim(rgb2)) == 1) {
    rgb2 <- matrix(rgb2, nrow = 4, ncol = 1)
  }
  rgb2 <- rgb2 / 255

  # Extract alpha channels
  alpha1 <- rgb1[4, ]
  alpha2 <- rgb2[4, ]

  # Calculate result alpha using Porter-Duff formula
  # αr = α1 + α2·(1-α1)
  alpha_result <- alpha1 + alpha2 * (1 - alpha1)

  # Initialize result matrix
  n_colors <- ncol(rgb1)
  rgb_result <- matrix(0, nrow = 3, ncol = n_colors)

  for (i in 1:3) {
    # Get color channel values
    c1 <- rgb1[i, ]
    c2 <- rgb2[i, ]

    # Apply screen blend formula with proper alpha compositing
    # For screen: f(c1, c2) = 1 - (1 - c1) * (1 - c2) = c1 + c2 - c1*c2
    # Result = (c1·α1·(1-α2) + c2·α2·(1-α1) + (c1 + c2 - c1·c2)·α1·α2) / αr

    # Handle case where result alpha is 0
    idx_valid <- alpha_result > 0

    # Screen blend function
    screen_blend <- c1 + c2 - c1 * c2

    result <- numeric(length(c1))
    result[idx_valid] <- (
      c1[idx_valid] * alpha1[idx_valid] * (1 - alpha2[idx_valid]) +  # Source only
        c2[idx_valid] * alpha2[idx_valid] * (1 - alpha1[idx_valid]) +  # Dest only
        screen_blend[idx_valid] * alpha1[idx_valid] * alpha2[idx_valid]  # Screen blend
    ) / alpha_result[idx_valid]

    # Set fully transparent pixels to 0
    result[!idx_valid] <- 0

    rgb_result[i, ] <- result
  }

  # Clamp values to [0, 1] - preserve matrix structure
  rgb_result[] <- pmax(0, pmin(1, rgb_result))
  alpha_result <- pmax(0, pmin(1, alpha_result))

  # Convert back to hex colours
  rgb(rgb_result[1, ], rgb_result[2, ], rgb_result[3, ], alpha_result)
}

#' Internal lighten blend implementation
#'
#' @param col1 Character vector of colours
#' @param col2 Character vector of colours
#' @return Character vector of lighten-blended colours
#' @noRd
.apply_blend_lighten <- function(col1, col2) {
  # Ensure vectors have compatible lengths
  len <- max(length(col1), length(col2))
  col1 <- rep_len(col1, len)
  col2 <- rep_len(col2, len)

  # Convert to RGB matrices - handle single color case
  rgb1 <- col2rgb(col1, alpha = TRUE)
  if (is.null(dim(rgb1)) || length(dim(rgb1)) == 1) {
    rgb1 <- matrix(rgb1, nrow = 4, ncol = 1)
  }
  rgb1 <- rgb1 / 255

  rgb2 <- col2rgb(col2, alpha = TRUE)
  if (is.null(dim(rgb2)) || length(dim(rgb2)) == 1) {
    rgb2 <- matrix(rgb2, nrow = 4, ncol = 1)
  }
  rgb2 <- rgb2 / 255

  # Extract alpha channels
  alpha1 <- rgb1[4, ]
  alpha2 <- rgb2[4, ]

  # Calculate result alpha using Porter-Duff formula
  # αr = α1 + α2·(1-α1)
  alpha_result <- alpha1 + alpha2 * (1 - alpha1)

  # Initialize result matrix
  n_colors <- ncol(rgb1)
  rgb_result <- matrix(0, nrow = 3, ncol = n_colors)

  for (i in 1:3) {
    # Get color channel values
    c1 <- rgb1[i, ]
    c2 <- rgb2[i, ]

    # Apply lighten blend formula with proper alpha compositing
    # For lighten: f(c1, c2) = max(c1, c2)
    # Result = (c1·α1·(1-α2) + c2·α2·(1-α1) + max(c1,c2)·α1·α2) / αr

    # Handle case where result alpha is 0
    idx_valid <- alpha_result > 0

    # Lighten blend function
    lighten_blend <- pmax(c1, c2)

    result <- numeric(length(c1))
    result[idx_valid] <- (
      c1[idx_valid] * alpha1[idx_valid] * (1 - alpha2[idx_valid]) +  # Source only
        c2[idx_valid] * alpha2[idx_valid] * (1 - alpha1[idx_valid]) +  # Dest only
        lighten_blend[idx_valid] * alpha1[idx_valid] * alpha2[idx_valid]  # Lighten blend
    ) / alpha_result[idx_valid]

    # Set fully transparent pixels to 0
    result[!idx_valid] <- 0

    rgb_result[i, ] <- result
  }

  # Clamp values to [0, 1] - preserve matrix structure
  rgb_result[] <- pmax(0, pmin(1, rgb_result))
  alpha_result <- pmax(0, pmin(1, alpha_result))

  # Convert back to hex colours
  rgb(rgb_result[1, ], rgb_result[2, ], rgb_result[3, ], alpha_result)
}

#' Internal darken blend implementation
#'
#' @param col1 Character vector of colours
#' @param col2 Character vector of colours
#' @return Character vector of darken-blended colours
#' @noRd
.apply_blend_darken <- function(col1, col2) {
  # Ensure vectors have compatible lengths
  len <- max(length(col1), length(col2))
  col1 <- rep_len(col1, len)
  col2 <- rep_len(col2, len)

  # Convert to RGB matrices - handle single color case
  rgb1 <- col2rgb(col1, alpha = TRUE)
  if (is.null(dim(rgb1)) || length(dim(rgb1)) == 1) {
    rgb1 <- matrix(rgb1, nrow = 4, ncol = 1)
  }
  rgb1 <- rgb1 / 255

  rgb2 <- col2rgb(col2, alpha = TRUE)
  if (is.null(dim(rgb2)) || length(dim(rgb2)) == 1) {
    rgb2 <- matrix(rgb2, nrow = 4, ncol = 1)
  }
  rgb2 <- rgb2 / 255

  # Extract alpha channels
  alpha1 <- rgb1[4, ]
  alpha2 <- rgb2[4, ]

  # Calculate result alpha using Porter-Duff formula
  # αr = α1 + α2·(1-α1)
  alpha_result <- alpha1 + alpha2 * (1 - alpha1)

  # Initialize result matrix
  n_colors <- ncol(rgb1)
  rgb_result <- matrix(0, nrow = 3, ncol = n_colors)

  for (i in 1:3) {
    # Get color channel values
    c1 <- rgb1[i, ]
    c2 <- rgb2[i, ]

    # Apply darken blend formula with proper alpha compositing
    # For darken: f(c1, c2) = min(c1, c2)
    # Result = (c1·α1·(1-α2) + c2·α2·(1-α1) + min(c1,c2)·α1·α2) / αr

    # Handle case where result alpha is 0
    idx_valid <- alpha_result > 0

    # Darken blend function
    darken_blend <- pmin(c1, c2)

    result <- numeric(length(c1))
    result[idx_valid] <- (
      c1[idx_valid] * alpha1[idx_valid] * (1 - alpha2[idx_valid]) +  # Source only
        c2[idx_valid] * alpha2[idx_valid] * (1 - alpha1[idx_valid]) +  # Dest only
        darken_blend[idx_valid] * alpha1[idx_valid] * alpha2[idx_valid]  # Darken blend
    ) / alpha_result[idx_valid]

    # Set fully transparent pixels to 0
    result[!idx_valid] <- 0

    rgb_result[i, ] <- result
  }

  # Clamp values to [0, 1] - preserve matrix structure
  rgb_result[] <- pmax(0, pmin(1, rgb_result))
  alpha_result <- pmax(0, pmin(1, alpha_result))

  # Convert back to hex colours
  rgb(rgb_result[1, ], rgb_result[2, ], rgb_result[3, ], alpha_result)
}

# Blend Functions with Cairo Algorithms
# Implements multiply, screen, lighten, and darken blend modes with proper alpha compositing

#' Multiply blend colours with proper alpha compositing
#'
#' @description
#' Blends colours using multiply mode with proper alpha handling, creating
#' a darker, more saturated effect.
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
    function(x) {
      # Get colours from functions or use directly
      col_vctr <- if (is.function(col)) {
        # Try to call the palette with x
        tryCatch(
          col(x),
          error = function(e) {
            # If it fails and x is a vector, it might be a discrete palette
            # being called with continuous values
            if (length(x) > 1 && inherits(col, "pal_discrete")) {
              # Convert discrete palette to continuous
              n_colors <- attr(col, "nlevels") %||% 256
              colors <- col(min(n_colors, 256))
              gradient_fn <- scales::pal_gradient_n(colours = colors)
              gradient_fn(x)
            } else if (length(x) == 1 && is.numeric(x)) {
              # Single value - might need to generate a sequence
              col(seq(0, 1, length.out = x))
            } else {
              stop(e)
            }
          }
        )
      } else {
        rep_len(col, length(x))
      }
      col2_vctr <- if (is.function(col2)) {
        # Try to call the palette with x
        tryCatch(
          col2(x),
          error = function(e) {
            # If it fails and x is a vector, it might be a discrete palette
            # being called with continuous values
            if (length(x) > 1 && inherits(col2, "pal_discrete")) {
              # Convert discrete palette to continuous
              n_colors <- attr(col2, "nlevels") %||% 256
              colors <- col2(min(n_colors, 256))
              gradient_fn <- scales::pal_gradient_n(colours = colors)
              gradient_fn(x)
            } else if (length(x) == 1 && is.numeric(x)) {
              # Single value - might need to generate a sequence
              col2(seq(0, 1, length.out = x))
            } else {
              stop(e)
            }
          }
        )
      } else {
        rep_len(col2, length(x))
      }
      # Perform multiply blend
      .apply_blend_multiply(col_vctr, col2_vctr)
    }
  } else if (is.character(col) && is.character(col2)) {
    # Blend the colour vectors directly
    .apply_blend_multiply(col, col2)
  } else {
    stop("Arguments must be either character vectors of colours or palette functions")
  }
}

#' Screen blend colours with proper alpha compositing
#'
#' @description
#' Blends colours using screen mode with proper alpha handling, creating
#' a lighter, brighter effect (inverse of multiply).
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
blend_screen <- function(...) {
  dots <- list(...)
  # Validate number of arguments
  if (length(dots) == 0) {
    stop("At least one colour argument is required")
  } else if (length(dots) == 1) {
    # Self-screen
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
    function(x) {
      # Get colours from functions or use directly
      col_vctr <- if (is.function(col)) {
        # Try to call the palette with x
        tryCatch(
          col(x),
          error = function(e) {
            # If it fails and x is a vector, it might be a discrete palette
            # being called with continuous values
            if (length(x) > 1 && inherits(col, "pal_discrete")) {
              # Convert discrete palette to continuous
              n_colors <- attr(col, "nlevels") %||% 256
              colors <- col(min(n_colors, 256))
              gradient_fn <- scales::pal_gradient_n(colours = colors)
              gradient_fn(x)
            } else if (length(x) == 1 && is.numeric(x)) {
              # Single value - might need to generate a sequence
              col(seq(0, 1, length.out = x))
            } else {
              stop(e)
            }
          }
        )
      } else {
        rep_len(col, length(x))
      }
      col2_vctr <- if (is.function(col2)) {
        # Try to call the palette with x
        tryCatch(
          col2(x),
          error = function(e) {
            # If it fails and x is a vector, it might be a discrete palette
            # being called with continuous values
            if (length(x) > 1 && inherits(col2, "pal_discrete")) {
              # Convert discrete palette to continuous
              n_colors <- attr(col2, "nlevels") %||% 256
              colors <- col2(min(n_colors, 256))
              gradient_fn <- scales::pal_gradient_n(colours = colors)
              gradient_fn(x)
            } else if (length(x) == 1 && is.numeric(x)) {
              # Single value - might need to generate a sequence
              col2(seq(0, 1, length.out = x))
            } else {
              stop(e)
            }
          }
        )
      } else {
        rep_len(col2, length(x))
      }
      # Perform screen blend
      .apply_blend_screen(col_vctr, col2_vctr)
    }
  } else if (is.character(col) && is.character(col2)) {
    # Blend the colour vectors directly
    .apply_blend_screen(col, col2)
  } else {
    stop("Arguments must be either character vectors of colours or palette functions")
  }
}
