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
              n_colours <- attr(col, "nlevels") %||% 256
              colours <- col(min(n_colours, 256))
              gradient_fn <- scales::pal_gradient_n(colours = colours)
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
              n_colours <- attr(col2, "nlevels") %||% 256
              colours <- col2(min(n_colours, 256))
              gradient_fn <- scales::pal_gradient_n(colours = colours)
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

  # Convert to RGB matrices - handle single colour case
  rgb1 <- grDevices::col2rgb(col1, alpha = TRUE)
  if (is.null(dim(rgb1)) || length(dim(rgb1)) == 1) {
    rgb1 <- matrix(rgb1, nrow = 4, ncol = 1)
  }
  rgb1 <- rgb1 / 255

  rgb2 <- grDevices::col2rgb(col2, alpha = TRUE)
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
  n_colours <- ncol(rgb1)
  rgb_result <- matrix(0, nrow = 3, ncol = n_colours)

  for (i in 1:3) {
    # Get colour channel values
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
  grDevices::rgb(rgb_result[1, ], rgb_result[2, ], rgb_result[3, ], alpha_result)
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
              n_colours <- attr(col, "nlevels") %||% 256
              colours <- col(min(n_colours, 256))
              gradient_fn <- scales::pal_gradient_n(colours = colours)
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
              n_colours <- attr(col2, "nlevels") %||% 256
              colours <- col2(min(n_colours, 256))
              gradient_fn <- scales::pal_gradient_n(colours = colours)
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

  # Convert to RGB matrices - handle single colour case
  rgb1 <- grDevices::col2rgb(col1, alpha = TRUE)
  if (is.null(dim(rgb1)) || length(dim(rgb1)) == 1) {
    rgb1 <- matrix(rgb1, nrow = 4, ncol = 1)
  }
  rgb1 <- rgb1 / 255

  rgb2 <- grDevices::col2rgb(col2, alpha = TRUE)
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
  n_colours <- ncol(rgb1)
  rgb_result <- matrix(0, nrow = 3, ncol = n_colours)

  for (i in 1:3) {
    # Get colour channel values
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
  grDevices::rgb(rgb_result[1, ], rgb_result[2, ], rgb_result[3, ], alpha_result)
}

#' Darken blend colours with proper alpha compositing
#'
#' @description
#' Blends colours using darken mode with proper alpha handling, selecting
#' the darker of the two colours for each channel.
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
blend_darken <- function(...) {
  dots <- list(...)
  # Validate number of arguments
  if (length(dots) == 0) {
    stop("At least one colour argument is required")
  } else if (length(dots) == 1) {
    # Self-darken (no change)
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
              n_colours <- attr(col, "nlevels") %||% 256
              colours <- col(min(n_colours, 256))
              gradient_fn <- scales::pal_gradient_n(colours = colours)
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
              n_colours <- attr(col2, "nlevels") %||% 256
              colours <- col2(min(n_colours, 256))
              gradient_fn <- scales::pal_gradient_n(colours = colours)
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
      # Perform darken blend
      .apply_blend_darken(col_vctr, col2_vctr)
    }
  } else if (is.character(col) && is.character(col2)) {
    # Blend the colour vectors directly
    .apply_blend_darken(col, col2)
  } else {
    stop("Arguments must be either character vectors of colours or palette functions")
  }
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

  # Convert to RGB matrices - handle single colour case
  rgb1 <- grDevices::col2rgb(col1, alpha = TRUE)
  if (is.null(dim(rgb1))) rgb1 <- matrix(rgb1, nrow = 4, ncol = 1)
  rgb1 <- rgb1 / 255

  rgb2 <- grDevices::col2rgb(col2, alpha = TRUE)
  if (is.null(dim(rgb2))) rgb2 <- matrix(rgb2, nrow = 4, ncol = 1)
  rgb2 <- rgb2 / 255

  # Extract alpha channels
  alpha1 <- rgb1[4, ]
  alpha2 <- rgb2[4, ]

  # Calculate result alpha using Porter-Duff formula
  # αr = α1 + α2·(1-α1)
  alpha_result <- alpha1 + alpha2 * (1 - alpha1)

  # Initialize result matrix
  n_colours <- ncol(rgb1)
  rgb_result <- matrix(0, nrow = 3, ncol = n_colours)

  for (i in 1:3) {
    # Get colour channel values
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
  grDevices::rgb(rgb_result[1, ], rgb_result[2, ], rgb_result[3, ], alpha_result)
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

  # Convert to RGB matrices - handle single colour case
  rgb1 <- grDevices::col2rgb(col1, alpha = TRUE)
  if (is.null(dim(rgb1)) || length(dim(rgb1)) == 1) {
    rgb1 <- matrix(rgb1, nrow = 4, ncol = 1)
  }
  rgb1 <- rgb1 / 255

  rgb2 <- grDevices::col2rgb(col2, alpha = TRUE)
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
  n_colours <- ncol(rgb1)
  rgb_result <- matrix(0, nrow = 3, ncol = n_colours)

  for (i in 1:3) {
    # Get colour channel values
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
  grDevices::rgb(rgb_result[1, ], rgb_result[2, ], rgb_result[3, ], alpha_result)
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

  # Convert to RGB matrices - handle single colour case
  rgb1 <- grDevices::col2rgb(col1, alpha = TRUE)
  if (is.null(dim(rgb1)) || length(dim(rgb1)) == 1) {
    rgb1 <- matrix(rgb1, nrow = 4, ncol = 1)
  }
  rgb1 <- rgb1 / 255

  rgb2 <- grDevices::col2rgb(col2, alpha = TRUE)
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
  n_colours <- ncol(rgb1)
  rgb_result <- matrix(0, nrow = 3, ncol = n_colours)

  for (i in 1:3) {
    # Get colour channel values
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
  grDevices::rgb(rgb_result[1, ], rgb_result[2, ], rgb_result[3, ], alpha_result)
}

#' Lighten blend colours with proper alpha compositing
#'
#' @description
#' Blends colours using lighten mode with proper alpha handling, selecting
#' the lighter of the two colours for each channel.
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
blend_lighten <- function(...) {
  dots <- list(...)
  # Validate number of arguments
  if (length(dots) == 0) {
    stop("At least one colour argument is required")
  } else if (length(dots) == 1) {
    # Self-lighten (no change)
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
              n_colours <- attr(col, "nlevels") %||% 256
              colours <- col(min(n_colours, 256))
              gradient_fn <- scales::pal_gradient_n(colours = colours)
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
              n_colours <- attr(col2, "nlevels") %||% 256
              colours <- col2(min(n_colours, 256))
              gradient_fn <- scales::pal_gradient_n(colours = colours)
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
      # Perform lighten blend
      .apply_blend_lighten(col_vctr, col2_vctr)
    }
  } else if (is.character(col) && is.character(col2)) {
    # Blend the colour vectors directly
    .apply_blend_lighten(col, col2)
  } else {
    stop("Arguments must be either character vectors of colours or palette functions")
  }
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

  # Convert to RGB matrices - handle single colour case
  rgb1 <- grDevices::col2rgb(col1, alpha = TRUE)
  if (is.null(dim(rgb1))) rgb1 <- matrix(rgb1, nrow = 4, ncol = 1)
  rgb1 <- rgb1 / 255

  rgb2 <- grDevices::col2rgb(col2, alpha = TRUE)
  if (is.null(dim(rgb2))) rgb2 <- matrix(rgb2, nrow = 4, ncol = 1)
  rgb2 <- rgb2 / 255

  # Extract alpha channels
  alpha1 <- rgb1[4, ]
  alpha2 <- rgb2[4, ]

  # Calculate result alpha using Porter-Duff formula
  # αr = α1 + α2·(1-α1)
  alpha_result <- alpha1 + alpha2 * (1 - alpha1)

  # Initialize result matrix
  n_colours <- ncol(rgb1)
  rgb_result <- matrix(0, nrow = 3, ncol = n_colours)

  for (i in 1:3) {
    # Get colour channel values
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
  grDevices::rgb(rgb_result[1, ], rgb_result[2, ], rgb_result[3, ], alpha_result)
}

#' Colour burn blend colours with proper alpha compositing
#'
#' @description
#' Blends colours using colour burn mode with proper alpha handling. Colour burn
#' darkens the destination colour to reflect the source colour, creating
#' intense shadows and rich, dark tones.
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
blend_colour_burn <- function(...) {
  dots <- list(...)
  # Validate number of arguments
  if (length(dots) == 0) {
    stop("At least one colour argument is required")
  } else if (length(dots) == 1) {
    # Self-colour-burn
    col <- dots[[1]]
    col2 <- col
  } else if (length(dots) == 2) {
    # Blend two colours
    col <- dots[[1]]
    col2 <- dots[[2]]
  } else {
    stop("blend_colour_burn accepts at most 2 colour arguments, got ", length(dots))
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
              n_colours <- attr(col, "nlevels") %||% 256
              colours <- col(min(n_colours, 256))
              gradient_fn <- scales::pal_gradient_n(colours = colours)
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
              n_colours <- attr(col2, "nlevels") %||% 256
              colours <- col2(min(n_colours, 256))
              gradient_fn <- scales::pal_gradient_n(colours = colours)
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
      # Perform colour burn blend
      .apply_blend_colour_burn(col_vctr, col2_vctr)
    }
  } else if (is.character(col) && is.character(col2)) {
    # Blend the colour vectors directly
    .apply_blend_colour_burn(col, col2)
  } else {
    stop("Arguments must be either character vectors of colours or palette functions")
  }
}

#' Internal colour burn blend implementation
#'
#' @param col1 Character vector of colours
#' @param col2 Character vector of colours
#' @return Character vector of colour-burn-blended colours
#' @noRd
.apply_blend_colour_burn <- function(col1, col2) {
  # Ensure vectors have compatible lengths
  len <- max(length(col1), length(col2))
  col1 <- rep_len(col1, len)
  col2 <- rep_len(col2, len)

  # Convert to RGB matrices - handle single colour case
  rgb1 <- grDevices::col2rgb(col1, alpha = TRUE)
  if (is.null(dim(rgb1))) rgb1 <- matrix(rgb1, nrow = 4, ncol = 1)
  rgb1 <- rgb1 / 255

  rgb2 <- grDevices::col2rgb(col2, alpha = TRUE)
  if (is.null(dim(rgb2))) rgb2 <- matrix(rgb2, nrow = 4, ncol = 1)
  rgb2 <- rgb2 / 255

  # Extract alpha channels
  alpha1 <- rgb1[4, ]
  alpha2 <- rgb2[4, ]

  # Calculate result alpha using Porter-Duff formula
  # αr = α1 + α2·(1-α1)
  alpha_result <- alpha1 + alpha2 * (1 - alpha1)

  # Initialize result matrix
  n_colours <- ncol(rgb1)
  rgb_result <- matrix(0, nrow = 3, ncol = n_colours)

  for (i in 1:3) {
    # Get colour channel values
    c1 <- rgb1[i, ]
    c2 <- rgb2[i, ]

    # Apply colour burn blend formula
    # f(c1, c2) = 1 - (1 - c2) / c1
    # Return 0 when c1 approaches 0

    colour_burn_blend <- ifelse(
      c1 <= 0,
      0,  # Maximum darkness when source is black
      pmax(0, 1 - (1 - c2) / c1)  # Normal burn calculation, clamped
    )

    # Handle case where result alpha is 0
    idx_valid <- alpha_result > 0

    result <- numeric(length(c1))
    result[idx_valid] <- (
      c1[idx_valid] * alpha1[idx_valid] * (1 - alpha2[idx_valid]) +  # Source only
        c2[idx_valid] * alpha2[idx_valid] * (1 - alpha1[idx_valid]) +  # Dest only
        colour_burn_blend[idx_valid] * alpha1[idx_valid] * alpha2[idx_valid]  # Colour burn blend
    ) / alpha_result[idx_valid]

    # Set fully transparent pixels to 0
    result[!idx_valid] <- 0

    rgb_result[i, ] <- result
  }

  # Clamp values to [0, 1] - preserve matrix structure
  rgb_result[] <- pmax(0, pmin(1, rgb_result))
  alpha_result <- pmax(0, pmin(1, alpha_result))

  # Convert back to hex colours
  grDevices::rgb(rgb_result[1, ], rgb_result[2, ], rgb_result[3, ], alpha_result)
}

#' Colour dodge blend colours with proper alpha compositing
#'
#' @description
#' Blends colours using colour dodge mode with proper alpha handling. Colour dodge
#' brightens the destination colour to reflect the source colour, creating
#' intense highlights and blown-out effects.
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
blend_colour_dodge <- function(...) {
  dots <- list(...)
  # Validate number of arguments
  if (length(dots) == 0) {
    stop("At least one colour argument is required")
  } else if (length(dots) == 1) {
    # Self-colour-dodge
    col <- dots[[1]]
    col2 <- col
  } else if (length(dots) == 2) {
    # Blend two colours
    col <- dots[[1]]
    col2 <- dots[[2]]
  } else {
    stop("blend_colour_dodge accepts at most 2 colour arguments, got ", length(dots))
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
              n_colours <- attr(col, "nlevels") %||% 256
              colours <- col(min(n_colours, 256))
              gradient_fn <- scales::pal_gradient_n(colours = colours)
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
              n_colours <- attr(col2, "nlevels") %||% 256
              colours <- col2(min(n_colours, 256))
              gradient_fn <- scales::pal_gradient_n(colours = colours)
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
      # Perform colour dodge blend
      .apply_blend_colour_dodge(col_vctr, col2_vctr)
    }
  } else if (is.character(col) && is.character(col2)) {
    # Blend the colour vectors directly
    .apply_blend_colour_dodge(col, col2)
  } else {
    stop("Arguments must be either character vectors of colours or palette functions")
  }
}

#' Internal colour dodge blend implementation
#'
#' @param col1 Character vector of colours
#' @param col2 Character vector of colours
#' @return Character vector of colour-dodge-blended colours
#' @noRd
.apply_blend_colour_dodge <- function(col1, col2) {
  # Ensure vectors have compatible lengths
  len <- max(length(col1), length(col2))
  col1 <- rep_len(col1, len)
  col2 <- rep_len(col2, len)

  # Convert to RGB matrices - handle single colour case
  rgb1 <- grDevices::col2rgb(col1, alpha = TRUE)
  if (is.null(dim(rgb1))) rgb1 <- matrix(rgb1, nrow = 4, ncol = 1)
  rgb1 <- rgb1 / 255

  rgb2 <- grDevices::col2rgb(col2, alpha = TRUE)
  if (is.null(dim(rgb2))) rgb2 <- matrix(rgb2, nrow = 4, ncol = 1)
  rgb2 <- rgb2 / 255

  # Extract alpha channels
  alpha1 <- rgb1[4, ]
  alpha2 <- rgb2[4, ]

  # Calculate result alpha using Porter-Duff formula
  # αr = α1 + α2·(1-α1)
  alpha_result <- alpha1 + alpha2 * (1 - alpha1)

  # Initialize result matrix
  n_colours <- ncol(rgb1)
  rgb_result <- matrix(0, nrow = 3, ncol = n_colours)

  for (i in 1:3) {
    # Get colour channel values
    c1 <- rgb1[i, ]
    c2 <- rgb2[i, ]

    # Apply colour dodge blend formula
    # f(c1, c2) = c2 / (1 - c1)
    # Clamp to 1 when c1 approaches 1

    colour_dodge_blend <- ifelse(
      c1 >= 1,
      1,  # Maximum brightness when source is white
      pmin(1, c2 / (1 - c1))  # Normal dodge calculation, clamped
    )

    # Handle case where result alpha is 0
    idx_valid <- alpha_result > 0

    result <- numeric(length(c1))
    result[idx_valid] <- (
      c1[idx_valid] * alpha1[idx_valid] * (1 - alpha2[idx_valid]) +  # Source only
        c2[idx_valid] * alpha2[idx_valid] * (1 - alpha1[idx_valid]) +  # Dest only
        colour_dodge_blend[idx_valid] * alpha1[idx_valid] * alpha2[idx_valid]  # Colour dodge blend
    ) / alpha_result[idx_valid]

    # Set fully transparent pixels to 0
    result[!idx_valid] <- 0

    rgb_result[i, ] <- result
  }

  # Clamp values to [0, 1] - preserve matrix structure
  rgb_result[] <- pmax(0, pmin(1, rgb_result))
  alpha_result <- pmax(0, pmin(1, alpha_result))

  # Convert back to hex colours
  grDevices::rgb(rgb_result[1, ], rgb_result[2, ], rgb_result[3, ], alpha_result)
}

#' Overlay blend colours with proper alpha compositing
#'
#' @description
#' Blends colours using overlay mode with proper alpha handling. Overlay combines
#' multiply and screen modes: multiplies dark colours and screens light colours,
#' increasing contrast while preserving highlights and shadows.
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
blend_overlay <- function(...) {
  dots <- list(...)
  # Validate number of arguments
  if (length(dots) == 0) {
    stop("At least one colour argument is required")
  } else if (length(dots) == 1) {
    # Self-overlay
    col <- dots[[1]]
    col2 <- col
  } else if (length(dots) == 2) {
    # Blend two colours
    col <- dots[[1]]
    col2 <- dots[[2]]
  } else {
    stop("blend_overlay accepts at most 2 colour arguments, got ", length(dots))
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
              n_colours <- attr(col, "nlevels") %||% 256
              colours <- col(min(n_colours, 256))
              gradient_fn <- scales::pal_gradient_n(colours = colours)
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
              n_colours <- attr(col2, "nlevels") %||% 256
              colours <- col2(min(n_colours, 256))
              gradient_fn <- scales::pal_gradient_n(colours = colours)
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
      # Perform overlay blend
      .apply_blend_overlay(col_vctr, col2_vctr)
    }
  } else if (is.character(col) && is.character(col2)) {
    # Blend the colour vectors directly
    .apply_blend_overlay(col, col2)
  } else {
    stop("Arguments must be either character vectors of colours or palette functions")
  }
}

#' Internal overlay blend implementation
#'
#' @param col1 Character vector of colours
#' @param col2 Character vector of colours
#' @return Character vector of overlay-blended colours
#' @noRd
.apply_blend_overlay <- function(col1, col2) {
  # Ensure vectors have compatible lengths
  len <- max(length(col1), length(col2))
  col1 <- rep_len(col1, len)
  col2 <- rep_len(col2, len)

  # Convert to RGB matrices - handle single colour case
  rgb1 <- grDevices::col2rgb(col1, alpha = TRUE)
  if (is.null(dim(rgb1))) rgb1 <- matrix(rgb1, nrow = 4, ncol = 1)
  rgb1 <- rgb1 / 255

  rgb2 <- grDevices::col2rgb(col2, alpha = TRUE)
  if (is.null(dim(rgb2))) rgb2 <- matrix(rgb2, nrow = 4, ncol = 1)
  rgb2 <- rgb2 / 255

  # Extract alpha channels
  alpha1 <- rgb1[4, ]
  alpha2 <- rgb2[4, ]

  # Calculate result alpha using Porter-Duff formula
  # αr = α1 + α2·(1-α1)
  alpha_result <- alpha1 + alpha2 * (1 - alpha1)

  # Initialize result matrix
  n_colours <- ncol(rgb1)
  rgb_result <- matrix(0, nrow = 3, ncol = n_colours)

  for (i in 1:3) {
    # Get colour channel values
    c1 <- rgb1[i, ]
    c2 <- rgb2[i, ]

    # Apply overlay blend formula with proper alpha compositing
    # Overlay formula:
    # if c2 <= 0.5: f(c1, c2) = 2 * c1 * c2 (multiply)
    # if c2 > 0.5:  f(c1, c2) = 1 - 2 * (1 - c1) * (1 - c2) (screen)
    # This is based on the destination colour (c2) determining the blend mode

    # Calculate overlay blend for each pixel
    overlay_blend <- ifelse(
      c2 <= 0.5,
      2 * c1 * c2,  # Multiply for dark colours
      1 - 2 * (1 - c1) * (1 - c2)  # Screen for light colours
    )

    # Handle case where result alpha is 0
    idx_valid <- alpha_result > 0

    result <- numeric(length(c1))
    result[idx_valid] <- (
      c1[idx_valid] * alpha1[idx_valid] * (1 - alpha2[idx_valid]) +  # Source only
        c2[idx_valid] * alpha2[idx_valid] * (1 - alpha1[idx_valid]) +  # Dest only
        overlay_blend[idx_valid] * alpha1[idx_valid] * alpha2[idx_valid]  # Overlay blend
    ) / alpha_result[idx_valid]

    # Set fully transparent pixels to 0
    result[!idx_valid] <- 0

    rgb_result[i, ] <- result
  }

  # Clamp values to [0, 1] - preserve matrix structure
  rgb_result[] <- pmax(0, pmin(1, rgb_result))
  alpha_result <- pmax(0, pmin(1, alpha_result))

  # Convert back to hex colours
  grDevices::rgb(rgb_result[1, ], rgb_result[2, ], rgb_result[3, ], alpha_result)
}

#' Hard light blend colours with proper alpha compositing
#'
#' @description
#' Blends colours using hard light mode with proper alpha handling. Hard light is
#' the inverse of overlay: it combines multiply and screen modes based on the source
#' colour rather than the destination, creating a more intense contrast effect.
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
blend_hard_light <- function(...) {
  dots <- list(...)
  # Validate number of arguments
  if (length(dots) == 0) {
    stop("At least one colour argument is required")
  } else if (length(dots) == 1) {
    # Self-hard-light
    col <- dots[[1]]
    col2 <- col
  } else if (length(dots) == 2) {
    # Blend two colours
    col <- dots[[1]]
    col2 <- dots[[2]]
  } else {
    stop("blend_hard_light accepts at most 2 colour arguments, got ", length(dots))
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
              n_colours <- attr(col, "nlevels") %||% 256
              colours <- col(min(n_colours, 256))
              gradient_fn <- scales::pal_gradient_n(colours = colours)
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
              n_colours <- attr(col2, "nlevels") %||% 256
              colours <- col2(min(n_colours, 256))
              gradient_fn <- scales::pal_gradient_n(colours = colours)
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
      # Perform hard light blend
      .apply_blend_hard_light(col_vctr, col2_vctr)
    }
  } else if (is.character(col) && is.character(col2)) {
    # Blend the colour vectors directly
    .apply_blend_hard_light(col, col2)
  } else {
    stop("Arguments must be either character vectors of colours or palette functions")
  }
}

#' Internal hard light blend implementation
#'
#' @param col1 Character vector of colours
#' @param col2 Character vector of colours
#' @return Character vector of hard-light-blended colours
#' @noRd
.apply_blend_hard_light <- function(col1, col2) {
  # Ensure vectors have compatible lengths
  len <- max(length(col1), length(col2))
  col1 <- rep_len(col1, len)
  col2 <- rep_len(col2, len)

  # Convert to RGB matrices - handle single colour case
  rgb1 <- grDevices::col2rgb(col1, alpha = TRUE)
  if (is.null(dim(rgb1))) rgb1 <- matrix(rgb1, nrow = 4, ncol = 1)
  rgb1 <- rgb1 / 255

  rgb2 <- grDevices::col2rgb(col2, alpha = TRUE)
  if (is.null(dim(rgb2))) rgb2 <- matrix(rgb2, nrow = 4, ncol = 1)
  rgb2 <- rgb2 / 255

  # Extract alpha channels
  alpha1 <- rgb1[4, ]
  alpha2 <- rgb2[4, ]

  # Calculate result alpha using Porter-Duff formula
  # αr = α1 + α2·(1-α1)
  alpha_result <- alpha1 + alpha2 * (1 - alpha1)

  # Initialize result matrix
  n_colours <- ncol(rgb1)
  rgb_result <- matrix(0, nrow = 3, ncol = n_colours)

  for (i in 1:3) {
    # Get colour channel values
    c1 <- rgb1[i, ]
    c2 <- rgb2[i, ]

    # Apply hard light blend formula with proper alpha compositing
    # Hard light formula (opposite of overlay):
    # if c1 <= 0.5: f(c1, c2) = 2 * c1 * c2 (multiply)
    # if c1 > 0.5:  f(c1, c2) = 1 - 2 * (1 - c1) * (1 - c2) (screen)
    # This is based on the source colour (c1) determining the blend mode

    # Calculate hard light blend for each pixel
    hard_light_blend <- ifelse(
      c1 <= 0.5,
      2 * c1 * c2,  # Multiply for dark source colours
      1 - 2 * (1 - c1) * (1 - c2)  # Screen for light source colours
    )

    # Handle case where result alpha is 0
    idx_valid <- alpha_result > 0

    result <- numeric(length(c1))
    result[idx_valid] <- (
      c1[idx_valid] * alpha1[idx_valid] * (1 - alpha2[idx_valid]) +  # Source only
        c2[idx_valid] * alpha2[idx_valid] * (1 - alpha1[idx_valid]) +  # Dest only
        hard_light_blend[idx_valid] * alpha1[idx_valid] * alpha2[idx_valid]  # Hard light blend
    ) / alpha_result[idx_valid]

    # Set fully transparent pixels to 0
    result[!idx_valid] <- 0

    rgb_result[i, ] <- result
  }

  # Clamp values to [0, 1] - preserve matrix structure
  rgb_result[] <- pmax(0, pmin(1, rgb_result))
  alpha_result <- pmax(0, pmin(1, alpha_result))

  # Convert back to hex colours
  grDevices::rgb(rgb_result[1, ], rgb_result[2, ], rgb_result[3, ], alpha_result)
}

#' Soft light blend colours with proper alpha compositing
#'
#' @description
#' Blends colours using soft light mode with proper alpha handling. Soft light is
#' a gentler version of overlay that produces a more subtle contrast enhancement.
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
blend_soft_light <- function(...) {
  dots <- list(...)
  # Validate number of arguments
  if (length(dots) == 0) {
    stop("At least one colour argument is required")
  } else if (length(dots) == 1) {
    # Self-soft-light
    col <- dots[[1]]
    col2 <- col
  } else if (length(dots) == 2) {
    # Blend two colours
    col <- dots[[1]]
    col2 <- dots[[2]]
  } else {
    stop("blend_soft_light accepts at most 2 colour arguments, got ", length(dots))
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
              n_colours <- attr(col, "nlevels") %||% 256
              colours <- col(min(n_colours, 256))
              gradient_fn <- scales::pal_gradient_n(colours = colours)
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
              n_colours <- attr(col2, "nlevels") %||% 256
              colours <- col2(min(n_colours, 256))
              gradient_fn <- scales::pal_gradient_n(colours = colours)
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
      # Perform soft light blend
      .apply_blend_soft_light(col_vctr, col2_vctr)
    }
  } else if (is.character(col) && is.character(col2)) {
    # Blend the colour vectors directly
    .apply_blend_soft_light(col, col2)
  } else {
    stop("Arguments must be either character vectors of colours or palette functions")
  }
}

#' Internal soft light blend implementation
#'
#' @param col1 Character vector of colours
#' @param col2 Character vector of colours
#' @return Character vector of soft-light-blended colours
#' @noRd
.apply_blend_soft_light <- function(col1, col2) {
  # Ensure vectors have compatible lengths
  len <- max(length(col1), length(col2))
  col1 <- rep_len(col1, len)
  col2 <- rep_len(col2, len)

  # Convert to RGB matrices - handle single colour case
  rgb1 <- grDevices::col2rgb(col1, alpha = TRUE)
  if (is.null(dim(rgb1))) rgb1 <- matrix(rgb1, nrow = 4, ncol = 1)
  rgb1 <- rgb1 / 255

  rgb2 <- grDevices::col2rgb(col2, alpha = TRUE)
  if (is.null(dim(rgb2))) rgb2 <- matrix(rgb2, nrow = 4, ncol = 1)
  rgb2 <- rgb2 / 255

  # Extract alpha channels
  alpha1 <- rgb1[4, ]
  alpha2 <- rgb2[4, ]

  # Calculate result alpha using Porter-Duff formula
  # αr = α1 + α2·(1-α1)
  alpha_result <- alpha1 + alpha2 * (1 - alpha1)

  # Initialize result matrix
  n_colours <- ncol(rgb1)
  rgb_result <- matrix(0, nrow = 3, ncol = n_colours)

  for (i in 1:3) {
    # Get colour channel values
    c1 <- rgb1[i, ]
    c2 <- rgb2[i, ]

    # Apply soft light blend formula (Cairo/Photoshop version)
    # if c1 <= 0.5: f(c1, c2) = c2 - (1 - 2*c1) * c2 * (1 - c2)
    # if c1 > 0.5:  f(c1, c2) = c2 + (2*c1 - 1) * (D(c2) - c2)
    # where D(x) = ((16*x - 12)*x + 4)*x for x <= 0.25, sqrt(x) for x > 0.25

    # Helper function D(x)
    D <- function(x) {
      ifelse(x <= 0.25,
             ((16 * x - 12) * x + 4) * x,
             sqrt(x))
    }

    # Calculate soft light blend for each pixel
    soft_light_blend <- ifelse(
      c1 <= 0.5,
      c2 - (1 - 2 * c1) * c2 * (1 - c2),  # Darken
      c2 + (2 * c1 - 1) * (D(c2) - c2)     # Lighten
    )

    # Handle case where result alpha is 0
    idx_valid <- alpha_result > 0

    result <- numeric(length(c1))
    result[idx_valid] <- (
      c1[idx_valid] * alpha1[idx_valid] * (1 - alpha2[idx_valid]) +  # Source only
        c2[idx_valid] * alpha2[idx_valid] * (1 - alpha1[idx_valid]) +  # Dest only
        soft_light_blend[idx_valid] * alpha1[idx_valid] * alpha2[idx_valid]  # Soft light blend
    ) / alpha_result[idx_valid]

    # Set fully transparent pixels to 0
    result[!idx_valid] <- 0

    rgb_result[i, ] <- result
  }

  # Clamp values to [0, 1] - preserve matrix structure
  rgb_result[] <- pmax(0, pmin(1, rgb_result))
  alpha_result <- pmax(0, pmin(1, alpha_result))

  # Convert back to hex colours
  grDevices::rgb(rgb_result[1, ], rgb_result[2, ], rgb_result[3, ], alpha_result)
}

#' Difference blend colours with proper alpha compositing
#'
#' @description
#' Blends colours using difference mode with proper alpha handling. Difference
#' creates an inversion effect based on the difference between colours, useful
#' for comparing colours or creating psychedelic effects.
#'
#' @param ... Either one or two colour arguments:
#'   - If one argument: the colour is blended with itself (returns black)
#'   - If two arguments: the first is blended with the second
#'   Each argument can be a character vector of colours or a `scales::pal_*()` function
#'
#' @return
#' If inputs are character vectors, returns a character vector of blended colours.
#' If any input is a function, returns a function that generates blended colours.
#'
#' @export
blend_difference <- function(...) {
  dots <- list(...)
  # Validate number of arguments
  if (length(dots) == 0) {
    stop("At least one colour argument is required")
  } else if (length(dots) == 1) {
    # Self-difference (always produces black/dark)
    col <- dots[[1]]
    col2 <- col
  } else if (length(dots) == 2) {
    # Blend two colours
    col <- dots[[1]]
    col2 <- dots[[2]]
  } else {
    stop("blend_difference accepts at most 2 colour arguments, got ", length(dots))
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
              n_colours <- attr(col, "nlevels") %||% 256
              colours <- col(min(n_colours, 256))
              gradient_fn <- scales::pal_gradient_n(colours = colours)
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
              n_colours <- attr(col2, "nlevels") %||% 256
              colours <- col2(min(n_colours, 256))
              gradient_fn <- scales::pal_gradient_n(colours = colours)
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
      # Perform difference blend
      .apply_blend_difference(col_vctr, col2_vctr)
    }
  } else if (is.character(col) && is.character(col2)) {
    # Blend the colour vectors directly
    .apply_blend_difference(col, col2)
  } else {
    stop("Arguments must be either character vectors of colours or palette functions")
  }
}

#' Internal difference blend implementation
#'
#' @param col1 Character vector of colours
#' @param col2 Character vector of colours
#' @return Character vector of difference-blended colours
#' @noRd
.apply_blend_difference <- function(col1, col2) {
  # Ensure vectors have compatible lengths
  len <- max(length(col1), length(col2))
  col1 <- rep_len(col1, len)
  col2 <- rep_len(col2, len)

  # Convert to RGB matrices - handle single colour case
  rgb1 <- grDevices::col2rgb(col1, alpha = TRUE)
  if (is.null(dim(rgb1))) rgb1 <- matrix(rgb1, nrow = 4, ncol = 1)
  rgb1 <- rgb1 / 255

  rgb2 <- grDevices::col2rgb(col2, alpha = TRUE)
  if (is.null(dim(rgb2))) rgb2 <- matrix(rgb2, nrow = 4, ncol = 1)
  rgb2 <- rgb2 / 255

  # Extract alpha channels
  alpha1 <- rgb1[4, ]
  alpha2 <- rgb2[4, ]

  # Calculate result alpha using Porter-Duff formula
  # αr = α1 + α2·(1-α1)
  alpha_result <- alpha1 + alpha2 * (1 - alpha1)

  # Initialize result matrix
  n_colours <- ncol(rgb1)
  rgb_result <- matrix(0, nrow = 3, ncol = n_colours)

  for (i in 1:3) {
    # Get colour channel values
    c1 <- rgb1[i, ]
    c2 <- rgb2[i, ]

    # Apply difference blend formula
    # f(c1, c2) = abs(c1 - c2)
    difference_blend <- abs(c1 - c2)

    # Handle case where result alpha is 0
    idx_valid <- alpha_result > 0

    result <- numeric(length(c1))
    result[idx_valid] <- (
      c1[idx_valid] * alpha1[idx_valid] * (1 - alpha2[idx_valid]) +  # Source only
        c2[idx_valid] * alpha2[idx_valid] * (1 - alpha1[idx_valid]) +  # Dest only
        difference_blend[idx_valid] * alpha1[idx_valid] * alpha2[idx_valid]  # Difference blend
    ) / alpha_result[idx_valid]

    # Set fully transparent pixels to 0
    result[!idx_valid] <- 0

    rgb_result[i, ] <- result
  }

  # Clamp values to [0, 1] - preserve matrix structure
  rgb_result[] <- pmax(0, pmin(1, rgb_result))
  alpha_result <- pmax(0, pmin(1, alpha_result))

  # Convert back to hex colours
  grDevices::rgb(rgb_result[1, ], rgb_result[2, ], rgb_result[3, ], alpha_result)
}

#' Exclusion blend colours with proper alpha compositing
#'
#' @description
#' Blends colours using exclusion mode with proper alpha handling. Exclusion
#' creates a lower-contrast inversion effect similar to difference but softer,
#' useful for subtle colour comparisons.
#'
#' @param ... Either one or two colour arguments:
#'   - If one argument: the colour is blended with itself (returns grey)
#'   - If two arguments: the first is blended with the second
#'   Each argument can be a character vector of colours or a `scales::pal_*()` function
#'
#' @return
#' If inputs are character vectors, returns a character vector of blended colours.
#' If any input is a function, returns a function that generates blended colours.
#'
#' @export
blend_exclusion <- function(...) {
  dots <- list(...)
  # Validate number of arguments
  if (length(dots) == 0) {
    stop("At least one colour argument is required")
  } else if (length(dots) == 1) {
    # Self-exclusion (produces grey)
    col <- dots[[1]]
    col2 <- col
  } else if (length(dots) == 2) {
    # Blend two colours
    col <- dots[[1]]
    col2 <- dots[[2]]
  } else {
    stop("blend_exclusion accepts at most 2 colour arguments, got ", length(dots))
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
              n_colours <- attr(col, "nlevels") %||% 256
              colours <- col(min(n_colours, 256))
              gradient_fn <- scales::pal_gradient_n(colours = colours)
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
              n_colours <- attr(col2, "nlevels") %||% 256
              colours <- col2(min(n_colours, 256))
              gradient_fn <- scales::pal_gradient_n(colours = colours)
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
      # Perform exclusion blend
      .apply_blend_exclusion(col_vctr, col2_vctr)
    }
  } else if (is.character(col) && is.character(col2)) {
    # Blend the colour vectors directly
    .apply_blend_exclusion(col, col2)
  } else {
    stop("Arguments must be either character vectors of colours or palette functions")
  }
}

#' Internal exclusion blend implementation
#'
#' @param col1 Character vector of colours
#' @param col2 Character vector of colours
#' @return Character vector of exclusion-blended colours
#' @noRd
.apply_blend_exclusion <- function(col1, col2) {
  # Ensure vectors have compatible lengths
  len <- max(length(col1), length(col2))
  col1 <- rep_len(col1, len)
  col2 <- rep_len(col2, len)

  # Convert to RGB matrices - handle single colour case
  rgb1 <- grDevices::col2rgb(col1, alpha = TRUE)
  if (is.null(dim(rgb1))) rgb1 <- matrix(rgb1, nrow = 4, ncol = 1)
  rgb1 <- rgb1 / 255

  rgb2 <- grDevices::col2rgb(col2, alpha = TRUE)
  if (is.null(dim(rgb2))) rgb2 <- matrix(rgb2, nrow = 4, ncol = 1)
  rgb2 <- rgb2 / 255

  # Extract alpha channels
  alpha1 <- rgb1[4, ]
  alpha2 <- rgb2[4, ]

  # Calculate result alpha using Porter-Duff formula
  # αr = α1 + α2·(1-α1)
  alpha_result <- alpha1 + alpha2 * (1 - alpha1)

  # Initialize result matrix
  n_colours <- ncol(rgb1)
  rgb_result <- matrix(0, nrow = 3, ncol = n_colours)

  for (i in 1:3) {
    # Get colour channel values
    c1 <- rgb1[i, ]
    c2 <- rgb2[i, ]

    # Apply exclusion blend formula
    # f(c1, c2) = c1 + c2 - 2*c1*c2
    # This is like difference but with lower contrast
    exclusion_blend <- c1 + c2 - 2 * c1 * c2

    # Handle case where result alpha is 0
    idx_valid <- alpha_result > 0

    result <- numeric(length(c1))
    result[idx_valid] <- (
      c1[idx_valid] * alpha1[idx_valid] * (1 - alpha2[idx_valid]) +  # Source only
        c2[idx_valid] * alpha2[idx_valid] * (1 - alpha1[idx_valid]) +  # Dest only
        exclusion_blend[idx_valid] * alpha1[idx_valid] * alpha2[idx_valid]  # Exclusion blend
    ) / alpha_result[idx_valid]

    # Set fully transparent pixels to 0
    result[!idx_valid] <- 0

    rgb_result[i, ] <- result
  }

  # Clamp values to [0, 1] - preserve matrix structure
  rgb_result[] <- pmax(0, pmin(1, rgb_result))
  alpha_result <- pmax(0, pmin(1, alpha_result))

  # Convert back to hex colours
  grDevices::rgb(rgb_result[1, ], rgb_result[2, ], rgb_result[3, ], alpha_result)
}
