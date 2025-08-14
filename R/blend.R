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

