# ============================================================================
# INTERNAL HELPER FUNCTIONS (start with .)
# ============================================================================

#' Check if a color value is transparent or NA
#'
#' @param col A colour value to check
#' @return TRUE if the color is transparent, NA, or NULL
#' @noRd
.is_transparent_or_na <- function(col) {
  rlang::is_null(col) ||
    length(col) == 0 ||
    is.na(col) ||
    identical(col, "transparent") ||
    identical(col, NA_character_) ||
    (is.character(col) && tolower(col) == "transparent")
}

#' Check if a colour is dark
#'
#' @param col A colour value
#' @return TRUE if dark (luminance <= 50) and FALSE otherwise
#' @noRd
.is_col_dark <- function(col) {
  # Handle NULL, NA, transparent, or missing input
  if (.is_transparent_or_na(col)) {
    return(FALSE)  # Default to light for transparent/missing colors
  }

  # Try to calculate luminance, with error handling
  col_luminance <- tryCatch({
    farver::get_channel(
      colour = col,
      channel = "l",
      space = "hcl"
    )
  }, error = function(e) {
    warning(paste("Could not parse color:", col, "- defaulting to light"))
    return(100)  # Return high luminance (light)
  })

  # Return TRUE if dark (low luminance), FALSE if light
  col_luminance <= 50
}

#' Check if a colour is light
#'
#' @param col A colour value
#' @return TRUE if light (luminance > 50) and FALSE otherwise
#' @noRd
.is_col_light <- function(col) {
  # Handle NULL, NA, transparent, or missing input
  if (.is_transparent_or_na(col)) {
    return(TRUE)  # Default to light for transparent/missing colors
  }

  # Try to calculate luminance, with error handling
  col_luminance <- tryCatch({
    farver::get_channel(
      colour = col,
      channel = "l",
      space = "hcl"
    )
  }, error = function(e) {
    warning(paste("Could not parse color:", col, "- defaulting to light"))
    return(100)  # Return high luminance (light)
  })

  # Return TRUE if light (high luminance), FALSE if dark
  col_luminance > 50
}

#' Check if palette string is in paletteer format
#'
#' @param x A string to check
#' @return TRUE if string is in "package::palette" format
#' @noRd
.is_paletteer_string <- function(x) {
  is.character(x) &&
    length(x) == 1 &&
    stringr::str_detect(x, stringr::fixed("::"))
}

# ============================================================================
# EXPORTED PANEL DETECTION FUNCTIONS
# ============================================================================

#' Check if theme panel background is dark
#'
#' @description
#' Determines whether the current ggplot2 theme has a dark or light panel background
#' by examining its luminance. Falls back to plot background if panel is transparent.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param theme A ggplot2 theme object. If NULL (default), uses the current theme
#'        from `ggplot2::theme_get()`.
#'
#' @return TRUE if dark (luminance <= 50) and FALSE otherwise.
#'
#' @export
is_panel_dark <- function(..., theme = NULL) {
  # Get theme if not provided
  if (rlang::is_null(theme)) {
    theme <- ggplot2::theme_get()
  }

  # Get panel background colour from theme
  panel_col <- ggplot2::calc_element(theme = theme, element = "panel.background")$fill

  # If panel is transparent/NA, check plot background as fallback
  if (.is_transparent_or_na(panel_col)) {
    plot_col <- ggplot2::calc_element(theme = theme, element = "plot.background")$fill

    # If plot background is also transparent/NA, assume light (most common default)
    if (.is_transparent_or_na(plot_col)) {
      return(FALSE)  # Default to light
    }

    return(.is_col_dark(plot_col))
  }

  # Use .is_col_dark to check if the panel colour is dark
  .is_col_dark(panel_col)
}

#' Check if theme panel background is light
#'
#' @description
#' Determines whether the current ggplot2 theme has a light panel background
#' by examining its luminance. Falls back to plot background if panel is transparent.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param theme A ggplot2 theme object. If NULL (default), uses the current theme
#'        from `ggplot2::theme_get()`.
#'
#' @return TRUE if light (luminance > 50) and FALSE otherwise.
#'
#' @export
is_panel_light <- function(..., theme = NULL) {
  # Get theme if not provided
  if (rlang::is_null(theme)) {
    theme <- ggplot2::theme_get()
  }

  # Get panel background colour from theme
  panel_col <- ggplot2::calc_element(theme = theme, element = "panel.background")$fill

  # If panel is transparent/NA, check plot background as fallback
  if (.is_transparent_or_na(panel_col)) {
    plot_col <- ggplot2::calc_element(theme = theme, element = "plot.background")$fill

    # If plot background is also transparent/NA, assume light (most common default)
    if (.is_transparent_or_na(plot_col)) {
      return(TRUE)  # Default to light
    }

    return(.is_col_light(plot_col))
  }

  # Use .is_col_light to check if the panel colour is light
  .is_col_light(panel_col)
}

# ============================================================================
# MAIN ADAPTIVE PALETTE FUNCTION
# ============================================================================

#' Generic Adaptive Palette Function with Optional Paletteer Support
#'
#' @description
#' Returns a palette function that automatically adjusts direction based on
#' panel background luminance to maximize contrast. Works with any continuous
#' color palette. If paletteer is installed, also supports paletteer syntax.
#'
#' @param palette Either:
#'   - A string in paletteer format: "package::palette" (requires paletteer)
#'   - A palette function that takes `n` as its first argument
#'   - A character vector of colors to interpolate
#'   - A single color name/code (will create gradient)
#' @param ... Additional arguments passed to the palette function
#' @param rev Logical. If `TRUE`, reverses the behavior of the direction
#'   adjustment based on panel background. Default is `FALSE`.
#' @param interpolate Logical. If `TRUE` and palette is a vector of colors,
#'   interpolate between them. Default is `TRUE`.
#'
#' @return A function that takes a single argument `n` and returns a vector
#'   of `n` colors
#' @export
#'
#' @examples
#' # With palette functions (no paletteer needed)
#' pal <- pal_adaptive(scales::viridis_pal())
#' pal(5)
#'
#' # With color vectors (no paletteer needed)
#' pal <- pal_adaptive(c("#f0f0f0", "#0095A8FF"))
#'
#' \dontrun{
#' # With paletteer syntax (requires paletteer package)
#' pal <- pal_adaptive("scico::berlin")
#' pal(5)
#' }
pal_adaptive <- function(
    palette,
    ...,
    rev = FALSE,
    interpolate = TRUE
) {
  # Capture the arguments in the closure
  force(palette)
  force(rev)
  force(interpolate)
  dots <- list(...)

  # Return a function that determines direction at call time
  function(n) {
    # Determine if we should reverse based on panel
    should_reverse <- if (!rev) {
      is_panel_light()
    } else {
      !is_panel_light()
    }

    # Handle different palette types
    if (.is_paletteer_string(palette)) {
      # Check if paletteer is available
      if (!requireNamespace("paletteer", quietly = TRUE)) {
        stop("Package 'paletteer' is required to use palette string '", palette, "'.\n",
             "Please install it with: install.packages('paletteer')")
      }

      # Parse paletteer string
      palette_parts <- stringr::str_split(palette, stringr::fixed("::"))[[1]]

      if (length(palette_parts) != 2) {
        stop("Palette string must be in format 'package::palette'")
      }

      # Try continuous palettes first, then discrete if that fails
      colors <- tryCatch({
        # Try continuous palette
        paletteer::paletteer_c(palette, n = n, direction = 1)
      }, error = function(e) {
        # Try discrete palette with interpolation
        tryCatch({
          discrete_colors <- paletteer::paletteer_d(palette)
          if (length(discrete_colors) == n) {
            discrete_colors
          } else if (interpolate) {
            scales::colour_ramp(discrete_colors)(seq(0, 1, length.out = n))
          } else {
            stop("Discrete palette has ", length(discrete_colors),
                 " colors but n = ", n, ". Set interpolate = TRUE.")
          }
        }, error = function(e2) {
          stop("Could not find palette '", palette, "' in paletteer")
        })
      })

    } else if (is.function(palette)) {
      # Palette is a function - call it with n and any extra args
      colors <- do.call(palette, c(list(n), dots))

    } else if (is.character(palette)) {
      if (length(palette) == 1 && !stringr::str_detect(palette, stringr::fixed("::"))) {
        # Single color: create gradient from white/black to this color
        # The specified color should always be at the "high" end
        if (should_reverse) {
          # Light background: reverse so high values are dark
          colors <- scales::seq_gradient_pal("white", palette)(seq(0, 1, length.out = n))
        } else {
          # Dark background: normal direction, high values are the specified color
          colors <- scales::seq_gradient_pal("black", palette)(seq(0, 1, length.out = n))
        }
        # Return early as we've already handled reversal
        return(colors)
      } else if (interpolate && length(palette) != n) {
        # Multiple colors: interpolate if needed
        colors <- scales::colour_ramp(palette)(seq(0, 1, length.out = n))
      } else if (length(palette) == n) {
        # Exact number of colors provided
        colors <- palette
      } else {
        # Not interpolating and wrong number of colors
        stop("Palette has ", length(palette), " colors but n = ", n,
             ". Set interpolate = TRUE or provide exactly n colors.")
      }
    } else {
      stop("Palette must be a function or character vector of colors")
    }

    # For multi-color palettes, check if we need to reverse based on first/last color luminance
    if (should_reverse && length(colors) > 1) {
      # On light background, we want dark colors at the high end
      # Check if the last color is lighter than the first
      first_is_dark <- .is_col_dark(colors[1])
      last_is_dark <- .is_col_dark(colors[length(colors)])

      # If last color is lighter than first (wrong direction for light background), reverse
      if (first_is_dark && !last_is_dark) {
        colors <- rev(colors)
      }
    } else if (!should_reverse && length(colors) > 1) {
      # On dark background, we want light colors at the high end
      # Check if the last color is darker than the first
      first_is_dark <- .is_col_dark(colors[1])
      last_is_dark <- .is_col_dark(colors[length(colors)])

      # If last color is darker than first (wrong direction for dark background), reverse
      if (!first_is_dark && last_is_dark) {
        colors <- rev(colors)
      }
    }

    colors
  }
}

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Check if Paletteer is Available
#'
#' @description
#' Helper function to check if paletteer package is installed.
#'
#' @return Logical indicating if paletteer is available
#' @export
#'
#' @examples
#' has_paletteer()
has_paletteer <- function() {
  requireNamespace("paletteer", quietly = TRUE)
}

#' Show Adaptive Palette Support Status
#'
#' @description
#' Shows which palette formats are currently available based on installed packages.
#'
#' @return Invisibly returns a list of available formats
#' @export
#'
#' @examples
#' show_palette_support()
show_palette_support <- function() {
  cat("Adaptive Palette Support Status:\n")
  cat("================================\n\n")

  cat("✓ Always available:\n")
  cat("  - Palette functions (e.g., scales::viridis_pal())\n")
  cat("  - Color vectors (e.g., c('#FF0000', '#0000FF'))\n")
  cat("  - Single colors (e.g., 'darkred')\n")
  cat("  - Built-in R palettes via scales\n\n")

  if (has_paletteer()) {
    cat("✓ Paletteer support: ENABLED\n")
    cat("  - Use palettes like 'scico::berlin'\n")
    cat("  - Access to 2000+ palettes from 50+ packages\n")

    # Show counts if paletteer is available
    if (requireNamespace("paletteer", quietly = TRUE)) {
      n_continuous <- nrow(paletteer::palettes_c_names)
      n_discrete <- nrow(paletteer::palettes_d_names)
      cat(sprintf("  - %d continuous palettes available\n", n_continuous))
      cat(sprintf("  - %d discrete palettes available\n", n_discrete))
    }
  } else {
    cat("○ Paletteer support: NOT AVAILABLE\n")
    cat("  - Install with: install.packages('paletteer')\n")
    cat("  - This enables syntax like 'scico::berlin'\n")
  }

  invisible(list(
    base = TRUE,
    paletteer = has_paletteer()
  ))
}
