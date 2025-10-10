#' Automatically Orient Palette Direction for Panel Contrast
#'
#' @description
#' Orients a palette to maximize contrast with the ggplot2 panel background.
#' Compares the luminance of palette endpoints with the panel and ensures
#' maximum contrast is placed at high values (default) or low values (reversed).
#' Returns the palette in the same format as provided.
#'
#' @param palette A palette in any format:
#'   - A function that takes n (e.g., `scales::pal_viridis()`)
#'   - A function that takes values 0-1 (e.g., `scales::pal_gradient_n()`)
#'   - A character vector of colours
#'   - Other colour objects
#' @param ... Additional arguments (for extensibility)
#' @param reverse Logical. If FALSE (default), maximum contrast is placed
#'   at high values. If TRUE, maximum contrast is placed at low values.
#'
#' @return The palette in the same format as input, oriented for optimal contrast
#' @export
direction_contrast <- function(palette, ..., reverse = FALSE) {
  # Force evaluation
  force(palette)
  force(reverse)

  # Handle functions
  if (is.function(palette)) {
    # Capture all attributes from the original function
    original_attrs <- attributes(palette)

    # Return a wrapped function that applies direction at call time
    if (inherits(palette, "pal_discrete")) {
      # n-based function
      wrapped_fn <- function(x) {
        colours <- palette(x)
        .apply_direction(colours, reverse)
      }
    } else if (inherits(palette, "pal_continuous")) {
      # value-based function
      wrapped_fn <- function(x) {
        colours <- palette(x)
        .apply_direction(colours, reverse)
      }
    } else {
      # Unknown function type - return wrapped version that tries to detect
      wrapped_fn <- function(x) {
        # Try to get colours from the palette
        colours <- tryCatch(
          palette(x),
          error = function(e) {
            # If x is a single number, might be n-based, try as values
            if (length(x) == 1 && is.numeric(x)) {
              palette(seq(0, 1, length.out = x))
            } else {
              stop(e)
            }
          }
        )
        .apply_direction(colours, reverse)
      }
    }

    # CRITICAL: Preserve ALL attributes from the original function
    # This includes class, type, nlevels, etc.
    attributes(wrapped_fn) <- original_attrs

    # Add a marker that this has been wrapped with direction
    attr(wrapped_fn, "direction_wrapped") <- TRUE

    return(wrapped_fn)
  }

  # Handle non-functions (vectors)
  colours <- as.character(palette)

  # Single colour: return unchanged
  if (length(colours) == 1) {
    return(palette)  # Return in original format
  }

  # Multiple colours: apply direction
  oriented_colours <- .apply_direction(colours, reverse)

  # Return in same format as input
  if (inherits(palette, "character")) {
    return(oriented_colours)
  } else {
    # Preserve original class if possible
    tryCatch(
      structure(oriented_colours, class = class(palette)),
      error = function(e) oriented_colours
    )
  }
}

#' Apply direction logic to a colour vector
#' @noRd
.apply_direction <- function(colours, reverse = FALSE) {
  # Single colour or empty: return unchanged
  if (length(colours) <= 1) {
    return(colours)
  }

  # Get panel luminance
  panel_luminance <- .get_panel_luminance()

  # Get luminance of first and last colours
  first_lum <- .get_colour_luminance(colours[1])
  last_lum <- .get_colour_luminance(colours[length(colours)])

  # Calculate contrast with panel
  first_contrast <- abs(first_lum - panel_luminance)
  last_contrast <- abs(last_lum - panel_luminance)

  # Determine if reversal is needed
  needs_reversal <- if (!reverse) {
    # Want maximum contrast at high end (default)
    last_contrast < first_contrast
  } else {
    # Want maximum contrast at low end
    first_contrast < last_contrast
  }

  if (needs_reversal) {
    rev(colours)
  } else {
    colours
  }
}

#' Get panel background luminance
#' @noRd
.get_panel_luminance <- function(theme = NULL) {
  if (rlang::is_null(theme)) {
    theme <- ggplot2::theme_get()
  }

  # Get panel background colour
  panel_col <- ggplot2::calc_element(theme = theme, element = "panel.background")@fill

  # If panel is transparent/NA, check plot background
  if (.is_transparent_or_na(panel_col)) {
    plot_col <- ggplot2::calc_element(theme = theme, element = "plot.background")@fill

    if (.is_transparent_or_na(plot_col)) {
      return(100)  # Default to light
    }

    panel_col <- plot_col
  }

  `.get_colour_luminance`(panel_col)
}

#' Get colour luminance
#' @noRd
.get_colour_luminance <- function(col) {
  if (.is_transparent_or_na(col)) {
    return(100)  # Default to light
  }

  tryCatch({
    farver::get_channel(colour = col, channel = "l", space = "hcl")
  }, error = function(e) {
    100  # Default to light if error
  })
}

#' Check if a colour value is transparent or NA
#' @noRd
.is_transparent_or_na <- function(col) {
  rlang::is_null(col) ||
    length(col) == 0 ||
    is.na(col) ||
    identical(col, "transparent") ||
    identical(col, NA_character_) ||
    (is.character(col) && tolower(col) == "transparent")
}

#' Check if theme panel background is dark
#'
#' @description
#' Determines whether the current ggplot2 theme has a dark or light panel background
#' by examining its luminance.
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
  col <- ggplot2::calc_element(theme = theme, element = "panel.background")@fill

  # Use .is_col_dark to check if the panel colour is dark
  is_col_dark(col)
}

#' Check if a colour is dark
#'
#' @description
#' Determines whether a colour is dark by examining its luminance value.
#'
#' @param col A colour value. Can be a hex code, colour name, or any format
#'        accepted by farver. If NULL, returns FALSE.
#'
#' @return TRUE if dark (luminance <= 50) and FALSE otherwise.
#'
#' @export
#'
#' @examples
#' is_col_dark("#0095A8FF")
#'
is_col_dark <- function(col) {
  # Handle NULL or missing input
  if (rlang::is_null(col) || length(col) == 0) {
    return(FALSE)
  }

  # Calculate luminance of the colour
  col_luminance <- farver::get_channel(
    colour = col,
    channel = "l",
    space = "hcl"
  )

  # Return TRUE if low luminance
  col_luminance <= 50
}
