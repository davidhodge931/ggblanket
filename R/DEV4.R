#' #' Automatically Orient Palette Direction for Panel Contrast
#' #'
#' #' @description
#' #' Orients a palette to maximize contrast with the ggplot2 panel background.
#' #' Compares the luminance of palette endpoints with the panel and ensures
#' #' maximum contrast is placed at high values (default) or low values (reversed).
#' #' Returns the palette in the same format as provided.
#' #'
#' #' @param palette A palette in any format:
#' #'   - A function that takes n (e.g., `scales::pal_viridis()`)
#' #'   - A function that takes values 0-1 (e.g., `scales::pal_gradient_n()`)
#' #'   - A character vector of colors
#' #'   - Other color objects
#' #' @param ... Additional arguments (for extensibility)
#' #' @param reverse Logical. If FALSE (default), maximum contrast is placed
#' #'   at high values. If TRUE, maximum contrast is placed at low values.
#' #'
#' #' @return The palette in the same format as input, oriented for optimal contrast
#' #' @export
#' #'
#' #' @examples
#' #' # Vector in, vector out
#' #' pal_direction(c("white", "red"))  # Returns c("white", "red") or c("red", "white")
#' #' pal_direction(viridis::mako(256))  # Returns 256 colors, possibly reversed
#' #'
#' #' # Function in, function out
#' #' pal_direction(scales::pal_viridis())  # Returns adapted function
#' #'
#' #' # Single color unchanged
#' #' pal_direction("darkred")  # Returns "darkred"
#' #'
#' #' # Reverse for maximum contrast at low values
#' #' pal_direction(c("white", "red"), reverse = TRUE)
#' pal_direction <- function(palette, ..., reverse = FALSE) {
#'   # Force evaluation
#'   force(palette)
#'   force(reverse)
#'
#'   # Handle functions
#'   if (is.function(palette)) {
#'     # Return a wrapped function that applies direction at call time
#'     if (inherits(palette, "pal_discrete")) {
#'       # n-based function
#'       return(function(n) {
#'         colors <- palette(n)
#'         .apply_direction(colors, reverse)
#'       })
#'     } else if (inherits(palette, "pal_continuous")) {
#'       # value-based function
#'       return(function(x) {
#'         colors <- palette(x)
#'         .apply_direction(colors, reverse)
#'       })
#'     } else {
#'       # Unknown function type - return wrapped version that tries to detect
#'       return(function(x) {
#'         # Try to get colors from the palette
#'         colors <- tryCatch(
#'           palette(x),
#'           error = function(e) {
#'             # If x is a single number, might be n-based, try as values
#'             if (length(x) == 1 && is.numeric(x)) {
#'               palette(seq(0, 1, length.out = x))
#'             } else {
#'               stop(e)
#'             }
#'           }
#'         )
#'         .apply_direction(colors, reverse)
#'       })
#'     }
#'   }
#'
#'   # Handle non-functions (vectors)
#'   colors <- as.character(palette)
#'
#'   # Single color: return unchanged
#'   if (length(colors) == 1) {
#'     return(palette)  # Return in original format
#'   }
#'
#'   # Multiple colors: apply direction
#'   oriented_colors <- .apply_direction(colors, reverse)
#'
#'   # Return in same format as input
#'   if (inherits(palette, "character")) {
#'     return(oriented_colors)
#'   } else {
#'     # Preserve original class if possible
#'     tryCatch(
#'       structure(oriented_colors, class = class(palette)),
#'       error = function(e) oriented_colors
#'     )
#'   }
#' }
#'
#' #' Apply direction logic to a color vector
#' #' @noRd
#' .apply_direction <- function(colors, reverse = FALSE) {
#'   # Single color or empty: return unchanged
#'   if (length(colors) <= 1) {
#'     return(colors)
#'   }
#'
#'   # Get panel luminance
#'   panel_luminance <- .get_panel_luminance()
#'
#'   # Get luminance of first and last colors
#'   first_lum <- .get_color_luminance(colors[1])
#'   last_lum <- .get_color_luminance(colors[length(colors)])
#'
#'   # Calculate contrast with panel
#'   first_contrast <- abs(first_lum - panel_luminance)
#'   last_contrast <- abs(last_lum - panel_luminance)
#'
#'   # Determine if reversal is needed
#'   needs_reversal <- if (!reverse) {
#'     # Want maximum contrast at high end (default)
#'     last_contrast < first_contrast
#'   } else {
#'     # Want maximum contrast at low end
#'     first_contrast < last_contrast
#'   }
#'
#'   if (needs_reversal) {
#'     rev(colors)
#'   } else {
#'     colors
#'   }
#' }
#'
#' # ============================================================================
#' # HELPER FUNCTIONS
#' # ============================================================================
#'
#' #' Check if theme panel background is light
#' #'
#' #' @param theme A ggplot2 theme object. If NULL, uses current theme.
#' #' @return TRUE if light (luminance > 50), FALSE if dark
#' #' @export
#' is_panel_light <- function(theme = NULL) {
#'   .get_panel_luminance(theme) > 50
#' }
#'
#' #' Check if theme panel background is dark
#' #'
#' #' @param theme A ggplot2 theme object. If NULL, uses current theme.
#' #' @return TRUE if dark (luminance <= 50), FALSE if light
#' #' @export
#' is_panel_dark <- function(theme = NULL) {
#'   .get_panel_luminance(theme) <= 50
#' }
#'
#' # ============================================================================
#' # INTERNAL HELPERS
#' # ============================================================================
#'
#' #' Get panel background luminance
#' #' @noRd
#' .get_panel_luminance <- function(theme = NULL) {
#'   if (rlang::is_null(theme)) {
#'     theme <- ggplot2::theme_get()
#'   }
#'
#'   # Get panel background colour
#'   panel_col <- ggplot2::calc_element(theme = theme, element = "panel.background")$fill
#'
#'   # If panel is transparent/NA, check plot background
#'   if (.is_transparent_or_na(panel_col)) {
#'     plot_col <- ggplot2::calc_element(theme = theme, element = "plot.background")$fill
#'
#'     if (.is_transparent_or_na(plot_col)) {
#'       return(100)  # Default to light
#'     }
#'
#'     panel_col <- plot_col
#'   }
#'
#'   .get_color_luminance(panel_col)
#' }
#'
#' #' Get color luminance
#' #' @noRd
#' .get_color_luminance <- function(col) {
#'   if (.is_transparent_or_na(col)) {
#'     return(100)  # Default to light
#'   }
#'
#'   tryCatch({
#'     farver::get_channel(colour = col, channel = "l", space = "hcl")
#'   }, error = function(e) {
#'     100  # Default to light if error
#'   })
#' }
#'
#' #' Check if a color value is transparent or NA
#' #' @noRd
#' .is_transparent_or_na <- function(col) {
#'   rlang::is_null(col) ||
#'     length(col) == 0 ||
#'     is.na(col) ||
#'     identical(col, "transparent") ||
#'     identical(col, NA_character_) ||
#'     (is.character(col) && tolower(col) == "transparent")
#' }
