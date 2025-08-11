#' #' Direct palette by panel contrast
#' #'
#' #' @description
#' #' Directs a continuous palette such to maximise the contrast of high values against the panel background.
#' #' Or low values if `reverse = TRUE`.
#' #'
#' #' @param palette A palette in any format:
#' #'   - A function that takes n (e.g., `scales::pal_viridis()`)
#' #'   - A function that takes values 0-1 (e.g., `scales::pal_gradient_n()`)
#' #'   - A character vector of colours
#' #'   - Other colour objects
#' #' @param ... Additional arguments (for extensibility)
#' #' @param reverse Logical. If FALSE (default), maximum contrast is placed
#' #'   at high values. If TRUE, maximum contrast is placed at low values.
#' #'
#' #' @return A palette function that takes n and returns n colours
#' #' @export
#' #'
#' #' @examples
#' #' # Maximum contrast at high values (default)
#' #' pal_direction(c("white", "red"))
#' #' pal_direction(viridis::mako(256))
#' #' pal_direction(scales::pal_viridis())
#' #'
#' #' # Maximum contrast at low values
#' #' pal_direction(c("white", "red"), reverse = TRUE)
#' pal_direction <- function(palette, ..., reverse = FALSE) {
#'
#'   # Force evaluation
#'   force(palette)
#'   force(reverse)
#'
#'   # Return a function that determines direction at call time
#'   function(n) {
#'     # Get panel luminance
#'     panel_luminance <- .get_panel_luminance()
#'
#'     # Get colors from palette based on its type
#'     if (is.function(palette)) {
#'       # Check if it's a discrete (n-based) or continuous (value-based) function
#'       if (inherits(palette, "pal_discrete")) {
#'         colors <- palette(n)
#'       } else if (inherits(palette, "pal_continuous")) {
#'         colors <- palette(seq(0, 1, length.out = n))
#'       } else {
#'         # Try both approaches
#'         colors <- tryCatch(
#'           palette(n),
#'           error = function(e) {
#'             palette(seq(0, 1, length.out = n))
#'           }
#'         )
#'       }
#'     } else {
#'       # Convert to character vector
#'       colors <- as.character(palette)
#'
#'       # Handle different lengths
#'       if (length(colors) == 1) {
#'         # Single color: create gradient to maximize contrast
#'         if (!reverse) {
#'           # High values get the specified color
#'           if (panel_luminance > 50) {
#'             # Light panel: gradient from white to color
#'             colors <- scales::seq_gradient_pal("white", colors)(seq(0, 1, length.out = n))
#'           } else {
#'             # Dark panel: gradient from black to color
#'             colors <- scales::seq_gradient_pal("black", colors)(seq(0, 1, length.out = n))
#'           }
#'         } else {
#'           # Low values get the specified color
#'           if (panel_luminance > 50) {
#'             # Light panel: gradient from color to white
#'             colors <- scales::seq_gradient_pal(colors, "white")(seq(0, 1, length.out = n))
#'           } else {
#'             # Dark panel: gradient from color to black
#'             colors <- scales::seq_gradient_pal(colors, "black")(seq(0, 1, length.out = n))
#'           }
#'         }
#'         return(colors)
#'       } else if (length(colors) != n) {
#'         # Interpolate to get n colors
#'         colors <- scales::colour_ramp(colors)(seq(0, 1, length.out = n))
#'       }
#'     }
#'
#'     # For multi-color palettes, determine best direction
#'     if (length(colors) > 1) {
#'       # Get luminance of first and last colors
#'       first_lum <- .get_color_luminance(colors[1])
#'       last_lum <- .get_color_luminance(colors[length(colors)])
#'
#'       # Calculate contrast with panel
#'       first_contrast <- abs(first_lum - panel_luminance)
#'       last_contrast <- abs(last_lum - panel_luminance)
#'
#'       if (!reverse) {
#'         # Want maximum contrast at high end (default)
#'         if (last_contrast < first_contrast) {
#'           # First has more contrast, reverse to put it at the end
#'           colors <- rev(colors)
#'         }
#'       } else {
#'         # Want maximum contrast at low end
#'         if (first_contrast < last_contrast) {
#'           # Last has more contrast, reverse to put it at the start
#'           colors <- rev(colors)
#'         }
#'       }
#'     }
#'
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
