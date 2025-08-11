#' #' Adapt Palette Direction by Panel Background
#' #'
#' #' @description
#' #' Automatically adjusts palette direction based on the ggplot2 panel background
#' #' to maximize contrast. Works with any palette input: functions that take n,
#' #' functions that take values, character vectors, or other color objects.
#' #'
#' #' The function ensures that the specified end of the scale (high or low values)
#' #' has maximum contrast with the panel background. On light panels, dark colors
#' #' are placed at the specified end; on dark panels, light colors are placed there.
#' #'
#' #' @param palette A palette in any of these formats:
#' #'   - A function that takes n (e.g., `scales::pal_viridis()`)
#' #'   - A function that takes values 0-1 (e.g., `scales::pal_gradient_n()`)
#' #'   - A character vector of colors (e.g., `c("white", "red")`)
#' #'   - Other color objects (colors, palettes_colour, etc.)
#' #' @param ... Additional arguments (for extensibility)
#' #' @param max_contrast Which end of the scale should have maximum contrast with
#' #'   the panel. Either "high" (default) or "low".
#' #'
#' #' @return A palette function that takes n and returns n colors, automatically
#' #'   oriented for optimal contrast with the current theme's panel background.
#' #'
#' #' @export
#' #'
#' #' @examples
#' #' # Works with any palette type
#' #' pal <- adapt_by_panel(scales::pal_viridis())
#' #' pal <- adapt_by_panel(c("white", "darkblue"))
#' #' pal <- adapt_by_panel(viridis::mako(256))
#' #'
#' #' # Control which end has maximum contrast
#' #' pal <- adapt_by_panel(viridis::plasma(100), max_contrast = "low")
#' #'
#' #' # Use in ggblanket
#' #' gg_blanket(
#' #'   data = df,
#' #'   geom = "point",
#' #'   col_palette = adapt_by_panel(scales::pal_viridis())
#' #' )
#' adapt_by_panel <- function(palette, ..., max_contrast = "high") {
#'   # Validate max_contrast
#'   if (!max_contrast %in% c("high", "low")) {
#'     stop("max_contrast must be either 'high' or 'low'")
#'   }
#'
#'   # Force evaluation
#'   force(palette)
#'   force(max_contrast)
#'
#'   # Return a function that determines direction at call time
#'   function(n) {
#'     # Determine panel background
#'     panel_is_light <- is_panel_light()
#'
#'     # Get colors from palette based on its type
#'     if (is.function(palette)) {
#'       # Check if it's a discrete (n-based) or continuous (value-based) function
#'       if (inherits(palette, "pal_discrete")) {
#'         # Discrete palette - takes n
#'         colors <- palette(n)
#'       } else if (inherits(palette, "pal_continuous")) {
#'         # Continuous palette - takes values
#'         colors <- palette(seq(0, 1, length.out = n))
#'       } else {
#'         # Unknown function type - try both approaches
#'         colors <- tryCatch(
#'           palette(n),
#'           error = function(e) {
#'             palette(seq(0, 1, length.out = n))
#'           }
#'         )
#'       }
#'     } else {
#'       # Not a function - convert to character vector
#'       colors <- as.character(palette)
#'
#'       # Handle different lengths
#'       if (length(colors) == 1) {
#'         # Single color: create gradient
#'         if (max_contrast == "high") {
#'           # High values should contrast with panel
#'           if (panel_is_light) {
#'             # Light panel: gradient from light to the color
#'             colors <- scales::seq_gradient_pal("white", colors)(seq(0, 1, length.out = n))
#'           } else {
#'             # Dark panel: gradient from dark to the color
#'             colors <- scales::seq_gradient_pal("black", colors)(seq(0, 1, length.out = n))
#'           }
#'         } else {
#'           # Low values should contrast with panel
#'           if (panel_is_light) {
#'             # Light panel: gradient from the color to light
#'             colors <- scales::seq_gradient_pal(colors, "white")(seq(0, 1, length.out = n))
#'           } else {
#'             # Dark panel: gradient from the color to dark
#'             colors <- scales::seq_gradient_pal(colors, "black")(seq(0, 1, length.out = n))
#'           }
#'         }
#'         return(colors)
#'       } else if (length(colors) != n) {
#'         # Multiple colors but not exact count - interpolate
#'         colors <- scales::colour_ramp(colors)(seq(0, 1, length.out = n))
#'       }
#'       # Else: exact number of colors provided, use as is
#'     }
#'
#'     # Determine if reversal is needed based on contrast requirements
#'     if (length(colors) > 1) {
#'       # Check luminance of first and last colors
#'       first_luminance <- tryCatch({
#'         farver::get_channel(colors[1], channel = "l", space = "hcl")
#'       }, error = function(e) 50)
#'
#'       last_luminance <- tryCatch({
#'         farver::get_channel(colors[length(colors)], channel = "l", space = "hcl")
#'       }, error = function(e) 50)
#'
#'       if (max_contrast == "high") {
#'         # High values should contrast with panel
#'         if (panel_is_light) {
#'           # Light panel: want darker colors at high end
#'           # If first is darker than last, reverse
#'           if (first_luminance < last_luminance) {
#'             colors <- rev(colors)
#'           }
#'         } else {
#'           # Dark panel: want lighter colors at high end
#'           # If first is lighter than last, reverse
#'           if (first_luminance > last_luminance) {
#'             colors <- rev(colors)
#'           }
#'         }
#'       } else {
#'         # Low values should contrast with panel
#'         if (panel_is_light) {
#'           # Light panel: want darker colors at low end
#'           # If last is darker than first, reverse
#'           if (last_luminance < first_luminance) {
#'             colors <- rev(colors)
#'           }
#'         } else {
#'           # Dark panel: want lighter colors at low end
#'           # If last is lighter than first, reverse
#'           if (last_luminance > first_luminance) {
#'             colors <- rev(colors)
#'           }
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
#'       return(TRUE)  # Default to light
#'     }
#'
#'     return(.is_col_light(plot_col))
#'   }
#'
#'   .is_col_light(panel_col)
#' }
#'
#' #' Check if theme panel background is dark
#' #'
#' #' @param theme A ggplot2 theme object. If NULL, uses current theme.
#' #' @return TRUE if dark (luminance <= 50), FALSE if light
#' #' @export
#' is_panel_dark <- function(theme = NULL) {
#'   !is_panel_light(theme)
#' }
#'
#' # ============================================================================
#' # INTERNAL HELPERS
#' # ============================================================================
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
#'
#' #' Check if a colour is dark
#' #' @noRd
#' .is_col_dark <- function(col) {
#'   if (.is_transparent_or_na(col)) {
#'     return(FALSE)
#'   }
#'
#'   col_luminance <- tryCatch({
#'     farver::get_channel(colour = col, channel = "l", space = "hcl")
#'   }, error = function(e) {
#'     100  # Default to light if error
#'   })
#'
#'   # Adjusted threshold - red (~53) should be considered dark
#'   col_luminance <= 55
#' }
#'
#' #' Check if a colour is light
#' #' @noRd
#' .is_col_light <- function(col) {
#'   if (.is_transparent_or_na(col)) {
#'     return(TRUE)
#'   }
#'
#'   col_luminance <- tryCatch({
#'     farver::get_channel(colour = col, channel = "l", space = "hcl")
#'   }, error = function(e) {
#'     100  # Default to light if error
#'   })
#'
#'   # Adjusted threshold
#'   col_luminance > 55
#' }
