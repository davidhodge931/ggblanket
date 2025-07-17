#' #' Get a dark/light colour for contrast
#' #'
#' #' @description Get a dark/light colour based on contrast with fill colours.
#' #'              When dark/light colours are not provided, intelligently derives them from
#' #'              the current ggplot2 theme by examining text and panel background colours.
#' #'              colours are automatically desaturated to ensure readability.
#' #'
#' #' @param fill A fill aesthetic from which to determine the colour scale for contrast.
#' #' @param ... Provided to require argument naming, support trailing commas etc.
#' #' @param dark A dark colour. If NULL, derived from theme text or panel background
#' #'             (whichever is darker), with saturation removed.
#' #' @param light A light colour. If NULL, derived from theme text or panel background
#' #'              (whichever is lighter), with saturation removed.
#' #'
#' #' @return A character vector of colours, the same length as `fill`, containing either
#' #'         the dark or light colour based on the luminance of each fill value.
#' #'
#' #' @details
#' #' The function calculates the luminance of each fill colour and returns:
#' #' - The `dark` colour when fill luminance > 50 (light fills)
#' #' - The `light` colour when fill luminance <= 50 (dark fills)
#' #'
#' #' When deriving colours from the theme, the function:
#' #' 1. Extracts text colour (from axis.text.x, axis.text.y, axis.text, or text)
#' #' 2. Extracts panel background colour
#' #' 3. Desaturates both colours to remove any colourful tints
#' #' 4. Assigns the darker one to `dark` and lighter one to `light`
#' #'
#' #' This ensures readable contrast even when themes use colourful base colours.
#' #'
#' #' @noRd
#' #'
#' #' @examples
#' #' get_contrast(fill = c("navy", "yellow", "orange"), dark = "black", light = "white")
#' #' get_contrast(fill = c("#000000", "#FFFFFF", "#808080"))  # Uses theme colours
#' #'
#' get_contrast <- function(fill, ..., dark = NULL, light = NULL) {
#'   # Only get theme if we need it
#'   if (is.null(dark) || is.null(light)) {
#'     # Get current theme
#'     current_theme <- ggplot2::get_theme()
#'
#'     # Get text colour from theme
#'     theme_text <- current_theme$axis.text.x$colour %||%
#'       current_theme$axis.text.y$colour %||%
#'       current_theme$axis.text$colour %||%
#'       current_theme$text$colour %||%
#'       "black"
#'
#'     # Get panel background from theme
#'     theme_panel <- current_theme$panel.background$fill %||%
#'       "white"
#'
#'     # Determine which is dark and which is light based on luminance
#'     text_luminance <- farver::get_channel(
#'       colour = theme_text,
#'       channel = "l",
#'       space = "hcl"
#'     )
#'
#'     if (text_luminance < 50) {
#'       # Dark text theme (light mode)
#'       dark <- dark %||% theme_text
#'       light <- light %||% theme_panel
#'     } else {
#'       # Light text theme (dark mode)
#'       dark <- dark %||% theme_panel
#'       light <- light %||% theme_text
#'     }
#'   }
#'
#'   ifelse(
#'     farver::get_channel(
#'       colour = fill,
#'       channel = "l",
#'       space = "hcl"
#'     ) > 50,
#'     dark,
#'     light
#'   )
#' }
#'
#' #' A colour aesthetic for contrast
#' #'
#' #' @description A colour aesthetic to contrast with a fill aesthetic. Can be spliced
#' #'              into [ggplot2::aes] with [rlang::!!!]. This function creates text/label
#' #'              colours that are readable against varying fill backgrounds by automatically
#' #'              selecting dark or light colours based on fill luminance.
#' #'
#' #' @param ... Provided to require argument naming, support trailing commas etc.
#' #' @param dark A dark colour. If NULL, derived from the current theme by using
#' #'             either the theme's text colour or panel background (whichever is darker),
#' #'             with saturation removed for better contrast.
#' #' @param light A light colour. If NULL, derived from the current theme by using
#' #'              either the theme's text colour or panel background (whichever is lighter),
#' #'              with saturation removed for better contrast.
#' #'
#' #' @return A ggplot2 aesthetic that can be used directly in geom_text(), geom_label(),
#' #'         or similar geoms. Uses `after_scale()` to calculate appropriate contrast
#' #'         colours based on the fill aesthetic.
#' #'
#' #' @details
#' #' This function is designed to be used with text/label geoms to ensure readability.
#' #' It works by:
#' #' 1. Accessing the fill aesthetic using `after_scale()`
#' #' 2. Calculating the luminance of each fill value
#' #' 3. Selecting either the dark or light colour based on contrast needs
#' #'
#' #' The function intelligently adapts to both light and dark themes by examining
#' #' the current theme's text and panel background colours. Any colourful tints
#' #' (e.g., from base_colour) are removed to ensure neutral contrast colours.
#' #'
#' #' @seealso
#' #' \code{\link[ggplot2]{aes}}, \code{\link[ggplot2]{after_scale}},
#' #' \code{\link[rlang]{splice}}
#' #'
#' #' @export
#' #'
#' #' @examples
#' #' library(ggplot2)
#' #' library(dplyr)
#' #' library(stringr)
#' #'
#' #' set_blanket()
#' #'
#' #' palmerpenguins::penguins |>
#' #'   count(species, sex) |>
#' #'   mutate(sex = str_to_sentence(sex)) |>
#' #'   gg_col(
#' #'     x = sex,
#' #'     y = n,
#' #'     col = species,
#' #'     label = n,
#' #'     position = position_dodge(preserve = "single"),
#' #'     width = 0.75,
#' #'   ) +
#' #'   geom_text(
#' #'     mapping = aes_contrast(),
#' #'     position = position_dodge(width = 0.75, preserve = "single"),
#' #'     vjust = 1.33,
#' #'     show.legend = FALSE,
#' #'   )
#' aes_contrast <- function(..., dark = NULL, light = NULL) {
#'   # Only get theme if we need it
#'   if (is.null(dark) || is.null(light)) {
#'     # Get current theme
#'     current_theme <- ggplot2::get_theme()
#'
#'     # Get text colour from theme
#'     theme_text <- current_theme$axis.text.x$colour %||%
#'       current_theme$axis.text.y$colour %||%
#'       current_theme$axis.text$colour %||%
#'       current_theme$text$colour %||%
#'       "black"
#'
#'     # Get panel background from theme
#'     theme_panel <- current_theme$panel.background$fill %||%
#'       "white"
#'
#'     # Determine which is dark and which is light based on luminance
#'     text_luminance <- farver::get_channel(
#'       colour = theme_text,
#'       channel = "l",
#'       space = "hcl"
#'     )
#'
#'     if (text_luminance < 50) {
#'       # Dark text theme (light mode)
#'       dark <- dark %||% theme_text
#'       light <- light %||% theme_panel
#'     } else {
#'       # Light text theme (dark mode)
#'       dark <- dark %||% theme_panel
#'       light <- light %||% theme_text
#'     }
#'   }
#'
#'   ggplot2::aes(
#'     colour = ggplot2::after_scale(
#'       get_contrast(.data$fill, dark = dark, light = light)
#'     )
#'   )
#' }
