#' Get a dark/light colour for contrast
#'
#' @description Get a dark/light colour based on contrast.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param col A vector of colours from which to determine a contrast vector of light/dark colours.
#' @param dark A dark colour. If NULL, derived from theme text or panel background.
#' @param light A light colour. If NULL, derived from theme text or panel background.
#'
#' @return A character vector of colours, the same length as the `col` vector, containing either
#'         the dark or light colour determined for contrast.
#'
#' @noRd
#'
#' @examples
#' get_contrast(col = c("#000000", "#FFFFFF", "#808080"))  # Uses theme colours
#' get_contrast(col = c("navy", "yellow", "orange"), dark = "navy", light = "lightblue")
#'
get_contrast <- function(..., col, dark = NULL, light = NULL) {
  # Only get theme if we need it
  if (rlang::is_null(dark) || rlang::is_null(light)) {
    # Get current theme
    current_theme <- ggplot2::get_theme()

    # Get text colour from theme
    theme_text <- current_theme$axis.text.x$colour %||%
      current_theme$axis.text.y$colour %||%
      current_theme$axis.text$colour %||%
      current_theme$text$colour %||%
      "black"

    # Get panel background from theme
    theme_panel <- current_theme$panel.background$fill %||%
      "white"

    # Determine which is dark and which is light using is_col_dark
    if (is_col_dark(theme_text)) {
      # Dark text theme (light mode)
      dark <- dark %||% theme_text
      light <- light %||% theme_panel
    } else {
      # Light text theme (dark mode)
      dark <- dark %||% theme_panel
      light <- light %||% theme_text
    }
  }

  # Use is_col_dark to determine which colour to return
  ifelse(!is_col_dark(col), dark, light)
}

#' Modify an aesthetic for contrast
#'
#' @description Modifies a mapped colour (or fill) aesthetic for contrast against the fill (or colour) aesthetic.
#'
#' Function can be spliced into [ggplot2::aes] with [rlang::!!!].
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param dark A dark colour. If NULL, derived from theme text or panel background.
#' @param light A light colour. If NULL, derived from theme text or panel background.
#' @param aesthetic The aesthetic to be modified for contrast. Either "colour" (default) or "fill".
#'
#' @return A ggplot2 aesthetic in [ggplot2::aes].
#'
#' @seealso
#' \code{\link[rlang]{splice}}
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(stringr)
#'
#' set_blanket(
#'   col_palette_d = jumble,
#' )
#'
#' p <- palmerpenguins::penguins |>
#'   count(species, sex) |>
#'   mutate(sex = str_to_sentence(sex)) |>
#'   gg_col(
#'     x = sex,
#'     y = n,
#'     col = species,
#'     label = n,
#'     position = position_dodge(preserve = "single"),
#'     width = 0.75,
#'   )
#'
#' p +
#'   geom_text(
#'     mapping = aes_contrast(),
#'     position = position_dodge(width = 0.75, preserve = "single"),
#'     vjust = 1.33,
#'     show.legend = FALSE,
#'   )
#'
aes_contrast <- function(..., dark = NULL, light = NULL, aesthetic = "colour") {
  # Only get theme if we need it
  if (rlang::is_null(dark) || rlang::is_null(light)) {
    # Get current theme
    current_theme <- ggplot2::get_theme()

    # Get text colour from theme
    theme_text <- current_theme$axis.text.x$colour %||%
      current_theme$axis.text.y$colour %||%
      current_theme$axis.text$colour %||%
      current_theme$text$colour %||%
      "black"

    # Get panel background from theme
    theme_panel <- current_theme$panel.background$fill %||%
      "white"

    # Determine which is dark and which is light using is_col_dark
    if (is_col_dark(theme_text)) {
      # Dark text theme (light mode)
      dark <- dark %||% theme_text
      light <- light %||% theme_panel
    } else {
      # Light text theme (dark mode)
      dark <- dark %||% theme_panel
      light <- light %||% theme_text
    }
  }

  if (aesthetic == "colour") {
    ggplot2::aes(
      colour = ggplot2::after_scale(
        get_contrast(.data$fill, dark = dark, light = light)
      )
    )
  } else if (aesthetic == "fill") {
    ggplot2::aes(
      fill = ggplot2::after_scale(
        get_contrast(.data$colour, dark = dark, light = light)
      )
    )
  } else {
    rlang::abort("aesthetic must be either 'colour' or 'fill'")
  }
}
