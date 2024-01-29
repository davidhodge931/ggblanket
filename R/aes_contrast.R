#' Contrast internal function
#'
#' @param col The fill or colour aesthetic.
#' @param col_pal_dark A dark colour used for text on light polygons.
#' @param col_pal_light A light colour used for text on dark polygons.
#'
#' @keywords internal
#'
#' @references Based on code by Teun van den Brand
contrast <- function(col,
                     col_pal_dark = "black",
                     col_pal_light = "white") {

  out <- rep(col_pal_dark, length(col))
  light <- farver::get_channel(col, "l", space = "hcl")
  out[light < 50] <- col_pal_light
  out
}

#' An automatically contrasting colour aesthetic
#'
#' @description A colour aesthetic that automatically contrasts with fill.
#'
#' @param theme_family The ggblanket theme family default colours. Either "light_mode" or "dark_mode".
#' @param col_pal_dark A dark colour for use on light fill.
#' @param col_pal_light A light colour for use on dark fill.
#'
#' @return An aesthetic
#' @export
#'
#' @examples
#' library(palmerpenguins)
#' library(dplyr)
#' library(ggplot2)
#'
#' penguins |>
#'   count(species, sex) |>
#'   gg_col(
#'     x = sex,
#'     y = n,
#'     col = species,
#'     position = position_dodge2(preserve = "single"),
#'     width = 0.75,
#'     x_labels = \(x) stringr::str_to_sentence(x),
#'   ) +
#'   geom_text(
#'     mapping = aes(y = n - (max(n * 0.04)), label = n, !!!aes_contrast("light_mode")),
#'     position = position_dodge2(width = 0.75, preserve = "single"),
#'     show.legend = FALSE,
#'   )
aes_contrast <- function(theme_family = NULL, col_pal_dark = NULL, col_pal_light = NULL) {

  if (!rlang::is_null(theme_family)) {
    if (theme_family == "light_mode") {
      col_pal_dark <- pal_light_mode["text"]
      col_pal_light <- pal_light_mode["panel"]
    }
    else if (theme_family == "dark_mode") {
      col_pal_dark <- pal_dark_mode["plot"]
      col_pal_light <- pal_dark_mode["text"]
    }
  }
  else {
    if (rlang::is_null(col_pal_dark)) col_pal_dark <- "black"
    if (rlang::is_null(col_pal_light)) col_pal_light <- "white"
  }

  ggplot2::aes(
    colour = ggplot2::after_scale(
      contrast(.data$fill,
               col_pal_dark = col_pal_dark,
               col_pal_light = col_pal_light)
    )
  )
}

