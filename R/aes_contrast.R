#' Get contrast
#'
#' @param fill A fill aesthetic from which to determine the colour scale for contrast.
#' @param dark A dark colour. Defaults to `"black"`.
#' @param light A light colour. Defaults to `"white"`.
#'
#' @noRd
#'
#' @examples
#' get_contrast(fill = c("navy", "yellow", "orange"), dark = "black", light = "white")
#'
get_contrast <- function(fill, dark = "black", light = "white") {
  out <- rep(dark, length(fill))
  channel <- farver::get_channel(fill, "l", space = "hcl")
  out[channel < 50] <- light
  out
}

#' An auto-contrast colour aesthetic
#'
#' @description A colour aesthetic for annotation that automatically contrasts with fill. Can be spliced into [ggplot2::aes] with [rlang::!!!].
#'
#' @param mode_family The mode family to optimise light and dark colours for. Options are "light", "grey" or "dark".
#' @param dark A dark colour. If NULL, uses `mode_family` optimised colour.
#' @param light A light colour. If NULL, uses `mode_family` optimised colour.
#'
#' @return An aesthetic
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(stringr)
#' library(palmerpenguins)
#'
#' set_blanket()
#'
#' penguins |>
#'   count(species, sex) |>
#'   gg_col(
#'     x = sex,
#'     y = n,
#'     col = species,
#'     position = position_dodge2(preserve = "single"),
#'     width = 0.75,
#'     x_labels = \(x) str_to_sentence(x),
#'   ) +
#'   geom_text(
#'     mapping = aes(label = n, !!!aes_contrast()),
#'     position = position_dodge2(width = 0.75, preserve = "single"),
#'     vjust = 1.33,
#'     show.legend = FALSE,
#'   )
#'
#' penguins |>
#'   count(species, sex) |>
#'   gg_col(
#'     x = sex,
#'     y = n,
#'     col = species,
#'     position = position_dodge2(preserve = "single"),
#'     width = 0.75,
#'     x_labels = \(x) str_to_sentence(x),
#'     mode = dark_mode_r(),
#'   ) +
#'   geom_text(
#'     mapping = aes(label = n, !!!aes_contrast("dark")),
#'     position = position_dodge2(width = 0.75, preserve = "single"),
#'     vjust = 1.33,
#'     show.legend = FALSE,
#'   )
aes_contrast <- function(mode_family = "light", dark = NULL, light = NULL) {

  if (mode_family == "light") {
    if (rlang::is_null(dark)) dark <- "#121B24FF"
    if (rlang::is_null(light)) light <- "#FFFFFFFF"
  }
  else if (mode_family == "grey") {
    if (rlang::is_null(dark)) dark <- "#121B24FF"
    if (rlang::is_null(light)) light <- "#F6F8FAFF"
  }
  else if (mode_family == "dark") {
    if (rlang::is_null(dark)) dark <- "#050D1BFF"
    if (rlang::is_null(light)) light <- "#C8D7DFFF"
  }

  ggplot2::aes(
    colour = ggplot2::after_scale(
      get_contrast(.data$fill, dark = dark, light = light)
    )
  )
}
