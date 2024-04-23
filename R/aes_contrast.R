#' Get contrast
#'
#' @param fill A fill aesthetic from which to determine the colour scale for contrast.
#' @param dark A dark colour. Defaults to `"#121b24"`.
#' @param light A light colour. Defaults to `"#ffffff"`.
#'
#' @noRd
#'
#' @examples
#' get_contrast(fill = c("navy", "yellow", "orange"), dark = "black", light = "white")
#'
get_contrast <- function(fill, dark = "#121b24", light = "#ffffff") {
  out <- rep(dark, length(fill))
  channel <- farver::get_channel(fill, "l", space = "hcl")
  out[channel < 50] <- light
  out
}

#' A colour aesthetic that automatically contrasts with fill.
#'
#' @description A colour aesthetic for annotation that automatically contrasts with fill. Can be spliced into [ggplot2::aes] with [rlang::!!!].
#'
#' @param dark A dark colour. Defaults to `"#121b24"`.
#' @param light A light colour. Defaults to `"#ffffff"`.
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
#'     label = n,
#'     position = position_dodge2(preserve = "single"),
#'     width = 0.75,
#'     x_labels = \(x) str_to_sentence(x),
#'   ) +
#'   geom_text(
#'     mapping = aes_contrast(),
#'     # mapping = aes(!!!aes_contrast()),
#'     position = position_dodge2(width = 0.75, preserve = "single"),
#'     vjust = 1.33,
#'     show.legend = FALSE,
#'   )
#'
aes_contrast <- function(dark = "#121b24", light = "#ffffff") {

  ggplot2::aes(
    colour = ggplot2::after_scale(
      get_contrast(.data$fill, dark = dark, light = light)
      )
    )
}
