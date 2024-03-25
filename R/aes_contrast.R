#' Contrast internal function
#'
#' @param col The fill or colour aesthetic.
#' @param annotate_pal A vector of a dark colour and then a light colour (e.g. `greyness[2:3]` or `darkness[1:2]`). Defaults to `lightness[2:3]`.
#'
#' @noRd
contrast <- function(col, annotate_pal = lightness[2:3]) {

  out <- rep(annotate_pal[1], length(col))
  light <- farver::get_channel(col, "l", space = "hcl")
  out[light < 50] <- annotate_pal[2]
  out
}

#' A colour aesthetic that automatically contrasts with fill.
#'
#' @description A colour aesthetic for annotation that automatically contrasts with fill. Can be spliced into [ggplot2::aes] with [rlang::!!!].
#'
#' @param annotate_pal A vector of a dark colour and then a light colour (e.g. `greyness[2:3]` or `darkness[1:2]`). Defaults to `lightness[2:3]`.
#'
#' @return An aesthetic
#' @export
#'
#' @examples
#' library(palmerpenguins)
#' library(dplyr)
#' library(ggplot2)
#' library(stringr)
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
#'     x = n,
#'     y = sex,
#'     col = species,
#'     position = position_dodge2(preserve = "single"),
#'     width = 0.75,
#'     y_labels = \(x) str_to_sentence(x),
#'     mode = dark_mode_r(),
#'   ) +
#'   geom_text(
#'     mapping = aes(label = n, !!!aes_contrast(darkness[1:2])),
#'     position = position_dodge2(width = 0.75, preserve = "single"),
#'     hjust = 1.25,
#'     show.legend = FALSE,
#'   )
aes_contrast <- function(annotate_pal = lightness[2:3]) {

  ggplot2::aes(
    colour = ggplot2::after_scale(
      contrast(.data$fill, annotate_pal = annotate_pal[1:2])
      )
    )
}
