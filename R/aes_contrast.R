#' Contrast internal function
#'
#' @param col The fill or colour aesthetic.
#' @param col_pal A vector of a dark colour and then a light colour. Defaults to `c("black", "white")`.
#'
#' @noRd
contrast <- function(col, col_pal = c("black", "white")) {

  out <- rep(col_pal[1], length(col))
  light <- farver::get_channel(col, "l", space = "hcl")
  out[light < 50] <- col_pal[2]
  out
}

#' A colour aesthetic that automatically contrasts with fill.
#'
#' @description A colour aesthetic that automatically contrasts with fill. Can be spliced into [ggplot2::aes] with [rlang::!!!].
#'
#' @param col_pal A vector of a dark colour and then a light colour. Defaults to `c("black", "white")`.
#' Use `lightness`, `greyness` or `darkness` with the applicable `*_mode_*` theme.
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
#'     mapping = aes(y = n - (max(n * 0.04)), label = n,
#'                   !!!aes_contrast(lightness)),
#'     position = position_dodge2(width = 0.75, preserve = "single"),
#'     show.legend = FALSE,
#'   )
aes_contrast <- function(col_pal = c("black", "white")) {

  ggplot2::aes(
    colour = ggplot2::after_scale(
      contrast(.data$fill, col_pal = col_pal[1:2])
      )
    )
}
