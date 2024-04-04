#' Get contrast
#'
#' @param fill A fill aesthetic from which to determine the colour scale for contrast.
#' @param contrast_pal A vector of a dark colour and then a light colour. Defaults to `c("#121b24" ,"#ffffff")` (i.e. `lightness[c(1, 3)]`).
#'
#' @noRd
#'
#' @examples
#' get_contrast(fill = c("navy", "yellow", "orange"), contrast_pal = c("black", "white"))
#'
get_contrast <- function(fill, contrast_pal = c("#121b24", "#ffffff")) {
  out <- rep(contrast_pal[1], length(fill))
  light <- farver::get_channel(fill, "l", space = "hcl")
  out[light < 50] <- contrast_pal[2]
  out
}

#' A colour aesthetic that automatically contrasts with fill.
#'
#' @description A colour aesthetic for annotation that automatically contrasts with fill. Can be spliced into [ggplot2::aes] with [rlang::!!!].
#'
#' @param contrast_pal A vector of a dark colour and then a light colour. Defaults to `c("#121b24" ,"#ffffff")` (i.e. `lightness[c(1, 3)]`).
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
#'     x = n,
#'     y = sex,
#'     col = species,
#'     position = position_dodge2(preserve = "single"),
#'     width = 0.75,
#'     y_labels = \(x) str_to_sentence(x),
#'     mode = dark_mode_r(),
#'   ) +
#'   geom_text(
#'     mapping = aes(label = n, !!!aes_contrast(darkness[c(4, 1)])),
#'     position = position_dodge2(width = 0.75, preserve = "single"),
#'     hjust = 1.25,
#'     show.legend = FALSE,
#'   )
aes_contrast <- function(contrast_pal = c("#121b24", "#ffffff")) {

  ggplot2::aes(
    colour = ggplot2::after_scale(
      get_contrast(.data$fill, contrast_pal = contrast_pal[1:2])
      )
    )
}
