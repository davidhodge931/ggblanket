#' Get contrast
#'
#' @param fill A fill aesthetic from which to determine the colour scale for contrast.
#' @param contrast_palette A vector of a dark colour and then a light colour. Defaults to `c("#121b24" ,"#ffffff")` (i.e. `lightness[c(1, 4)]`).
#'
#' @noRd
#'
#' @examples
#' get_contrast(fill = c("navy", "yellow", "orange"), contrast_palette = c("black", "white"))
#'
get_contrast <- function(fill, contrast_palette = c("#121b24", "#ffffff")) {
  out <- rep(contrast_palette[1], length(fill))
  light <- farver::get_channel(fill, "l", space = "hcl")
  out[light < 50] <- contrast_palette[2]
  out
}

#' A colour aesthetic that automatically contrasts with fill.
#'
#' @description A colour aesthetic for annotation that automatically contrasts with fill. Can be spliced into [ggplot2::aes] with [rlang::!!!].
#'
#' @param contrast_palette A vector of a dark colour and then a light colour. Defaults to `c("#121b24" ,"#ffffff")` (i.e. `lightness[c(1, 4)]`).
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
#'     mapping = aes(label = n, !!!aes_contrast(darkness[c(5, 1)])),
#'     position = position_dodge2(width = 0.75, preserve = "single"),
#'     hjust = 1.25,
#'     show.legend = FALSE,
#'   )
aes_contrast <- function(contrast_palette = c("#121b24", "#ffffff")) {

  ggplot2::aes(
    colour = ggplot2::after_scale(
      get_contrast(.data$fill, contrast_palette = contrast_palette[1:2])
      )
    )
}
