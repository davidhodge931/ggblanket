#' Get a dark/light colour for contrast
#'
#' @description Get a dark/light colour based on contrast with fill colours
#'
#' @param fill A fill aesthetic from which to determine the colour scale for contrast.
#' @param dark A dark colour.
#' @param light A light colour.
#'
#' @noRd
#'
#' @examples
#' get_colour_contrast(fill = c("navy", "yellow", "orange"), dark = "black", light = "white")
#'
get_colour_contrast <- function(fill,
                                dark = "#121B24FF",
                                light = "#FFFFFFFF") {

  ifelse(farver::get_channel(
    colour = fill,
    channel = "l",
    space = "hcl"
  ) < 50,
  light,
  dark)
}

#' A colour aesthetic for contrast
#'
#' @description A colour aesthetic to contrast with a fill aesthetic. Can be spliced into [ggplot2::aes] with [rlang::!!!].
#'
#' @param ... Provided to force user argument naming etc.
#' @param dark A dark colour.
#' @param light A light colour.
#'
#' @return A ggplot2 aesthetic
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
#'     position = position_dodge(preserve = "single"),
#'     width = 0.75,
#'     x_labels = \(x) str_to_sentence(x),
#'   ) +
#'   geom_text(
#'     mapping = aes_colour_contrast(),
#'     position = position_dodge(width = 0.75, preserve = "single"),
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
#'     position = position_dodge(preserve = "single"),
#'     width = 0.75,
#'     x_labels = \(x) str_to_sentence(x),
#'     theme = dark_mode_r(),
#'   ) +
#'   geom_text(
#'     mapping = aes(label = n, !!!aes_colour_contrast(dark = darkness[3], light = darkness[1])),
#'     position = position_dodge(width = 0.75, preserve = "single"),
#'     vjust = 1.33,
#'     show.legend = FALSE,
#'   )
aes_colour_contrast <- function(..., dark = "#121B24FF", light = "#FFFFFFFF") {

  ggplot2::aes(
    colour = ggplot2::after_scale(
      get_colour_contrast(.data$fill, dark = dark, light = light)
    )
  )
}
