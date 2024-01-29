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

#' A colour aesthetic that automatically contrasts with fill
#'
#' @description A colour aesthetic that automatically contrasts with fill, and can be spliced within ggplot2::aes.
#'
#' @param col_pal_dark A dark colour used for text on light polygons.
#' @param col_pal_light A light colour used for text on dark polygons.
#'
#' @return An aesthetic.
#' @export
#'
#' @references Based on code by Teun van den Brand
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
#'     col_pal = c("#0095A8", "#112E51", "#FF7043"),
#'     position = position_dodge2(preserve = "single"),
#'     width = 0.75,
#'     x_labels = \(x) stringr::str_to_sentence(x),
#'   ) +
#'   geom_text(
#'     mapping = aes(y = n - (max(n * 0.04)), label = n, !!!aes_contrast()),
#'     position = position_dodge2(width = 0.75, preserve = "single"),
#'     show.legend = FALSE,
#'   )
#'
#' penguins |>
#'   count(species, sex) |>
#'   gg_col(
#'    x = sex,
#'    y = n,
#'    col = species,
#'    position = position_dodge2(preserve = "single"),
#'    width = 0.75,
#'    x_labels = \(x) stringr::str_to_sentence(x),
#'    theme = dark_mode_rt(),
#'  ) +
#'  geom_text(
#'    mapping = aes(y = n - (max(n * 0.04)), label = n,
#'                  !!!aes_contrast(pal_dark_mode["plot"], pal_dark_mode["text"])),
#'    position = position_dodge2(width = 0.75, preserve = "single"),
#'    show.legend = FALSE,
#'  )
#'
aes_contrast <- function(col_pal_dark = pal_light_mode["text"],
                         col_pal_light = pal_light_mode["panel"]) {

  ggplot2::aes(
    colour = ggplot2::after_scale(
      contrast(.data$fill,
               col_pal_dark = col_pal_dark,
               col_pal_light = col_pal_light)
    )
  )
}

