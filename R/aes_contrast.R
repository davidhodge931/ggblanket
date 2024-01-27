#' Contrast internal function
#'
#' @param col The fill or colour aesthetic.
#' @param col_pal_dark A dark colour used for text on light polygons. Defaults to "black".
#' @param col_pal_light A light colour used for text on dark polygons. Defaults to "white".
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

#' Contrast aesthetic
#'
#' @description Contrast aesthetic to be sliced within ggplot2::aes
#'
#' @param col_pal_dark A dark colour used for text on light polygons. Defaults to "white".
#' @param col_pal_light A light colour used for text on dark polygons. Defaults to "black".
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
aes_contrast <- function(col_pal_dark = "black", col_pal_light = "white") {
  ggplot2::aes(
    colour = ggplot2::after_scale(
      contrast(.data$fill,
               col_pal_dark = col_pal_dark,
               col_pal_light = col_pal_light)
    )
  )
}

#' Contrast aesthetic for light_mode_* theme
#'
#' @description Contrast aesthetic to be sliced within ggplot2::aes with light/dark text colours consistent with light_mode_* defaults.
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
#'     mapping = aes(y = n - (max(n * 0.04)), label = n, !!!aes_contrast_light_mode),
#'     position = position_dodge2(width = 0.75, preserve = "single"),
#'     show.legend = FALSE,
#'   )
#'
aes_contrast_light_mode <- {
  ggplot2::aes(
    colour = ggplot2::after_scale(
      contrast(.data$fill,
               col_pal_dark = pal_light_mode["text"],
               col_pal_light = pal_light_mode["panel"])
    )
  )
}

#' Contrast aesthetic for dark_mode_* theme
#'
#' @description Contrast aesthetic to be sliced within ggplot2::aes with light/dark text colours consistent with dark_mode_* defaults.
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
#'     theme = dark_mode_rt(),
#'   ) +
#'   geom_text(
#'     mapping = aes(y = n - (max(n * 0.04)), label = n, !!!aes_contrast_dark_mode),
#'     position = position_dodge2(width = 0.75, preserve = "single"),
#'     show.legend = FALSE,
#'   )
#'
aes_contrast_dark_mode <- {
  ggplot2::aes(
    colour = ggplot2::after_scale(
      contrast(.data$fill,
               col_pal_dark = pal_dark_mode["plot"],
               col_pal_light = pal_dark_mode["text"])
    )
  )
}
