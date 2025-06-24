#' Guides for legend element colour
#'
#' @description
#' Guides to over-ride legend elements with a grey colour
#' * `guides_shape_grey()` for shape
#' * `guides_linewidth_grey()` for linewidth
#' * `guides_size_grey()` for size.
#'
#' @param colour A default hex code to override the colour of the legend elements. Note, the "fill" inherits from this argument.  Defaults to grey.
#' @param ... Other arguments passed to [ggplot2::guide_legend ()].
#'
#' @return A ggplot guides.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#' library(palmerpenguins)
#'
#' set_blanket()
#'
#' penguins |>
#'   drop_na() |>
#'   gg_jitter(
#'     x = species,
#'     y = flipper_length_mm,
#'     col = island,
#'     mapping = aes(shape = sex),
#'   ) +
#'   guides_shape_grey()
#'
guides_shape_grey <- function(colour = grey, ...) {
  ggplot2::guides(
    shape = ggplot2::guide_legend(
      override.aes = list(colour = colour, fill = colour),
      ...
    )
  )
}

#' @rdname guides_shape_grey
#' @export
guides_linewidth_grey <- function(colour = grey, ...) {
  ggplot2::guides(
    linewidth = ggplot2::guide_legend(
      override.aes = list(colour = colour, fill = colour),
      ...
    )
  )
}

#' @rdname guides_shape_grey
#' @export
guides_size_grey <- function(colour = grey, ...) {
  ggplot2::guides(
    size = ggplot2::guide_legend(
      override.aes = list(colour = colour, fill = colour),
      ...
    )
  )
}
