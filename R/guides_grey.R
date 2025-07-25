#' Guides for legend element colour
#'
#' @description
#' Guides to over-ride legend elements with a grey colour
#' * `guides_shape_grey()` for shape
#' * `guides_linewidth_grey()` for linewidth
#' * `guides_size_grey()` for size.
#'
#' @param col A default hex code to override the colour of the legend elements. Defaults to grey.
#' @param ... Other arguments passed to [ggplot2::guide_legend()].
#'
#' @return A ggplot guides.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#'
#' set_blanket()
#'
#' palmerpenguins::penguins |>
#'   drop_na() |>
#'   gg_jitter(
#'     x = species,
#'     y = flipper_length_mm,
#'     col = island,
#'     mapping = aes(shape = sex),
#'   ) +
#'   guides_shape_grey()
#'
guides_shape_grey <- function(col = "#8991A1", ...) {
  # Get current theme and geom defaults
  current_theme <- ggplot2::theme_get()
  shape <- ggplot2::get_geom_defaults("point")$shape

  # If shape is fillable (21-25), adjust colour and use col for fill
  if (!is.null(shape) && shape %in% 21:25) {
    if (is_panel_dark(theme = current_theme)) {
      border <- col_screen(col)
    } else {
      border <- col_multiply(col)
    }
    override_aes <- list(colour = border, fill = col)
  } else {
    # Otherwise just use normal colour
    override_aes <- list(colour = col)
  }

  ggplot2::guides(
    shape = ggplot2::guide_legend(
      override.aes = override_aes,
      ...
    )
  )
}

#' @rdname guides_shape_grey
#' @export
guides_linewidth_grey <- function(col = "#8991A1", ...) {
  # Linewidth only needs colour, no shape checking needed
  ggplot2::guides(
    linewidth = ggplot2::guide_legend(
      override.aes = list(colour = col, fill = col),
      ...
    )
  )
}

#' @rdname guides_shape_grey
#' @export
guides_size_grey <- function(col = "#8991A1", ...) {
  # Get current theme and geom defaults
  current_theme <- ggplot2::theme_get()
  shape <- ggplot2::get_geom_defaults("point")$shape

  # If shape is fillable (21-25), adjust colour and use col for fill
  if (!is.null(shape) && shape %in% 21:25) {
    if (is_panel_dark(theme = current_theme)) {
      border <- col_screen(col)
    } else {
      border <- col_multiply(col)
    }
    override_aes <- list(colour = border, fill = col)
  } else {
    # Otherwise just use normal colour
    override_aes <- list(colour = col)
  }

  ggplot2::guides(
    size = ggplot2::guide_legend(
      override.aes = override_aes,
      ...
    )
  )
}
