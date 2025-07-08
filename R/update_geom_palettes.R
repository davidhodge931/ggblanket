#' Update the geom palettes
#'
#' @description
#' This function allows you to set colour and fill palettes with a hierarchical priority system. More specific parameters override more general ones.
#'
#' @param ... Provided to require argument naming, support trailing commas etc.
#' @param col_palette_d For a discrete colour and fill scale, a character vector of hex codes, or a `scales::pal_*()` function.
#' @param col_palette_c For a continuous colour and fill scale, a character vector of hex codes, or a `scales::pal_*()` function.
#'
#' @return An updated ggplot2 theme.
#'
#' @seealso
#' \code{\link[ggplot2]{theme}}, \code{\link[ggplot2]{update_theme}}
#'
#' @export
update_geom_palettes <- function(
    ...,
    col_palette_d = NULL,
    col_palette_c = NULL
) {
  # options(ggplot2.discrete.colour = NULL)
  # options(ggplot2.continuous.colour = NULL)
  # options(ggplot2.discrete.fill = NULL)
  # options(ggplot2.continuous.fill = NULL)

  # Determine colour_palette_d (most specific to most general)
  colour_palette_d <- col_palette_d %||%
    jumble

  # Determine colour_palette_c (most specific to most general)
  colour_palette_c <- col_palette_c %||%
    scales::pal_viridis(option = "G", direction = -1)

  # Determine fill_palette_d (most specific to most general)
  fill_palette_d <- col_palette_d %||%
    jumble

  # Determine fill_palette_c (most specific to most general)
  fill_palette_c <- col_palette_c %||%
    scales::pal_viridis(option = "G", direction = -1)

  # Update the theme with palette values
  ggplot2::update_theme(
    palette.colour.discrete = colour_palette_d,
    palette.colour.continuous = colour_palette_c,
    palette.fill.discrete = fill_palette_d,
    palette.fill.continuous = fill_palette_c
  )
}
