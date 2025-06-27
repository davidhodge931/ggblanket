#' Set the geom abline defaults
#'
#' @description Update the "abline" geom defaults.
#'
#' @param ... Additional arguments passed to [ggplot2::element_geom()].
#' @param colour A hex code.
#' @param linewidth A linewidth.
#'
#' @noRd
weave_geom_abline <- function(
    ...,
    colour = NULL,
    linewidth = NULL
) {
  # Get current theme
  current_theme <- ggplot2::get_theme()

  # Extract theme properties with fallback hierarchy
  if (rlang::is_null(colour)) {
    colour <- current_theme$axis.line.x.bottom$colour %||%
      current_theme$axis.line.x.top$colour %||%
      current_theme$axis.line.y.left$colour %||%
      current_theme$axis.line.y.right$colour %||%
      current_theme$axis.line.x$colour %||%
      current_theme$axis.line.y$colour %||%
      current_theme$axis.line$colour %||%
      "black"
  }

  if (rlang::is_null(linewidth)) {
    linewidth <- current_theme$axis.line.x.bottom$linewidth %||%
      current_theme$axis.line.x.top$linewidth %||%
      current_theme$axis.line.y.left$linewidth %||%
      current_theme$axis.line.y.right$linewidth %||%
      current_theme$axis.line.x$linewidth %||%
      current_theme$axis.line.y$linewidth %||%
      current_theme$axis.line$linewidth %||%
      0.25
  }

  # Update the theme
  ggplot2::update_theme(
    geom.abline = ggplot2::element_geom(
      colour = colour,
      linewidth = linewidth,
      ...
    )
  )
}
