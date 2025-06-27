#' Set the reference line geom defaults
#'
#' @description Update defaults for reference line geoms (abline, hline, vline, curve).
#'              Uses axis line properties as the basis for styling.
#'
#' @param ... Additional arguments passed to [ggplot2::element_geom()].
#' @param colour A hex code. If NULL, derived from axis line colour.
#' @param linewidth A linewidth. If NULL, derived from axis line linewidth.
#'
#' @noRd
weave_geom_reference_line <- function(
    ...,
    colour = NULL,
    linewidth = NULL
) {
  # Get current theme
  current_theme <- ggplot2::get_theme()

  # Extract axis line properties for reference lines
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

  # Update the theme for reference line geoms
  ggplot2::update_theme(
    geom.abline = ggplot2::element_geom(
      colour = colour,
      linewidth = linewidth,
      ...
    ),
    geom.hline = ggplot2::element_geom(
      colour = colour,
      linewidth = linewidth,
      ...
    ),
    geom.vline = ggplot2::element_geom(
      colour = colour,
      linewidth = linewidth,
      ...
    ),
    geom.curve = ggplot2::element_geom(
      colour = colour,
      linewidth = linewidth,
      ...
    )
  )
}
