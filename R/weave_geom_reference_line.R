#' Set the reference line geom defaults
#'
#' @description Update defaults for reference line geoms (abline, hline, vline).
#'              Uses axis line properties as the basis for styling.
#'
#' @param ... Additional arguments passed to [ggplot2::element_geom()].
#' @param colour A hex code. If NULL, derived from axis line colour.
#' @param linewidth A linewidth. If NULL, derived from axis line linewidth.
#' @param linetype A linewidth. If NULL, derived from axis line linetype.
#'
#' @return An updated ggplot2 theme with modified geom abline, hline and vline defaults
#'
#' @export
#'
weave_geom_reference_line <- function(
    ...,
    colour = NULL,
    linewidth = NULL,
    linetype = NULL
) {
  # Get current theme
  current_theme <- ggplot2::get_theme()

  # Extract axis line properties for reference lines
  colour <- colour %||%
      current_theme$axis.line.x.bottom$colour %||%
      current_theme$axis.line.x.top$colour %||%
      current_theme$axis.line.y.left$colour %||%
      current_theme$axis.line.y.right$colour %||%
      current_theme$axis.line.x$colour %||%
      current_theme$axis.line.y$colour %||%
      current_theme$axis.line$colour %||%
      "black"

  linewidth <- linewidth %||%
    current_theme$axis.line.x.bottom$linewidth %||%
    current_theme$axis.line.x.top$linewidth %||%
    current_theme$axis.line.y.left$linewidth %||%
    current_theme$axis.line.y.right$linewidth %||%
    current_theme$axis.line.x$linewidth %||%
    current_theme$axis.line.y$linewidth %||%
    current_theme$axis.line$linewidth %||%
    0.25

  linetype <- linetype %||%
    current_theme$axis.line.x.bottom$linetype %||%
    current_theme$axis.line.x.top$linetype %||%
    current_theme$axis.line.y.left$linetype %||%
    current_theme$axis.line.y.right$linetype %||%
    current_theme$axis.line.x$linetype %||%
    current_theme$axis.line.y$linetype %||%
    current_theme$axis.line$linetype %||%
    1

  # Update the theme for reference line geoms
  ggplot2::update_theme(
    geom.abline = ggplot2::element_geom(
      colour = colour,
      linewidth = linewidth,
      linetype = linetype,
      ...
    ),
    geom.hline = ggplot2::element_geom(
      colour = colour,
      linewidth = linewidth,
      linetype = linetype,
      ...
    ),
    geom.vline = ggplot2::element_geom(
      colour = colour,
      linewidth = linewidth,
      linetype = linetype,
      ...
    )
  )
}
