#' Set the font family geom defaults
#'
#' @description Update font family for text/label geoms (text, label).
#'
#' @param family_font A font family. If NULL, derived from axis text family.
#' @param ... Additional arguments passed to [ggplot2::element_geom()].
#'
#' @return An updated ggplot2 theme.
#'
#' @noRd
update_geom_family <- function(
    family_font = NULL,
    ...
) {
  # Get current theme
  current_theme <- ggplot2::get_theme()

  if (rlang::is_null(family_font)) {
    family_font <- current_theme$axis.text.x$family %||%
      current_theme$axis.text.y$family %||%
      current_theme$axis.text$family %||%
      current_theme$text$family %||%
      ""
  }

  # Update the theme for font-based geoms
  ggplot2::update_theme(
    geom.text = ggplot2::element_geom(
      family = family_font,
      ...
    ),
    geom.label = ggplot2::element_geom(
      family = family_font,
      ...
    )
  )
}
