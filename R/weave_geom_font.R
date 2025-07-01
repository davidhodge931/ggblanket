#' Set the font-based geom defaults
#'
#' @description Update defaults for text/label geoms (text, label).
#'              Uses axis text properties as the basis for styling.
#'
#' @param ... Additional arguments passed to [ggplot2::element_geom()].
#' @param colour A hex code. If NULL, derived from axis text colour.
#' @param fill A hex code. If NULL, derived from panel background.
#' @param size A size (for text geoms). If NULL, derived from axis text size.
#' @param family A font family. If NULL, derived from axis text family.
#'
#' @noRd
weave_geom_font <- function(
    ...,
    colour = NULL,
    fill = NULL,
    size = NULL,
    family = NULL
) {
  # Get current theme
  current_theme <- ggplot2::get_theme()

  # Extract text properties for font-based geoms
    colour <- colour %||%
      current_theme$axis.text.x$colour %||%
      current_theme$axis.text.y$colour %||%
      current_theme$axis.text$colour %||%
      current_theme$text$colour %||%
      "black"

    fill <- fill %||%
      current_theme$panel.background$fill %||%
      "white"

  # Text-specific size handling
  if (rlang::is_null(size)) {
    # Get the raw size value from theme hierarchy
    raw_size <- current_theme$axis.text.x$size %||%
      current_theme$axis.text.y$size %||%
      current_theme$axis.text$size %||%
      current_theme$text$size
    # If we found a theme size, handle rel() objects
    if (!is.null(raw_size)) {
      if (inherits(raw_size, "rel")) {
        base_size <- current_theme$text$size
        # If base_size is also rel() or NULL, use default
        if (is.null(base_size) || inherits(base_size, "rel")) {
          base_size <- 11
        }
        size <- as.numeric(raw_size) * base_size
      } else {
        size <- raw_size
      }
    } else {
      # Only use the conversion factor for the final fallback
      size <- 11
    }
    # Theme sizes are already in the correct units for fontsize
    # No conversion needed
  } else {
    # If size is provided by user in mm, convert to fontsize scale
    # fontsize expects points, so convert mm to points
    size <- as.numeric(size) / 0.352777778
  }

  if (rlang::is_null(family)) {
    family <- current_theme$axis.text.x$family %||%
      current_theme$axis.text.y$family %||%
      current_theme$axis.text$family %||%
      current_theme$text$family %||%
      ""
  }

  # Update the theme for font-based geoms
  ggplot2::update_theme(
    geom.text = ggplot2::element_geom(
      colour = colour,
      family = family,
      fontsize = size,
      ...
    ),
    geom.label = ggplot2::element_geom(
      colour = colour,
      fill = fill,
      family = family,
      fontsize = size,
      ...
    )
  )
}
