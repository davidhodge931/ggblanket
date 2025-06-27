#' Set the label geom defaults
#'
#' @description Update the "label" geom defaults.
#'
#' @param fill A hex code.
#' @param colour A hex code.
#' @param size A size in mm. Use `pt_to_mm` to convert.
#' @param family A family.
#' @param ... Additional arguments passed to [ggplot2::element_geom()].
#'
#' @noRd
#'
#' @examples
#' weave_geom_label(size = 4.233333)
#' weave_geom_label(size = pt_to_mm(12))
#'
weave_geom_label <- function(
    ...,
    fill = NULL,
    colour = NULL,
    size = NULL,
    family = NULL
) {
  # Get current theme
  current_theme <- ggplot2::get_theme()

  # Extract theme properties with fallback hierarchy
  if (rlang::is_null(colour)) {
    colour <- current_theme$axis.text.x$colour %||%
      current_theme$axis.text.y$colour %||%
      current_theme$axis.text$colour %||%
      current_theme$text$colour %||%
      "black"
  }

  if (rlang::is_null(fill)) {
    fill <- current_theme$panel.background$fill %||%
      "white"
  }

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

  # Update the theme
  ggplot2::update_theme(
    geom.label = ggplot2::element_geom(
      colour = colour,
      fill = fill,
      family = family,
      fontsize = size,
      ...
    )
  )
}
