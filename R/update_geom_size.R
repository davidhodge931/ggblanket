#' Update the default size
#'
#' @description
#' Updates the active theme to apply consistent size styling.
#'
#' @param size A default size for point geoms.
#' @param size_font A size for text/label geoms in mm. If NULL, derived from axis text size.
#' @param family_font A font family. If NULL, derived from axis text family.
#' @param ... Additional arguments (not used).
#'
#' @return Updated geom defaults for size
#'
#' @noRd
update_geom_size <- function(
    size = 1.5,
    size_font = NULL,
    ...
) {
  # Get current theme for font defaults
  current_theme <- ggplot2::get_theme()

  # Text-specific size handling
  if (rlang::is_null(size_font)) {
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
        size_font <- as.numeric(raw_size) * base_size
      } else {
        size_font <- raw_size
      }
    } else {
      # Only use the conversion factor for the final fallback
      size_font <- 11
    }
    # Theme sizes are already in the correct units for fontsize
    # No conversion needed
  } else {
    # If size is provided by user in mm, convert to fontsize scale
    # fontsize expects points, so convert mm to points
    size_font <- as.numeric(size_font) / 0.352777778
  }

  # Update point geoms
  ggplot2::update_theme(
    geom.count = ggplot2::element_geom(pointsize = size),
    geom.jitter = ggplot2::element_geom(pointsize = size),
    geom.point = ggplot2::element_geom(pointsize = size),
    geom.qq = ggplot2::element_geom(pointsize = size),

    geom.pointrange = ggplot2::element_geom(pointsize = size * 3),

    # Update font geoms
    geom.text = ggplot2::element_geom(
      fontsize = size_font
    ),
    geom.label = ggplot2::element_geom(
      fontsize = size_font
    )
  )
}
