#' Update the default shape
#'
#' @description
#' Updates the active theme to apply consistent shape styling for point geoms.
#'
#' @param shape A default shape for point geoms. Must be an integer between 0 and 25.
#' @param ... Additional arguments (not used).
#'
#' @return Updated geom defaults for shape
#'
#' @noRd
update_geom_shape <- function(
    shape = 21,
    ...
) {
  # Validate shape input
  if (!is.numeric(shape) || length(shape) != 1 || shape != as.integer(shape)) {
    rlang::abort("shape must be a single integer")
  }

  if (shape < 0 || shape > 25) {
    rlang::abort("shape must be an integer between 0 and 25")
  }

  # Update point geoms with the specified shape
  ggplot2::update_theme(
    geom.count = ggplot2::element_geom(pointshape = shape),
    geom.jitter = ggplot2::element_geom(pointshape = shape),
    geom.point = ggplot2::element_geom(pointshape = shape),
    geom.qq = ggplot2::element_geom(pointshape = shape),
    geom.pointrange = ggplot2::element_geom(pointshape = shape)
  )
}
