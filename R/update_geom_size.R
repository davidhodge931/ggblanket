#' Update the default size
#'
#' @description
#' Updates the active theme to apply consistent size styling.
#' Excludes "text", "label", "hline", and "vline".
#'
#' @param size A default size. Defaults to 1.5.
#' @param ... Provided to require argument naming, support trailing commas etc.
#'
#' @return Updated geom defaults for size
#'
#' @noRd
update_geom_size <- function(size = 1.5, ...) {
  ggplot2::update_theme(
    geom.count = ggplot2::element_geom(pointsize = size),
    geom.jitter = ggplot2::element_geom(pointsize = size),
    geom.point = ggplot2::element_geom(pointsize = size),
    geom.pointrange = ggplot2::element_geom(pointsize = size * 3),
    geom.qq = ggplot2::element_geom(pointsize = size),
  )
}
