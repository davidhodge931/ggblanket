#' Update the size for point/pointrange geoms
#'
#' @description
#' Update the size for geoms. Excludes "text" and "label".
#'
#' @param size A default size for the point geom. Defaults to 1.5.
#' @param ... Provided to require argument naming, support trailing commas etc.
#'
#' @return Updated geom defaults for size
#'
#' @export
weave_geom_size <- function(size = 1.5, ...) {
  ggplot2::update_theme(
    geom.count = ggplot2::element_geom(pointsize = size),
    geom.jitter = ggplot2::element_geom(pointsize = size),
    geom.point = ggplot2::element_geom(pointsize = size),
    geom.pointrange = ggplot2::element_geom(pointsize = size),
    geom.qq = ggplot2::element_geom(pointsize = size),
  )
}
