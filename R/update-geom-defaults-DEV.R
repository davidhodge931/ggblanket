#' Update the colour/fill
#'
#' @description
#' Updates the active theme for colour/fill styling for set aesthetics.
#'
#' @param col colour/fill base for most geoms.
#' @param ... Additional arguments (not used).
#'
#' @return An updated ggplot2 theme.
#'
#' @export
update_geom_col <- function(
    col = "#357BA2FF",
    ...
) {
  ggplot2::update_theme(geom = ggplot2::element_geom(colour = col, fill = col))
}

#' Update the shape
#'
#' @description
#' Updates the active theme for shape styling for set aesthetics.
#'
#' @param shape A shape.
#' @param ... Additional arguments (not used).
#'
#' @return Updated geom defaults for shape
#'
#' @export
update_geom_shape <- function(
    shape = 21,
    ...
) {
  ggplot2::update_theme(geom = ggplot2::element_geom(pointshape = shape))
}

#' Update the linetype
#'
#' @description
#' Updates the active theme for linetype styling for set aesthetics.
#'
#' @param linetype linetype.
#' @param ... Additional arguments (not used).
#'
#' @return An updated ggplot2 theme.
#'
#' @export
update_geom_linetype <- function(
    linetype = 1,
    ...
) {
  ggplot2::update_theme(geom = ggplot2::element_geom(linetype = linetype,
                                                     bordertype = linetype))
}

#' Update the linewidth
#'
#' @description
#' Updates the active theme for linewidth styling for set aesthetics.
#'
#' @param linewidth linewidth.
#' @param ... Additional arguments (not used).
#'
#' @return An updated ggplot2 theme.
#'
#' @export
update_geom_linewidth <- function(
    linewidth = 0.66,
    ...
) {
  ggplot2::update_theme(geom = ggplot2::element_geom(linewidth = linewidth,
                                                     borderwidth = linewidth))
}

#' Update the size
#'
#' @description
#' Updates the active theme for size styling for set aesthetics.
#'
#' @param size A size for point geoms.
#' @param ... Additional arguments (not used).
#'
#' @return Updated geom defaults for size
#'
#' @export
update_geom_size <- function(
    size = 1.5,
    ...
) {
  ggplot2::update_theme(geom = ggplot2::element_geom(pointsize = size))
}

