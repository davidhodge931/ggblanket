#' Update default linetype for geoms
#'
#' @description
#' Modifies the default linetype for all geoms or specific geoms in ggplot2 plots.
#' This function updates the active theme to apply consistent linetype styling across
#' plot elements.
#'
#' @param linetype The default linetype to use for most geoms. Can be specified as:
#'   - An integer (0 = blank, 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash,
#'     5 = longdash, 6 = twodash)
#'   - A string ("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
#'   - A numeric vector defining a custom dash-dot pattern
#'   Defaults to 1 (solid line).
#' @param linetype2 The alternative linetype to use for geoms specified in
#'   `linetype2_geoms`. Uses the same specification options as `linetype`.
#'   Defaults to 0 (blank/no line).
#' @param linetype2_geoms Character vector of geom names that should use `linetype2`
#'   instead of the default `linetype`. Common filled geoms are included by default.
#'
#' @details
#' This function modifies both `linetype` and `bordertype` properties of geoms to
#' ensure consistent appearance. The `linetype2` value is typically used to remove
#' borders from filled geoms (like bars and areas) while maintaining lines for
#' other geoms (like lines and points).
#'
#' @return
#' Invisibly returns the previous theme settings.
#'
#' @examples
#' # Remove borders from filled geoms
#' update_geom_linetype(linetype = 1, linetype2 = 0)
#'
#' # Use dashed lines for specific geoms
#' update_geom_linetype(
#'   linetype = 1,
#'   linetype2 = 2,
#'   linetype2_geoms = c("smooth", "line")
#' )
#'
#' @seealso
#' \code{\link[ggplot2]{theme}}, \code{\link[ggplot2]{update_theme}}
#'
#' @export
update_geom_linetype <- function(
    linetype = 1,
    linetype2 = 0,
    linetype2_geoms = c("area", "bar", "col", "density", "map", "polygon",
                        "rect", "ribbon", "tile", "violin", "bin2d", "hex",
                        "raster", "contour_filled", "density_2d_filled")) {

  # Get all geom names from ggplot2
  all_geoms <- c("boxplot", "crossbar", "contour", "count", "curve",
                 "dotplot", "density2d", "errorbar", "freqpoly", "function",
                 "jitter", "line", "linerange", "path", "point", "pointrange",
                 "qq", "qq_line", "quantile", "rug", "segment", "smooth",
                 "spoke", "step", "area", "bar", "col", "density", "map",
                 "polygon", "rect", "ribbon", "tile", "violin", "bin2d",
                 "hex", "raster", "contour_filled", "density_2d_filled")

  # Build named list of theme elements
  theme_args <- list()

  # Apply linetype to all geoms
  for (geom in all_geoms) {
    geom_name <- paste0("geom.", gsub("_", "", geom))

    # Check if this geom should use linetype2
    if (geom %in% linetype2_geoms) {
      theme_args[[geom_name]] <- ggplot2::element_geom(
        bordertype = linetype2,
        linetype = linetype2
      )
    } else {
      theme_args[[geom_name]] <- ggplot2::element_geom(
        bordertype = linetype,
        linetype = linetype
      )
    }
  }

  # Use inject and splice to pass the arguments
  rlang::inject(ggplot2::update_theme(!!!theme_args))
}
