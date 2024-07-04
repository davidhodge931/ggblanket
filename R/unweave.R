#' Unweave geom defaults
#'
#' @description Reset all updated geom defaults back to ggplot2 defaults.
#'
#' @noRd
unweave_geom_defaults <- function() {
  ggplot2::update_geom_defaults("area", NULL)
  ggplot2::update_geom_defaults("bar", NULL)
  ggplot2::update_geom_defaults("boxplot", NULL)
  ggplot2::update_geom_defaults("col", NULL)
  ggplot2::update_geom_defaults("contour", NULL)
  ggplot2::update_geom_defaults("contour_filled", NULL)
  ggplot2::update_geom_defaults("crossbar", NULL)
  ggplot2::update_geom_defaults("density", NULL)
  ggplot2::update_geom_defaults("density2d", NULL)
  ggplot2::update_geom_defaults("density_2d_filled", NULL)
  ggplot2::update_geom_defaults("errorbar", NULL)
  ggplot2::update_geom_defaults("function", NULL)
  ggplot2::update_geom_defaults("hex", NULL)
  ggplot2::update_geom_defaults("line", NULL)
  ggplot2::update_geom_defaults("linerange", NULL)
  ggplot2::update_geom_defaults("path", NULL)
  ggplot2::update_geom_defaults("point", NULL)
  ggplot2::update_geom_defaults("pointrange", NULL)
  ggplot2::update_geom_defaults("polygon", NULL)
  ggplot2::update_geom_defaults("quantile", NULL)
  ggplot2::update_geom_defaults("raster", NULL)
  ggplot2::update_geom_defaults("rect", NULL)
  ggplot2::update_geom_defaults("ribbon", NULL)
  ggplot2::update_geom_defaults("rug", NULL)
  ggplot2::update_geom_defaults("segment", NULL)
  ggplot2::update_geom_defaults("sf", NULL)
  ggplot2::update_geom_defaults("smooth", NULL)
  ggplot2::update_geom_defaults("spoke", NULL)
  ggplot2::update_geom_defaults("step", NULL)
  ggplot2::update_geom_defaults("violin", NULL)
  #to add and adjust once ggplot makes GeomBin2d
  ggplot2::update_geom_defaults("tile", NULL)

  ggplot2::update_geom_defaults("abline", NULL)
  ggplot2::update_geom_defaults("hline", NULL)
  ggplot2::update_geom_defaults("vline", NULL)
  ggplot2::update_geom_defaults("curve", NULL)
  ggplot2::update_geom_defaults("text", NULL)
  ggplot2::update_geom_defaults("label", NULL)
}
