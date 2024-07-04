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

#' Label every nth element
#'
#' @description
#' Label every nth element in a vector, and replace the rest with "".
#'
#' @param n The increment of elements to hold as is. Defaults to `2`.
#' @param offset An offset for which element to first hold. Defaults to `0`. Possible values are `-1` to (`n - 2`)
#' @param ... If numeric, arguments passed to the `scales::comma` function. Otherwise, arguments passed to `format`.
#'
#' @return A labelling function
#' @noRd
#'
#' @examples
#'  label_every_nth()(scales::comma(seq(1000, 5000, 1000)))
#'  label_every_nth()(lubridate::ymd(c("2021-01-01", "2022-01-01", "2023-01-01", "2024-01-01")))
#'  label_every_nth()(LETTERS[1:12])
#'
#'  library(dplyr)
#'  library(palmerpenguins)
#'
#'  set_blanket()
#'
#'  penguins |>
#'    mutate(across(sex, \(x) stringr::str_to_sentence(x))) |>
#'    gg_point(
#'      x = flipper_length_mm,
#'      y = body_mass_g,
#'      col = sex,
#'      x_labels = label_every_nth(),
#'      y_labels = label_every_nth(),
#'    )
#
label_every_nth <- function(n = 2, offset = 0, ...) {
  function(x) {
    i <- which(is.finite(x) | is.character(x) | is.factor(x) | is.logical(x))
    i <- i[seq_along(i) %% n == (offset + 1)]

    if (is.numeric(x)) x <- scales::comma(x, ...)
    else x <- format(x, ...)

    x[-i] <- ""
    x
  }
}
