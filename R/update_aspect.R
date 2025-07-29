#' Set the aspect behaviour
#'
#' @description Set the aspect behaviour in `gg_*` functions.
#'
#' @param ... Provided to require argument naming, support trailing commas etc.
#' @param aspect_axis_line_transparent `TRUE` or `FALSE` of whether the `gg_*` function should remove the relevant axis line per the `aspect` of the plot.
#' @param aspect_axis_ticks_transparent `TRUE` or `FALSE` of whether the `gg_*` function should remove the relevant axis ticks per the `aspect` of the plot.
#' @param aspect_panel_grid_transparent `TRUE` or `FALSE` of whether the `gg_*` function should remove the relevant panel grid per the `aspect` of the plot.
#'
#' @noRd
update_aspect <- function(
  ...,
  aspect_axis_line_transparent = TRUE,
  aspect_axis_ticks_transparent = TRUE,
  aspect_panel_grid_transparent = TRUE
) {
  options(
    ggblanket.aspect_axis_line_transparent = aspect_axis_line_transparent,
    ggblanket.aspect_axis_ticks_transparent = aspect_axis_ticks_transparent,
    ggblanket.aspect_panel_grid_transparent = aspect_panel_grid_transparent
  )
}
