#' Set the perspective behaviour
#'
#' @description Set the perspective behaviour in `gg_*` functions.
#'
#' @param ... Provided to require argument naming, support trailing commas etc.
#' @param axis_line_transparent `TRUE` or `FALSE` of whether the `gg_*` function should remove the relevant axis line per the `perspective` of the plot.
#' @param axis_ticks_transparent `TRUE` or `FALSE` of whether the `gg_*` function should remove the relevant axis ticks per the `perspective` of the plot.
#' @param panel_grid_transparent `TRUE` or `FALSE` of whether the `gg_*` function should remove the relevant panel grid per the `perspective` of the plot.
#'
#' @noRd
update_perspective <- function(
    ...,
    axis_line_transparent = TRUE,
    axis_ticks_transparent = TRUE,
    panel_grid_transparent = TRUE
) {
  options(
    ggblanket.axis_line_transparent = axis_line_transparent,
    ggblanket.axis_ticks_transparent = axis_ticks_transparent,
    ggblanket.panel_grid_transparent = panel_grid_transparent
  )
}
