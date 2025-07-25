#' Set the aspect behaviour
#'
#' @description Set the aspect behaviour in `gg_*` functions.
#'
#' @param ... Provided to require argument naming, support trailing commas etc.
#' @param aspect_axis_line_rm `TRUE` or `FALSE` of whether the `gg_*` function should remove the relevant axis line per the `aspect` of the plot.
#' @param aspect_axis_ticks_rm `TRUE` or `FALSE` of whether the `gg_*` function should remove the relevant axis ticks per the `aspect` of the plot.
#' @param aspect_panel_grid_rm `TRUE` or `FALSE` of whether the `gg_*` function should remove the relevant panel grid per the `aspect` of the plot.
#'
#' @noRd
update_aspect <- function(
    ...,
    aspect_axis_line_rm = TRUE,
    aspect_axis_ticks_rm = TRUE,
    aspect_panel_grid_rm = TRUE
) {
  options(
    ggblanket.aspect_axis_line_rm = aspect_axis_line_rm,
    ggblanket.aspect_axis_ticks_rm = aspect_axis_ticks_rm,
    ggblanket.aspect_panel_grid_rm = aspect_panel_grid_rm
  )
}
