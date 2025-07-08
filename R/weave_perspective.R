# global defaults
ggblanket_global <- new.env(parent = emptyenv())

ggblanket_global$perspective <- NULL
ggblanket_global$axis_line_transparent <- NULL
ggblanket_global$axis_ticks_transparent <- NULL
ggblanket_global$axis_line_transparent <- NULL

#' Set the perspective behaviour
#'
#' @description Set the perspective behaviour in `gg_*` functions.
#'
#' @param perspective The perspective of plot, which affects the theme components that can be removed by the `gg_*` function. Either `"x"` or `"y"`. Defaults to `NULL`, which lets the `gg_*` function guess it based on the data.
#' @param ... Provided to require argument naming, support trailing commas etc.
#' @param axis_line_transparent `TRUE` or `FALSE` of whether the `gg_*` function should remove the relevant axis line per the `perspective` of the plot.
#' @param axis_ticks_transparent `TRUE` or `FALSE` of whether the `gg_*` function should remove the relevant axis ticks per the `perspective` of the plot.
#' @param panel_grid_transparent `TRUE` or `FALSE` of whether the `gg_*` function should remove the relevant panel grid per the `perspective` of the plot.
#'
#' @noRd
weave_perspective <- function(
    perspective = NULL,
    ...,
    axis_line_transparent = TRUE,
    axis_ticks_transparent = TRUE,
    panel_grid_transparent = TRUE
) {
  old <- ggblanket_global$perspective
  ggblanket_global$perspective <- perspective
  invisible(old)

  old <- ggblanket_global$axis_line_transparent
  ggblanket_global$axis_line_transparent <- axis_line_transparent
  invisible(old)

  old <- ggblanket_global$axis_ticks_transparent
  ggblanket_global$axis_ticks_transparent <- axis_ticks_transparent
  invisible(old)

  old <- ggblanket_global$panel_grid_transparent
  ggblanket_global$panel_grid_transparent <- panel_grid_transparent
  invisible(old)
}
