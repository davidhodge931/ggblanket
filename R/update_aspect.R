#' Set the aspect behaviour
#'
#' @description Set the aspect behaviour in `gg_*` functions.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param axis_line_aspect `"transparent"`, `"blank"` or `"keep"` of how to treat the y axis line for an `"x"` `aspect`, and vice versa.
#' @param axis_ticks_aspect `"transparent"`, `"blank"` or `"keep"` of how to treat the y axis ticks for an `"x"` `aspect`, and vice versa.
#' @param panel_grid_aspect `"transparent"`, `"blank"` or `"keep"` of how to treat the x panel grid for an `"x"` `aspect`, and vice versa.
#'
#' @noRd
update_aspect <- function(
  ...,
  axis_line_aspect = "transparent",
  axis_ticks_aspect = "transparent",
  panel_grid_aspect = "transparent"
) {
  if (!axis_line_aspect %in% c("transparent", "blank", "keep")) {
    rlang::abort("axis_line_aspect must be 'transparent', 'blank', or 'keep'")
  }
  if (!axis_ticks_aspect %in% c("transparent", "blank", "keep")) {
    rlang::abort("axis_ticks_aspect must be 'transparent', 'blank', or 'keep'")
  }
  if (!panel_grid_aspect %in% c("transparent", "blank", "keep")) {
    rlang::abort("panel_grid_aspect must be 'transparent', 'blank', or 'keep'")
  }

  options(
    ggblanket.axis_line_aspect = axis_line_aspect,
    ggblanket.axis_ticks_aspect = axis_ticks_aspect,
    ggblanket.panel_grid_aspect = panel_grid_aspect
  )
}
