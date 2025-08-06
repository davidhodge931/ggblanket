#' Set the aspect behaviour
#'
#' @description Set the aspect behaviour in `gg_*` functions.
#'
#' @param ... Provided to require argument naming, support trailing commas etc.
#' @param aspect_axis_line `"transparent"`, `"blank"` or `"keep"` of how to treat the y axis line for an `"x"` `aspect`, and vice versa.
#' @param aspect_axis_ticks `"transparent"`, `"blank"` or `"keep"` of how to treat the y axis ticks for an `"x"` `aspect`, and vice versa.
#' @param aspect_panel_grid `"transparent"`, `"blank"` or `"keep"` of how to treat the x panel grid for an `"x"` `aspect`, and vice versa.
#'
#' @noRd
update_aspect <- function(
  ...,
  aspect_axis_line = "transparent",
  aspect_axis_ticks = "transparent",
  aspect_panel_grid = "transparent"
) {
  if (!aspect_axis_line %in% c("transparent", "blank", "keep")) {
    rlang::abort("aspect_axis_line must be 'transparent', 'blank', or 'keep'")
  }
  if (!aspect_axis_ticks %in% c("transparent", "blank", "keep")) {
    rlang::abort("aspect_axis_ticks must be 'transparent', 'blank', or 'keep'")
  }
  if (!aspect_panel_grid %in% c("transparent", "blank", "keep")) {
    rlang::abort("aspect_panel_grid must be 'transparent', 'blank', or 'keep'")
  }

  options(
    ggblanket.aspect_axis_line = aspect_axis_line,
    ggblanket.aspect_axis_ticks = aspect_axis_ticks,
    ggblanket.aspect_panel_grid = aspect_panel_grid
  )
}
