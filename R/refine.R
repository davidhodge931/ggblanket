# ------------------------------------------------------------------------------
# Internal helpers
# ------------------------------------------------------------------------------

.infer_orientation <- function(discrete) {
  if (discrete == "x") {
    "x"
  } else if (discrete == "y") {
    "y"
  } else {
    "x"
  }
}

.validate_refine_args <- function(discrete, orientation) {
  rlang::check_required(discrete)
  discrete <- rlang::arg_match(discrete, c("none", "x", "y", "both"))

  if (is.null(orientation)) {
    orientation <- .infer_orientation(discrete)
  }

  orientation <- rlang::arg_match(orientation, c("x", "y"))

  list(
    discrete = discrete,
    orientation = orientation
  )
}

# ------------------------------------------------------------------------------
# Theme fragments
# ------------------------------------------------------------------------------

.remove_x_panel_grid <- function() {
  ggplot2::theme(
    panel.grid.major.x = ggplot2::element_line(linetype = 0),
    panel.grid.minor.x = ggplot2::element_line(linetype = 0)
  )
}

.remove_y_panel_grid <- function() {
  ggplot2::theme(
    panel.grid.major.y = ggplot2::element_line(linetype = 0),
    panel.grid.minor.y = ggplot2::element_line(linetype = 0)
  )
}

.remove_x_axis_line <- function() {
  ggplot2::theme(
    axis.line.x.bottom = ggplot2::element_line(linetype = 0),
    axis.line.x.top = ggplot2::element_line(linetype = 0)
  )
}

.remove_y_axis_line <- function() {
  ggplot2::theme(
    axis.line.y.left = ggplot2::element_line(linetype = 0),
    axis.line.y.right = ggplot2::element_line(linetype = 0)
  )
}

.remove_x_axis_ticks <- function() {
  ggplot2::theme(
    axis.ticks.x.bottom = ggplot2::element_line(linetype = 0),
    axis.ticks.x.top = ggplot2::element_line(linetype = 0),
    axis.minor.ticks.x.bottom = ggplot2::element_line(linetype = 0),
    axis.minor.ticks.x.top = ggplot2::element_line(linetype = 0)
  )
}

.remove_y_axis_ticks <- function() {
  ggplot2::theme(
    axis.ticks.y.left = ggplot2::element_line(linetype = 0),
    axis.ticks.y.right = ggplot2::element_line(linetype = 0),
    axis.minor.ticks.y.left = ggplot2::element_line(linetype = 0),
    axis.minor.ticks.y.right = ggplot2::element_line(linetype = 0)
  )
}

.remove_x_axis_text <- function() {
  ggplot2::theme(
    axis.text.x.top = ggplot2::element_blank(),
    axis.text.x.bottom = ggplot2::element_blank()
  )
}

.remove_y_axis_text <- function() {
  ggplot2::theme(
    axis.text.y.left = ggplot2::element_blank(),
    axis.text.y.right = ggplot2::element_blank()
  )
}

.remove_x_axis_title <- function() {
  ggplot2::theme(
    axis.title.x.top = ggplot2::element_blank(),
    axis.title.x.bottom = ggplot2::element_blank()
  )
}

.remove_y_axis_title <- function() {
  ggplot2::theme(
    axis.title.y.left = ggplot2::element_blank(),
    axis.title.y.right = ggplot2::element_blank()
  )
}

# ------------------------------------------------------------------------------
# Axis policies (prefixes)
# ------------------------------------------------------------------------------

.apply_axis_policy <- function(theme, axis_mode, discrete, orientation) {
  axis_mode <- rlang::arg_match(
    axis_mode,
    c("classic", "modern", "minimal", "void")
  )

  if (axis_mode == "classic") {
    if (discrete %in% c("x", "both")) {
      theme <- theme + .remove_x_axis_ticks()
    }

    if (discrete %in% c("y", "both")) {
      theme <- theme + .remove_y_axis_ticks()
    }

    return(theme)
  }

  if (axis_mode == "modern") {
    if (orientation == "x") {
      theme <- theme +
        .remove_y_axis_line() +
        .remove_y_axis_ticks()
    }

    if (orientation == "y") {
      theme <- theme +
        .remove_x_axis_line() +
        .remove_x_axis_ticks()
    }

    if (discrete %in% c("x", "both")) {
      theme <- theme + .remove_x_axis_ticks()
    }

    if (discrete %in% c("y", "both")) {
      theme <- theme + .remove_y_axis_ticks()
    }

    return(theme)
  }

  if (axis_mode == "minimal") {
    theme <- theme +
      .remove_x_axis_line() +
      .remove_y_axis_line() +
      .remove_x_axis_ticks() +
      .remove_y_axis_ticks()

    return(theme)
  }

  if (axis_mode == "void") {
    theme <- theme +
      .remove_x_axis_line() +
      .remove_y_axis_line() +
      .remove_x_axis_ticks() +
      .remove_y_axis_ticks() +
      .remove_x_axis_text() +
      .remove_y_axis_text() +
      .remove_x_axis_title() +
      .remove_y_axis_title()

    return(theme)
  }

  theme
}

# ------------------------------------------------------------------------------
# Grid policies (suffixes)
# ------------------------------------------------------------------------------

.apply_grid_policy <- function(theme, grid_mode, discrete, orientation) {
  grid_mode <- rlang::arg_match(
    grid_mode,
    c("keep", "drift", "flow", "drop")
  )

  if (grid_mode == "keep") {
    return(theme)
  }

  if (grid_mode == "drift") {
    if (discrete != "none") {
      if (orientation == "x") {
        theme <- theme + .remove_x_panel_grid()
      }

      if (orientation == "y") {
        theme <- theme + .remove_y_panel_grid()
      }
    }

    return(theme)
  }

  if (grid_mode == "flow") {
    if (orientation == "x") {
      theme <- theme + .remove_x_panel_grid()
    }

    if (orientation == "y") {
      theme <- theme + .remove_y_panel_grid()
    }

    return(theme)
  }

  if (grid_mode == "drop") {
    theme <- theme +
      .remove_x_panel_grid() +
      .remove_y_panel_grid()

    return(theme)
  }

  theme
}

# ------------------------------------------------------------------------------
# Composition helper
# ------------------------------------------------------------------------------

.compose_refine <- function(axis_mode, grid_mode, discrete, orientation) {
  args <- .validate_refine_args(discrete = discrete, orientation = orientation)

  theme <- ggplot2::theme()

  theme <- .apply_axis_policy(
    theme = theme,
    axis_mode = axis_mode,
    discrete = args$discrete,
    orientation = args$orientation
  )

  theme <- .apply_grid_policy(
    theme = theme,
    grid_mode = grid_mode,
    discrete = args$discrete,
    orientation = args$orientation
  )

  theme
}

# ------------------------------------------------------------------------------
# refine_* family (internal)
#
# These were previously individually exported (refine_classic_keep,
# refine_modern_drift, etc). They are now internal implementation details,
# dispatched to by the exported refine_axis_grid() below. Kept as separate
# functions (rather than inlined) so each axis_style/grid_style combination
# stays independently readable, documented, and testable.
# ------------------------------------------------------------------------------

# -- classic family -----------------------------------------------------------

# Removes axis ticks on discrete axes and leaves panel gridlines unchanged.
.refine_classic_keep <- function(discrete, orientation = NULL) {
  .compose_refine("classic", "keep", discrete, orientation)
}

# Removes axis ticks on discrete axes. Removes panel gridlines on the
# orientation axis only when at least one axis is discrete.
.refine_classic_drift <- function(discrete, orientation = NULL) {
  .compose_refine("classic", "drift", discrete, orientation)
}

# Removes axis ticks on discrete axes and removes panel gridlines on the
# orientation axis.
.refine_classic_flow <- function(discrete, orientation = NULL) {
  .compose_refine("classic", "flow", discrete, orientation)
}

# Removes axis ticks on discrete axes and removes all panel gridlines.
.refine_classic_drop <- function(discrete, orientation = NULL) {
  .compose_refine("classic", "drop", discrete, orientation)
}

# -- modern family --------------------------------------------------------

# Removes axis lines, ticks, and minor ticks from the non-orientation axis.
# Axis ticks on discrete axes are removed. Panel gridlines are left unchanged.
.refine_modern_keep <- function(discrete, orientation = NULL) {
  .compose_refine("modern", "keep", discrete, orientation)
}

# Removes axis lines, ticks, and minor ticks from the non-orientation axis.
# Axis ticks on discrete axes are removed. Removes panel gridlines on the
# orientation axis only when at least one axis is discrete.
.refine_modern_drift <- function(discrete, orientation = NULL) {
  .compose_refine("modern", "drift", discrete, orientation)
}

# Removes axis lines, ticks, and minor ticks from the non-orientation axis.
# Axis ticks on discrete axes are removed. Removes panel gridlines on the
# orientation axis.
.refine_modern_flow <- function(discrete, orientation = NULL) {
  .compose_refine("modern", "flow", discrete, orientation)
}

# Removes axis lines, ticks, and minor ticks from the non-orientation axis.
# Axis ticks on discrete axes are removed. Removes all panel gridlines.
.refine_modern_drop <- function(discrete, orientation = NULL) {
  .compose_refine("modern", "drop", discrete, orientation)
}

# -- minimal family -------------------------------------------------------

# Removes all axis lines, ticks, and minor ticks. Panel gridlines are left
# unchanged.
.refine_minimal_keep <- function(discrete, orientation = NULL) {
  .compose_refine("minimal", "keep", discrete, orientation)
}

# Removes all axis lines, ticks, and minor ticks. Removes panel gridlines on
# the orientation axis only when at least one axis is discrete.
.refine_minimal_drift <- function(discrete, orientation = NULL) {
  .compose_refine("minimal", "drift", discrete, orientation)
}

# Removes all axis lines, ticks, and minor ticks. Removes panel gridlines on
# the orientation axis.
.refine_minimal_flow <- function(discrete, orientation = NULL) {
  .compose_refine("minimal", "flow", discrete, orientation)
}

# Removes all axis lines, ticks, and minor ticks. Removes all panel gridlines.
.refine_minimal_drop <- function(discrete, orientation = NULL) {
  .compose_refine("minimal", "drop", discrete, orientation)
}

# -- void family ------------------------------------------------------------

# Removes all axis lines, ticks, and minor ticks, and removes all axis text
# and axis titles. Panel gridlines are left unchanged.
.refine_void_keep <- function(discrete, orientation = NULL) {
  .compose_refine("void", "keep", discrete, orientation)
}

# Removes all axis lines, ticks, and minor ticks, and removes all axis text
# and axis titles. Removes panel gridlines on the orientation axis only when
# at least one axis is discrete.
.refine_void_drift <- function(discrete, orientation = NULL) {
  .compose_refine("void", "drift", discrete, orientation)
}

# Removes all axis lines, ticks, and minor ticks, and removes all axis text
# and axis titles. Removes panel gridlines on the orientation axis.
.refine_void_flow <- function(discrete, orientation = NULL) {
  .compose_refine("void", "flow", discrete, orientation)
}

# Removes all axis lines, ticks, and minor ticks, and removes all axis text
# and axis titles. Removes all panel gridlines.
.refine_void_drop <- function(discrete, orientation = NULL) {
  .compose_refine("void", "drop", discrete, orientation)
}

# ------------------------------------------------------------------------------
# Exported master function
# ------------------------------------------------------------------------------

#' Refine plot axes and panel grid
#'
#' A single entry point over the `axis_mode` x `grid_mode` combinations
#' previously exposed as sixteen separate `refine_*_*()` functions. Those
#' functions still exist internally (e.g. `.refine_modern_drift()`) and do
#' the actual work; this function just dispatches to the right one.
#'
#' `axis_mode` controls what happens to axis lines, ticks, text, and titles:
#' \describe{
#'   \item{classic}{Removes ticks on discrete axes only.}
#'   \item{modern}{Like `classic`, plus removes the axis line and ticks on
#'     the non-orientation axis.}
#'   \item{minimal}{Removes all axis lines and ticks, regardless of
#'     orientation or discreteness.}
#'   \item{void}{Like `minimal`, plus removes all axis text and titles.}
#' }
#'
#' `grid_mode` controls what happens to panel gridlines:
#' \describe{
#'   \item{keep}{Leaves panel gridlines unchanged.}
#'   \item{drift}{Removes gridlines on the orientation axis, but only when
#'     at least one axis is discrete.}
#'   \item{flow}{Removes gridlines on the orientation axis unconditionally.}
#'   \item{drop}{Removes all panel gridlines.}
#' }
#'
#' @param discrete Character. Which axes should be treated as discrete for
#'   refinement purposes: `"none"`, `"x"`, `"y"`, or `"both"`. Required.
#' @param orientation Character. The primary axis of interest: `"x"` or
#'   `"y"`. Affects `grid_mode` values `"drift"` and `"flow"`. If `NULL`
#'   (default), it is inferred from `discrete`: `"y"` gives `"y"`, otherwise
#'   `"x"`.
#' @param axis_mode Character. One of `"classic"`, `"modern"`, `"minimal"`,
#'   `"void"`. Default `"modern"`.
#' @param grid_mode Character. One of `"keep"`, `"drift"`, `"flow"`,
#'   `"drop"`. Default `"drift"`.
#'
#' @return A ggplot2 theme object.
#' @noRd
refine_axis_grid <- function(
    discrete,
    orientation = NULL,
    axis_mode = "modern",
    grid_mode = "drift"
) {
  axis_mode <- rlang::arg_match(
    axis_mode,
    c("classic", "modern", "minimal", "void")
  )
  grid_mode <- rlang::arg_match(
    grid_mode,
    c("keep", "drift", "flow", "drop")
  )

  refine_fn <- get(
    paste0(".refine_", axis_mode, "_", grid_mode),
    mode = "function"
  )

  refine_fn(discrete = discrete, orientation = orientation)
}
