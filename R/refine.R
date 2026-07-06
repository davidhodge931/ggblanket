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

#' Modern drift refine
#'
#' Removes axis lines, ticks, and minor ticks from the non-orientationed axis.
#' Axis ticks on discrete axes are removed. Removes panel gridlines on the
#' orientationed axis only when at least one axis is discrete.
#'
#' @param ... Reserved for future extensions. Placed first so later arguments
#'   must be named, and to support trailing commas in calls.
#' @param discrete Character scalar describing which axes should be treated as
#'   discrete for refinement purposes: `"none"`, `"x"`, `"y"`, or `"both"`.
#' @param orientation Character. The primary axis of interest: `"x"` or `"y"`.
#'   This affects grid modes such as `*_drift()` and `*_flow()`. If `NULL`
#'   (default), orientation is inferred from `discrete`: `"y"` gives `"y"`,
#'   otherwise `"x"`.
#'
#' @return A ggplot2 theme object
#' @export
modern_drift <- function(
  ...,
  discrete = "none",
  orientation = NULL
) {
  rlang::check_dots_empty0(...)

  .compose_refine("modern", "drift", discrete, orientation)
}


#' Void drop refine
#'
#' Removes all axis lines, ticks, and minor ticks, and removes all axis text
#' and axis titles. Removes all panel gridlines.
#'
#' @inheritParams modern_drift
#'
#' @return A ggplot2 theme object
#' @export
void_drop <- function(
  ...,
  discrete = "none",
  orientation = NULL
) {
  rlang::check_dots_empty0(...)

  .compose_refine("void", "drop", discrete, orientation)
}
