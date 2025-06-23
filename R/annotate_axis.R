#' Annotated axis line segment
#'
#' @description Replace axis line with an annotated segment.
#'
#' @param position The position of the axis. One of "bottom", "top", "left", or "right".
#' @param ... Extra parameters passed to `ggplot2::annotate("segment", ...)`.
#' @param colour The colour of the annotated segment. Inherits from the current theme axis.line etc.
#' @param linewidth The linewidth of the annotated segment. Inherits from the current theme axis.line etc.
#'
#' @return A list of a annotate layer and theme elements.
#' @export
#'
annotate_axis_line <- function(position, ..., colour = NULL, linewidth = NULL) {
  rlang::inform(
    "Please use this function with ggplot2::coord_cartesian(clip = 'off')"
  )

  if (!position %in% c("bottom", "top", "left", "right")) {
    rlang::abort("position must be one of 'bottom', 'top', 'left', or 'right'")
  }

  # Get current theme
  current_theme <- ggplot2::get_theme()

  # Determine if this is x-axis or y-axis
  is_x_axis <- position %in% c("bottom", "top")

  # Extract theme properties for axis line
  if (rlang::is_null(colour)) {
    if (is_x_axis) {
      colour <- if (position == "bottom") {
        current_theme$axis.line.x.bottom$colour %||%
          current_theme$axis.line.x$colour %||%
          current_theme$axis.line$colour %||%
          "#121B24FF"
      } else {
        current_theme$axis.line.x.top$colour %||%
          current_theme$axis.line.x$colour %||%
          current_theme$axis.line$colour %||%
          "#121B24FF"
      }
    } else {
      colour <- if (position == "left") {
        current_theme$axis.line.y.left$colour %||%
          current_theme$axis.line.y$colour %||%
          current_theme$axis.line$colour %||%
          "#121B24FF"
      } else {
        current_theme$axis.line.y.right$colour %||%
          current_theme$axis.line.y$colour %||%
          current_theme$axis.line$colour %||%
          "#121B24FF"
      }
    }
  }

  if (rlang::is_null(linewidth)) {
    if (is_x_axis) {
      linewidth <- if (position == "bottom") {
        current_theme$axis.line.x.bottom$linewidth %||%
          current_theme$axis.line.x$linewidth %||%
          current_theme$axis.line$linewidth %||%
          0.5
      } else {
        current_theme$axis.line.x.top$linewidth %||%
          current_theme$axis.line.x$linewidth %||%
          current_theme$axis.line$linewidth %||%
          0.5
      }
    } else {
      linewidth <- if (position == "left") {
        current_theme$axis.line.y.left$linewidth %||%
          current_theme$axis.line.y$linewidth %||%
          current_theme$axis.line$linewidth %||%
          0.5
      } else {
        current_theme$axis.line.y.right$linewidth %||%
          current_theme$axis.line.y$linewidth %||%
          current_theme$axis.line$linewidth %||%
          0.5
      }
    }
  }

  # Create appropriate segment and theme based on position
  if (position == "bottom") {
    stamp <- rlang::list2(
      ggplot2::annotate(
        "segment",
        x = I(-Inf),
        xend = I(Inf),
        y = I(-Inf),
        yend = I(-Inf),
        colour = colour,
        linewidth = linewidth,
        ...
      ),
      ggplot2::theme(
        axis.line.x.bottom = ggplot2::element_line(colour = "transparent")
      )
    )
  } else if (position == "top") {
    stamp <- rlang::list2(
      ggplot2::annotate(
        "segment",
        x = I(-Inf),
        xend = I(Inf),
        y = I(Inf),
        yend = I(Inf),
        colour = colour,
        linewidth = linewidth,
        ...
      ),
      ggplot2::theme(
        axis.line.x.top = ggplot2::element_line(colour = "transparent")
      )
    )
  } else if (position == "left") {
    stamp <- rlang::list2(
      ggplot2::annotate(
        "segment",
        x = I(-Inf),
        xend = I(-Inf),
        y = I(-Inf),
        yend = I(Inf),
        colour = colour,
        linewidth = linewidth,
        ...
      ),
      ggplot2::theme(
        axis.line.y.left = ggplot2::element_line(colour = "transparent")
      )
    )
  } else if (position == "right") {
    stamp <- rlang::list2(
      ggplot2::annotate(
        "segment",
        x = I(Inf),
        xend = I(Inf),
        y = I(-Inf),
        yend = I(Inf),
        colour = colour,
        linewidth = linewidth,
        ...
      ),
      ggplot2::theme(
        axis.line.y.right = ggplot2::element_line(colour = "transparent")
      )
    )
  }

  return(stamp)
}

#' Annotated axis ticks segments
#'
#' @description Replace axis ticks with annotated segments using absolute measurements.
#' This function only works when panel dimensions are set uniformly via panel.widths and panel.heights.
#'
#' @param breaks A vector of breaks.
#' @param position The position of the axis. One of "bottom", "top", "left" or "right".
#' @param ... Extra parameters passed to `ggplot2::annotate("segment", ...)`.
#' @param colour The colour of the annotated segment. Inherits from the current theme axis.ticks etc.
#' @param linewidth The linewidth of the annotated segment. Inherits from the current theme axis.ticks etc.
#' @param length The absolute length of the annotated segment as a grid unit. Defaults to unit(11/3, "pt").
#'
#' @return A list of a annotate layer and theme elements.
#' @export
#'
annotate_axis_ticks <- function(
    position,
    breaks,
    ...,
    colour = NULL,
    linewidth = NULL,
    length = NULL
  ) {
  rlang::inform(
    "Please use this function with ggplot2::coord_cartesian(clip = 'off')"
  )

  # Check if panel dimensions are set
  current_theme <- ggplot2::get_theme()
  panel_widths <- current_theme$panel.widths
  panel_heights <- current_theme$panel.heights

  if (is.null(panel_widths) && is.null(panel_heights)) {
    rlang::abort(
      "This function only works when panel dimensions are explicitly set via theme(panel.widths = ..., panel.heights = ...)"
    )
  }

  # Determine if this is x-axis or y-axis
  is_x_axis <- position %in% c("bottom", "top")

  # Set default length from current theme
  if (rlang::is_null(length)) {
    if (is_x_axis) {
      length <- if (position == "bottom") {
        current_theme$axis.ticks.x.bottom$length %||%
          current_theme$axis.ticks.x$length %||%
          current_theme$axis.ticks$length %||%
          grid::unit(11 / 3, "pt")
      } else {
        current_theme$axis.ticks.x.top$length %||%
          current_theme$axis.ticks.x$length %||%
          current_theme$axis.ticks$length %||%
          grid::unit(11 / 3, "pt")
      }
    } else {
      length <- if (position == "left") {
        current_theme$axis.ticks.y.left$length %||%
          current_theme$axis.ticks.y$length %||%
          current_theme$axis.ticks$length %||%
          grid::unit(11 / 3, "pt")
      } else {
        current_theme$axis.ticks.y.right$length %||%
          current_theme$axis.ticks.y$length %||%
          current_theme$axis.ticks$length %||%
          grid::unit(11 / 3, "pt")
      }
    }
  }

  # Determine if this is x-axis or y-axis
  is_x_axis <- position %in% c("bottom", "top")

  # Get the relevant panel dimension
  if (is_x_axis) {
    if (is.null(panel_heights)) {
      rlang::abort(
        "panel.heights must be set in theme for horizontal axis tick annotation"
      )
    }
    if (
      length(panel_heights) > 1 && length(unique(as.numeric(panel_heights))) > 1
    ) {
      rlang::abort(
        "Different panel heights set. This function only works with uniform panel dimensions."
      )
    }
    panel_dimension <- panel_heights[1] # Use first panel height
  } else {
    if (is.null(panel_widths)) {
      rlang::abort(
        "panel.widths must be set in theme for vertical axis tick annotation"
      )
    }
    if (
      length(panel_widths) > 1 && length(unique(as.numeric(panel_widths))) > 1
    ) {
      rlang::abort(
        "Different panel widths set. This function only works with uniform panel dimensions."
      )
    }
    panel_dimension <- panel_widths[1] # Use first panel width
  }

  # Convert absolute length to relative proportion of panel
  # Convert both to same units for calculation
  length_mm <- grid::convertUnit(length, "mm", valueOnly = TRUE)
  panel_mm <- grid::convertUnit(panel_dimension, "mm", valueOnly = TRUE)
  relative_length <- length_mm / panel_mm

  # Extract theme properties for axis ticks
  if (rlang::is_null(colour)) {
    if (is_x_axis) {
      colour <- if (position == "bottom") {
        current_theme$axis.ticks.x.bottom$colour %||%
          current_theme$axis.ticks.x$colour %||%
          current_theme$axis.ticks$colour %||%
          "#121B24FF"
      } else {
        current_theme$axis.ticks.x.top$colour %||%
          current_theme$axis.ticks.x$colour %||%
          current_theme$axis.ticks$colour %||%
          "#121B24FF"
      }
    } else {
      colour <- if (position == "left") {
        current_theme$axis.ticks.y.left$colour %||%
          current_theme$axis.ticks.y$colour %||%
          current_theme$axis.ticks$colour %||%
          "#121B24FF"
      } else {
        current_theme$axis.ticks.y.right$colour %||%
          current_theme$axis.ticks.y$colour %||%
          current_theme$axis.ticks$colour %||%
          "#121B24FF"
      }
    }
  }

  if (rlang::is_null(linewidth)) {
    if (is_x_axis) {
      linewidth <- if (position == "bottom") {
        current_theme$axis.ticks.x.bottom$linewidth %||%
          current_theme$axis.ticks.x$linewidth %||%
          current_theme$axis.ticks$linewidth %||%
          0.5
      } else {
        current_theme$axis.ticks.x.top$linewidth %||%
          current_theme$axis.ticks.x$linewidth %||%
          current_theme$axis.ticks$linewidth %||%
          0.5
      }
    } else {
      linewidth <- if (position == "left") {
        current_theme$axis.ticks.y.left$linewidth %||%
          current_theme$axis.ticks.y$linewidth %||%
          current_theme$axis.ticks$linewidth %||%
          0.5
      } else {
        current_theme$axis.ticks.y.right$linewidth %||%
          current_theme$axis.ticks.y$linewidth %||%
          current_theme$axis.ticks$linewidth %||%
          0.5
      }
    }
  }

  # Create appropriate segment and theme based on position
  if (position == "bottom") {
    stamp <- rlang::list2(
      ggplot2::theme(
        axis.ticks.x.bottom = ggplot2::element_line(colour = "transparent")
      ),
      ggplot2::annotate(
        "segment",
        x = breaks,
        xend = breaks,
        y = I(0),
        yend = I(-relative_length),
        colour = colour,
        linewidth = linewidth,
        ...
      )
    )
  } else if (position == "top") {
    stamp <- rlang::list2(
      ggplot2::theme(
        axis.ticks.x.top = ggplot2::element_line(colour = "transparent")
      ),
      ggplot2::annotate(
        "segment",
        x = breaks,
        xend = breaks,
        y = I(1),
        yend = I(1 + relative_length),
        colour = colour,
        linewidth = linewidth,
        ...
      )
    )
  } else if (position == "left") {
    stamp <- rlang::list2(
      ggplot2::theme(
        axis.ticks.y.left = ggplot2::element_line(colour = "transparent")
      ),
      ggplot2::annotate(
        "segment",
        x = I(0),
        xend = I(-relative_length),
        y = breaks,
        yend = breaks,
        colour = colour,
        linewidth = linewidth,
        ...
      )
    )
  } else if (position == "right") {
    stamp <- rlang::list2(
      ggplot2::theme(
        axis.ticks.y.right = ggplot2::element_line(colour = "transparent")
      ),
      ggplot2::annotate(
        "segment",
        x = I(1),
        xend = I(1 + relative_length),
        y = breaks,
        yend = breaks,
        colour = colour,
        linewidth = linewidth,
        ...
      )
    )
  }

  return(stamp)
}
