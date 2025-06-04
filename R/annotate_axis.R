#' Annotated axis line segment
#'
#' @description Replace axis line with an annotated segment.
#'
#' @param ... Extra parameters passed to `ggplot2::annotate("segment", ...)`.
#' @param position The position of the axis. One of "bottom", "top", "left", or "right".
#' @param colour The colour of the annotated segment.
#' @param linewidth The linewidth of the annotated segment.
#'
#' @return A list of a annotate layer and theme elements.
#' @export
#'
annotate_axis_line <- function(...,
                               position = "bottom",
                               colour = NULL,
                               linewidth = NULL) {

  rlang::inform("Please use this function with ggplot2::coord_cartesian(clip = 'off')")

  if (!position %in% c("bottom", "top", "left", "right")) {
    rlang::abort("position must be one of 'bottom', 'top', 'left', or 'right'")
  }

  # Get current theme
  current_theme <- ggplot2::theme_get()

  # Determine if this is x-axis or y-axis
  is_x_axis <- position %in% c("bottom", "top")

  # Extract theme properties for axis line
  if (rlang::is_null(colour)) {
    if (is_x_axis) {
      colour <- if (position == "bottom") {
        current_theme$axis.line.x.bottom$colour %||%
          current_theme$axis.line.x$colour %||%
          current_theme$axis.line$colour %||%
          "black"
      } else {
        current_theme$axis.line.x.top$colour %||%
          current_theme$axis.line.x$colour %||%
          current_theme$axis.line$colour %||%
          "black"
      }
    } else {
      colour <- if (position == "left") {
        current_theme$axis.line.y.left$colour %||%
          current_theme$axis.line.y$colour %||%
          current_theme$axis.line$colour %||%
          "black"
      } else {
        current_theme$axis.line.y.right$colour %||%
          current_theme$axis.line.y$colour %||%
          current_theme$axis.line$colour %||%
          "black"
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
        x = I(-Inf), xend = I(Inf), y = I(-Inf), yend = I(-Inf),
        colour = colour,
        linewidth = linewidth,
        ...
      ),
      ggplot2::theme(axis.line.x.bottom = ggplot2::element_line(colour = "transparent"))
    )
  } else if (position == "top") {
    stamp <- rlang::list2(
      ggplot2::annotate(
        "segment",
        x = I(-Inf), xend = I(Inf), y = I(Inf), yend = I(Inf),
        colour = colour,
        linewidth = linewidth,
        ...
      ),
      ggplot2::theme(axis.line.x.top = ggplot2::element_line(colour = "transparent"))
    )
  } else if (position == "left") {
    stamp <- rlang::list2(
      ggplot2::annotate(
        "segment",
        x = I(-Inf), xend = I(-Inf), y = I(-Inf), yend = I(Inf),
        colour = colour,
        linewidth = linewidth,
        ...
      ),
      ggplot2::theme(axis.line.y.left = ggplot2::element_line(colour = "transparent"))
    )
  } else if (position == "right") {
    stamp <- rlang::list2(
      ggplot2::annotate(
        "segment",
        x = I(Inf), xend = I(Inf), y = I(-Inf), yend = I(Inf),
        colour = colour,
        linewidth = linewidth,
        ...
      ),
      ggplot2::theme(axis.line.y.right = ggplot2::element_line(colour = "transparent"))
    )
  }

  return(stamp)
}

#' #' Annotated axis ticks segment
#' #'
#' #' @description Replace axis ticks with annotated segments. Note these are of length relative to plot area.
#' #'
#' #' @param breaks A vector of breaks.
#' #' @param ... Extra parameters passed to `ggplot2::annotate("segment", ...)`.
#' #' @param position The position of the axis. One of "bottom" or "top".
#' #' @param colour The colour of the annotated segment.
#' #' @param linewidth The linewidth of the annotated segment.
#' #' @param length The length of the annotated segment, relative to the plot area. Defaults to 0.02.
#' #'
#' #' @return A list of a annotate layer and theme elements.
#' #' @export
#' #'
#' annotate_axis_ticks <- function(breaks = NULL,
#'                                 ...,
#'                                 position = "bottom",
#'                                 colour = NULL,
#'                                 linewidth = NULL,
#'                                 length = NULL) {
#'
#'   rlang::inform("Please use this function with ggplot2::coord_cartesian(clip = 'off')")
#'
#'   if (rlang::is_null(breaks)) {
#'     rlang::abort("breaks must be provided")
#'   }
#'
#'   # if (!position %in% c("bottom", "top", "left", "right")) {
#'   #   rlang::abort("position must be one of 'bottom', 'top', 'left', or 'right'")
#'   # }
#'   if (!position %in% c("bottom", "top")) {
#'     rlang::abort("position must be one of 'bottom' or 'top'")
#'   }
#'
#'   # Get current theme
#'   current_theme <- ggplot2::theme_get()
#'
#'   # Determine if this is x-axis or y-axis
#'   is_x_axis <- position %in% c("bottom", "top")
#'
#'   # Extract theme properties for axis ticks
#'   if (rlang::is_null(colour)) {
#'     if (is_x_axis) {
#'       colour <- if (position == "bottom") {
#'         current_theme$axis.ticks.x.bottom$colour %||%
#'           current_theme$axis.ticks.x$colour %||%
#'           current_theme$axis.ticks$colour %||%
#'           "black"
#'       } else {
#'         current_theme$axis.ticks.x.top$colour %||%
#'           current_theme$axis.ticks.x$colour %||%
#'           current_theme$axis.ticks$colour %||%
#'           "black"
#'       }
#'     } else {
#'       colour <- if (position == "left") {
#'         current_theme$axis.ticks.y.left$colour %||%
#'           current_theme$axis.ticks.y$colour %||%
#'           current_theme$axis.ticks$colour %||%
#'           "black"
#'       } else {
#'         current_theme$axis.ticks.y.right$colour %||%
#'           current_theme$axis.ticks.y$colour %||%
#'           current_theme$axis.ticks$colour %||%
#'           "black"
#'       }
#'     }
#'   }
#'
#'   if (rlang::is_null(linewidth)) {
#'     if (is_x_axis) {
#'       linewidth <- if (position == "bottom") {
#'         current_theme$axis.ticks.x.bottom$linewidth %||%
#'           current_theme$axis.ticks.x$linewidth %||%
#'           current_theme$axis.ticks$linewidth %||%
#'           0.5
#'       } else {
#'         current_theme$axis.ticks.x.top$linewidth %||%
#'           current_theme$axis.ticks.x$linewidth %||%
#'           current_theme$axis.ticks$linewidth %||%
#'           0.5
#'       }
#'     } else {
#'       linewidth <- if (position == "left") {
#'         current_theme$axis.ticks.y.left$linewidth %||%
#'           current_theme$axis.ticks.y$linewidth %||%
#'           current_theme$axis.ticks$linewidth %||%
#'           0.5
#'       } else {
#'         current_theme$axis.ticks.y.right$linewidth %||%
#'           current_theme$axis.ticks.y$linewidth %||%
#'           current_theme$axis.ticks$linewidth %||%
#'           0.5
#'       }
#'     }
#'   }
#'
#'   # Use fixed default length if not provided
#'   if (rlang::is_null(length)) {
#'     length <- 0.02  # 2% of plot area - reasonable default for tick length
#'   }
#'
#'   # Create appropriate segment and theme based on position
#'   if (position == "bottom") {
#'     stamp <- rlang::list2(
#'       ggplot2::theme(axis.ticks.x.bottom = ggplot2::element_line(colour = "transparent")),
#'       ggplot2::annotate("segment",
#'                         x = breaks, xend = breaks,
#'                         y = I(0), yend = I(-length),
#'                         colour = colour,
#'                         linewidth = linewidth,
#'                         ...)
#'     )
#'   } else if (position == "top") {
#'     stamp <- rlang::list2(
#'       ggplot2::theme(axis.ticks.x.top = ggplot2::element_line(colour = "transparent")),
#'       ggplot2::annotate("segment",
#'                         x = breaks, xend = breaks,
#'                         y = I(1), yend = I(1 + length),
#'                         colour = colour,
#'                         linewidth = linewidth,
#'                         ...)
#'     )
#'   } else if (position == "left") {
#'     stamp <- rlang::list2(
#'       ggplot2::theme(axis.ticks.y.left = ggplot2::element_line(colour = "transparent")),
#'       ggplot2::annotate("segment",
#'                         x = I(0), xend = I(-length),
#'                         y = breaks, yend = breaks,
#'                         colour = colour,
#'                         linewidth = linewidth,
#'                         ...)
#'     )
#'   } else if (position == "right") {
#'     stamp <- rlang::list2(
#'       ggplot2::theme(axis.ticks.y.right = ggplot2::element_line(colour = "transparent")),
#'       ggplot2::annotate("segment",
#'                         x = I(1), xend = I(1 + length),
#'                         y = breaks, yend = breaks,
#'                         colour = colour,
#'                         linewidth = linewidth,
#'                         ...)
#'     )
#'   }
#'
#'   return(stamp)
#' }
