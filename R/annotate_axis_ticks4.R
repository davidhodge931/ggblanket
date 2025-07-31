#' Annotated axis ticks segments
#'
#' @description Replace axis ticks with annotated segments using absolute measurements.
#' This function only works when panel dimensions are set uniformly via panel.widths and panel.heights.
#' It requires a `coord` of `ggplot2::coord_cartesian(clip = "off")`.
#'
#' @param ... Provided to require argument naming, support trailing commas etc.
#' @param x_breaks A vector of x-axis breaks for bottom/top axis ticks. If NULL, no x-axis ticks are drawn.
#' @param y_breaks A vector of y-axis breaks for left/right axis ticks. If NULL, no y-axis ticks are drawn.
#' @param x_position The position of the x-axis. One of "bottom" or "top". Defaults "bottom".
#' @param y_position The position of the y-axis. One of "left" or "right". Defaults "left".
#' @param colour The colour of the annotated segment. Inherits from the current theme axis.ticks etc.
#' @param linewidth The linewidth of the annotated segment. Inherits from the current theme axis.ticks etc.
#' @param length The absolute length of the annotated segment as a grid unit. Defaults to unit(11/3, "pt").
#' @param theme_elements What to do with theme axis tick elements. Either "transparent", "keep" or "blank". Defaults "transparent".
#'
#' @return A list of a annotate layer and theme elements.
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(stringr)
#' library(palmerpenguins)
#'
#' set_blanket(
#'   theme = theme_lighter(
#'     panel_heights = rep(unit(50, "mm"), 100),
#'     panel_widths = rep(unit(75, "mm"), 100),
#'   ),
#' )
#'
#' penguins |>
#'   tidyr::drop_na(sex) |>
#'   mutate(across(sex, \(x) str_to_sentence(x))) |>
#'   gg_histogram(
#'     x = flipper_length_mm,
#'     col = sex,
#'   ) +
#'   annotate_axis_ticks(x_breaks = c(185, 195))
#'
#' penguins |>
#'   tidyr::drop_na(sex) |>
#'   mutate(across(sex, \(x) str_to_sentence(x))) |>
#'   gg_blanket(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = sex,
#'   ) +
#'   annotate_axis_ticks(
#'     x_breaks = c(185, 195, 205, 215, 225),
#'     y_breaks = c(3500, 4500, 5500)
#'   ) +
#'   geom_point()
#'
annotate_axis_ticks <- function(
    ...,
    x_breaks = NULL,
    y_breaks = NULL,
    x_position = "bottom",
    y_position = "left",
    colour = NULL,
    linewidth = NULL,
    length = NULL,
    theme_elements = "transparent"
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

  # Return early if no breaks provided
  if (is.null(x_breaks) && is.null(y_breaks)) {
    return(list())
  }

  # Validate panel dimensions
  if (!is.null(x_breaks)) {
    if (is.null(panel_heights)) {
      rlang::abort(
        "panel.heights must be set in theme for x-axis tick annotation"
      )
    }
    if (length(panel_heights) > 1 && length(unique(as.numeric(panel_heights))) > 1) {
      rlang::abort(
        "Different panel heights set. This function only works with uniform panel dimensions."
      )
    }
  }

  if (!is.null(y_breaks)) {
    if (is.null(panel_widths)) {
      rlang::abort(
        "panel.widths must be set in theme for y-axis tick annotation"
      )
    }
    if (length(panel_widths) > 1 && length(unique(as.numeric(panel_widths))) > 1) {
      rlang::abort(
        "Different panel widths set. This function only works with uniform panel dimensions."
      )
    }
  }

  stamp <- list()

  # Process x-axis ticks
  if (!is.null(x_breaks)) {
    stamp <- c(stamp, create_axis_ticks(
      breaks = x_breaks,
      position = x_position,
      is_x_axis = TRUE,
      current_theme = current_theme,
      colour = colour,
      linewidth = linewidth,
      length = length,
      theme_elements = theme_elements
    ))
  }

  # Process y-axis ticks
  if (!is.null(y_breaks)) {
    stamp <- c(stamp, create_axis_ticks(
      breaks = y_breaks,
      position = y_position,
      is_x_axis = FALSE,
      current_theme = current_theme,
      colour = colour,
      linewidth = linewidth,
      length = length,
      theme_elements = theme_elements
    ))
  }

  return(stamp)
}

# Helper function to create axis ticks for a single axis
create_axis_ticks <- function(breaks, position, is_x_axis, current_theme,
                              colour, linewidth, length, theme_elements) {

  # Extract theme properties for axis ticks
  if (rlang::is_null(length)) {
    if (is_x_axis) {
      length <- if (position == "bottom") {
        current_theme$axis.ticks.length.x.bottom %||%
          current_theme$axis.ticks.length.x %||%
          current_theme$axis.ticks.length %||%
          grid::unit(11 / 3, "pt")
      } else {
        current_theme$axis.ticks.length.x.top %||%
          current_theme$axis.ticks.length.x %||%
          current_theme$axis.ticks.length %||%
          grid::unit(11 / 3, "pt")
      }
    } else {
      length <- if (position == "left") {
        current_theme$axis.ticks.length.y.left %||%
          current_theme$axis.ticks.length.y %||%
          current_theme$axis.ticks.length %||%
          grid::unit(11 / 3, "pt")
      } else {
        current_theme$axis.ticks.length.y.right %||%
          current_theme$axis.ticks.length.y %||%
          current_theme$axis.ticks.length %||%
          grid::unit(11 / 3, "pt")
      }
    }
  }

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

  # Create list to store output for this axis
  axis_stamp <- list()

  # Add theme modification if requested
  if (theme_elements == "transparent") {
    theme_element <- if (position == "bottom") {
      ggplot2::theme(axis.ticks.x.bottom = ggplot2::element_line(colour = "transparent"))
    } else if (position == "top") {
      ggplot2::theme(axis.ticks.x.top = ggplot2::element_line(colour = "transparent"))
    } else if (position == "left") {
      ggplot2::theme(axis.ticks.y.left = ggplot2::element_line(colour = "transparent"))
    } else if (position == "right") {
      ggplot2::theme(axis.ticks.y.right = ggplot2::element_line(colour = "transparent"))
    }
    axis_stamp <- c(axis_stamp, list(theme_element))
  } else if (theme_elements == "blank") {
    theme_element <- if (position == "bottom") {
      ggplot2::theme(axis.ticks.x.bottom = ggplot2::element_blank())
    } else if (position == "top") {
      ggplot2::theme(axis.ticks.x.top = ggplot2::element_blank())
    } else if (position == "left") {
      ggplot2::theme(axis.ticks.y.left = ggplot2::element_blank())
    } else if (position == "right") {
      ggplot2::theme(axis.ticks.y.right = ggplot2::element_blank())
    }
    axis_stamp <- c(axis_stamp, list(theme_element))
  }

  # Create annotation with segments at the edge
  for (break_val in breaks) {
    tick_grob <- if (position == "bottom") {
      grid::segmentsGrob(
        x0 = grid::unit(0.5, "npc"),
        x1 = grid::unit(0.5, "npc"),
        y0 = grid::unit(0, "npc"),
        y1 = grid::unit(0, "npc") - length,
        gp = grid::gpar(
          col = colour,
          lwd = linewidth * 72 / 25.4,
          lineend = "butt"
        )
      )
    } else if (position == "top") {
      grid::segmentsGrob(
        x0 = grid::unit(0.5, "npc"),
        x1 = grid::unit(0.5, "npc"),
        y0 = grid::unit(1, "npc"),
        y1 = grid::unit(1, "npc") + length,
        gp = grid::gpar(
          col = colour,
          lwd = linewidth * 72 / 25.4,
          lineend = "butt"
        )
      )
    } else if (position == "left") {
      grid::segmentsGrob(
        x0 = grid::unit(0, "npc"),
        x1 = grid::unit(0, "npc") - length,
        y0 = grid::unit(0.5, "npc"),
        y1 = grid::unit(0.5, "npc"),
        gp = grid::gpar(
          col = colour,
          lwd = linewidth * 72 / 25.4,
          lineend = "butt"
        )
      )
    } else if (position == "right") {
      grid::segmentsGrob(
        x0 = grid::unit(1, "npc"),
        x1 = grid::unit(1, "npc") + length,
        y0 = grid::unit(0.5, "npc"),
        y1 = grid::unit(0.5, "npc"),
        gp = grid::gpar(
          col = colour,
          lwd = linewidth * 72 / 25.4,
          lineend = "butt"
        )
      )
    }

    annotation_position <- if (position == "bottom") {
      list(xmin = break_val, xmax = break_val, ymin = -Inf, ymax = -Inf)
    } else if (position == "top") {
      list(xmin = break_val, xmax = break_val, ymin = Inf, ymax = Inf)
    } else if (position == "left") {
      list(xmin = -Inf, xmax = -Inf, ymin = break_val, ymax = break_val)
    } else if (position == "right") {
      list(xmin = Inf, xmax = Inf, ymin = break_val, ymax = break_val)
    }

    axis_stamp <- c(
      axis_stamp,
      list(
        do.call(
          ggplot2::annotation_custom,
          c(list(grob = tick_grob), annotation_position)
        )
      )
    )
  }

  return(axis_stamp)
}
