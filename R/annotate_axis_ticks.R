#' Annotated axis ticks segments
#'
#' @description Replace axis ticks with annotated segments using absolute measurements.
#' This function only works when panel dimensions are set via panel.widths and panel.heights.
#' It requires a `coord` of `ggplot2::coord_cartesian(clip = "off")`.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param axis The axis to annotate. One of "x" or "y".
#' @param breaks A vector of axis breaks for axis ticks.
#' @param position The position of the axis ticks. For x-axis: "bottom" or "top". For y-axis: "left" or "right". Defaults to "bottom" for x-axis and "left" for y-axis.
#' @param colour The colour of the annotated segment. Inherits from the current theme axis.ticks etc.
#' @param linewidth The linewidth of the annotated segment. Inherits from the current theme axis.ticks etc.
#' @param length The absolute length of the annotated segment as a grid unit. Defaults to unit(11/3, "pt").
#' @param theme_element What to do with the equivalent theme element. Either "transparent", "keep" or "blank". Defaults "transparent".
#'
#' @return A list of a annotate layer and theme elements.
#'
#' @export
#'
annotate_axis_ticks <- function(
    ...,
    axis,
    breaks,
    position = NULL,
    colour = NULL,
    linewidth = NULL,
    length = NULL,
    theme_element = "transparent"
) {
  # Validate arguments
  if (!axis %in% c("x", "y")) {
    rlang::abort("axis must be one of 'x' or 'y'")
  }

  # Set default position if not provided
  if (rlang::is_null(position)) {
    position <- if (axis == "x") "bottom" else "left"
  }

  if (axis == "x" && !position %in% c("bottom", "top")) {
    rlang::abort("For x-axis, position must be one of 'bottom' or 'top'")
  }

  if (axis == "y" && !position %in% c("left", "right")) {
    rlang::abort("For y-axis, position must be one of 'left' or 'right'")
  }

  if (!theme_element %in% c("transparent", "keep", "blank")) {
    rlang::abort(
      "theme_element must be one of 'transparent', 'keep', or 'blank'"
    )
  }

  # Get current theme and check panel dimensions
  current_theme <- ggplot2::theme_get()
  panel_widths <- current_theme$panel.widths
  panel_heights <- current_theme$panel.heights

  if (rlang::is_null(panel_widths) && rlang::is_null(panel_heights)) {
    rlang::abort(
      "This function only works when panel dimensions are explicitly set via theme(panel.widths = ..., panel.heights = ...)"
    )
  }

  # Validate panel dimensions for the specific axis
  if (axis == "x") {
    if (rlang::is_null(panel_heights)) {
      rlang::abort(
        "panel.heights must be set in theme for x-axis tick annotation"
      )
    }
    if (
      length(panel_heights) > 1 && length(unique(as.numeric(panel_heights))) > 1
    ) {
      rlang::abort(
        "Different panel heights set. This function only works with uniform panel dimensions."
      )
    }
  } else {
    if (rlang::is_null(panel_widths)) {
      rlang::abort(
        "panel.widths must be set in theme for y-axis tick annotation"
      )
    }
    if (
      length(panel_widths) > 1 && length(unique(as.numeric(panel_widths))) > 1
    ) {
      rlang::abort(
        "Different panel widths set. This function only works with uniform panel dimensions."
      )
    }
  }

  # Build hierarchy for axis ticks from most specific to least specific
  tick_specific <- paste0("axis.ticks.", axis, ".", position)
  tick_axis <- paste0("axis.ticks.", axis)
  tick_general <- "axis.ticks"

  tick_hierarchy <- c(tick_specific, tick_axis, tick_general)

  # Find the first non-blank resolved ticks element
  resolved_tick_element <- tick_hierarchy |>
    purrr::map(\(x) ggplot2::calc_element(x, current_theme, skip_blank = TRUE)) |>
    purrr::detect(\(x) !is.null(x) && !inherits(x, "element_blank"))

  # If still no element found, create a minimal fallback
  if (is.null(resolved_tick_element)) {
    resolved_tick_element <- list(colour = "black", linewidth = 0.5)
  }

  # Build hierarchy for axis ticks length from most specific to least specific
  length_specific <- paste0("axis.ticks.length.", axis, ".", position)
  length_axis <- paste0("axis.ticks.length.", axis)
  length_general <- "axis.ticks.length"

  length_hierarchy <- c(length_specific, length_axis, length_general)

  # Find the first non-blank resolved length element
  resolved_length_element <- length_hierarchy |>
    purrr::map(\(x) ggplot2::calc_element(x, current_theme, skip_blank = TRUE)) |>
    purrr::detect(\(x) !is.null(x) && !inherits(x, "element_blank"))

  # Extract theme properties with proper resolution
  tick_colour <- if (rlang::is_null(colour)) {
    resolved_tick_element$colour %||% "black"
  } else {
    colour
  }

  # Handle linewidth with proper rel() support
  if (rlang::is_null(linewidth)) {
    tick_linewidth <- resolved_tick_element$linewidth %||% 0.5
  } else {
    if (inherits(linewidth, "rel")) {
      # Apply user's rel() to the resolved theme linewidth
      base_linewidth <- resolved_tick_element$linewidth %||% 0.5
      tick_linewidth <- as.numeric(linewidth) * base_linewidth
    } else {
      tick_linewidth <- linewidth
    }
  }

  # Handle length with proper unit and rel() support
  if (rlang::is_null(length)) {
    # Use the resolved length element directly
    length <- resolved_length_element %||% grid::unit(11 / 3, "pt")

    # Handle rel() objects for length
    if (inherits(length, "rel")) {
      # Convert rel() to absolute unit - use default base of 11/3 pt
      length <- grid::unit(as.numeric(length) * 11 / 3, "pt")
    } else if (!inherits(length, "unit")) {
      # Ensure it's a proper unit object
      length <- grid::unit(11 / 3, "pt")
    }
  } else {
    # Handle user-provided length
    if (inherits(length, "rel")) {
      # Get the resolved theme length as base
      theme_length <- resolved_length_element %||% grid::unit(11 / 3, "pt")

      # Convert theme length to numeric points for rel() calculation
      if (inherits(theme_length, "rel")) {
        # If theme length is also rel(), use default base
        base_length_pts <- 11 / 3
      } else if (inherits(theme_length, "unit")) {
        # Convert unit to points
        base_length_pts <- as.numeric(grid::convertUnit(theme_length, "pt"))
      } else {
        base_length_pts <- 11 / 3
      }

      # Apply user's rel() to the base length
      length <- grid::unit(as.numeric(length) * base_length_pts, "pt")
    } else if (!inherits(length, "unit")) {
      # Convert numeric to unit
      length <- grid::unit(length, "pt")
    }
    # If already a unit, use as-is
  }

  stamp <- list()

  # Add theme modification if requested
  if (theme_element == "transparent") {
    theme_element_name <- paste0("axis.ticks.", axis, ".", position)
    theme_mod <- list()
    theme_mod[[theme_element_name]] <- ggplot2::element_line(colour = "transparent")
    stamp <- c(stamp, list(rlang::exec(ggplot2::theme, !!!theme_mod)))
  } else if (theme_element == "blank") {
    theme_element_name <- paste0("axis.ticks.", axis, ".", position)
    theme_mod <- list()
    theme_mod[[theme_element_name]] <- ggplot2::element_blank()
    stamp <- c(stamp, list(rlang::exec(ggplot2::theme, !!!theme_mod)))
  }

  # Create tick annotations
  for (break_val in breaks) {
    if (axis == "x") {
      tick_grob <- if (position == "bottom") {
        grid::segmentsGrob(
          x0 = grid::unit(0.5, "npc"),
          x1 = grid::unit(0.5, "npc"),
          y0 = grid::unit(0, "npc"),
          y1 = grid::unit(0, "npc") - length,
          gp = grid::gpar(
            col = tick_colour,
            lwd = tick_linewidth * 72 / 25.4,
            lineend = "butt"
          )
        )
      } else {
        # top
        grid::segmentsGrob(
          x0 = grid::unit(0.5, "npc"),
          x1 = grid::unit(0.5, "npc"),
          y0 = grid::unit(1, "npc"),
          y1 = grid::unit(1, "npc") + length,
          gp = grid::gpar(
            col = tick_colour,
            lwd = tick_linewidth * 72 / 25.4,
            lineend = "butt"
          )
        )
      }

      annotation_position <- if (position == "bottom") {
        list(xmin = break_val, xmax = break_val, ymin = -Inf, ymax = -Inf)
      } else {
        list(xmin = break_val, xmax = break_val, ymin = Inf, ymax = Inf)
      }
    } else {
      # y-axis
      tick_grob <- if (position == "left") {
        grid::segmentsGrob(
          x0 = grid::unit(0, "npc"),
          x1 = grid::unit(0, "npc") - length,
          y0 = grid::unit(0.5, "npc"),
          y1 = grid::unit(0.5, "npc"),
          gp = grid::gpar(
            col = tick_colour,
            lwd = tick_linewidth * 72 / 25.4,
            lineend = "butt"
          )
        )
      } else {
        # right
        grid::segmentsGrob(
          x0 = grid::unit(1, "npc"),
          x1 = grid::unit(1, "npc") + length,
          y0 = grid::unit(0.5, "npc"),
          y1 = grid::unit(0.5, "npc"),
          gp = grid::gpar(
            col = tick_colour,
            lwd = tick_linewidth * 72 / 25.4,
            lineend = "butt"
          )
        )
      }

      annotation_position <- if (position == "left") {
        list(xmin = -Inf, xmax = -Inf, ymin = break_val, ymax = break_val)
      } else {
        list(xmin = Inf, xmax = Inf, ymin = break_val, ymax = break_val)
      }
    }

    stamp <- c(
      stamp,
      list(
        rlang::exec(
          ggplot2::annotation_custom,
          grob = tick_grob,
          !!!annotation_position
        )
      )
    )
  }

  return(stamp)
}
