#' Annotate axis tick segments
#'
#' @description Create annotated segments of the axis ticks.
#'
#' This function is designed to work with a theme that is globally set, so that the annotated tick segments can be made consistent by default.
#'
#' It only works when panel dimensions are set are set in the theme.
#'
#' It requires a `coord` of `ggplot2::coord_cartesian(clip = "off")`.
#'
#' @param position The position of the axis ticks. One of "top", "bottom", "left", or "right".
#' @param ... Require named arguments (and support trailing commas).
#' @param x A vector of x-axis breaks for axis ticks. Cannot be used together with y.
#' @param y A vector of y-axis breaks for axis ticks. Cannot be used together with x.
#' @param colour The colour of the annotated segment. Inherits from the current theme axis.ticks etc.
#' @param linewidth The linewidth of the annotated segment. Inherits from the current theme axis.ticks etc.
#' @param length The absolute length of the annotated segment as a grid unit. Defaults to theme's axis.ticks.length (typically rel(0.66)).
#' @param theme_element What to do with the equivalent theme element. Either "transparent", "keep" or "blank". Defaults "transparent".
#'
#' @return A list of a annotate layer and theme elements.
#'
#' @export
#'
annotate_axis_ticks <- function(
    position,
    ...,
    x = NULL,
    y = NULL,
    colour = NULL,
    linewidth = NULL,
    length = NULL,
    theme_element = "transparent"
) {
  # Validate arguments
  if (!position %in% c("top", "bottom", "left", "right")) {
    rlang::abort("position must be one of 'top', 'bottom', 'left', or 'right'")
  }

  if (is.null(x) && is.null(y)) {
    rlang::abort("Either x or y must be specified")
  }

  if (!is.null(x) && !is.null(y)) {
    rlang::abort("Only one of x or y can be specified")
  }

  if (!theme_element %in% c("transparent", "keep", "blank")) {
    rlang::abort(
      "theme_element must be one of 'transparent', 'keep', or 'blank'"
    )
  }

  # Determine axis from position
  axis <- if (position %in% c("top", "bottom")) "x" else "y"

  # Get breaks - keep original for positioning
  breaks <- if (!is.null(x)) x else y

  # Check for empty breaks
  if (length(breaks) == 0) {
    return(list())
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

  # Use calc_element to properly resolve tick properties with inheritance
  resolved_tick_element <- NULL
  for (element_name in c(tick_specific, tick_axis, tick_general)) {
    element <- ggplot2::calc_element(element_name, current_theme, skip_blank = TRUE)
    if (!is.null(element) && !inherits(element, "element_blank")) {
      resolved_tick_element <- element
      break
    }
  }

  # If still no element found, create a minimal fallback
  if (is.null(resolved_tick_element)) {
    resolved_tick_element <- list(colour = "black", linewidth = 0.5)
  }

  # Build hierarchy for axis ticks length from most specific to least specific
  length_specific <- paste0("axis.ticks.length.", axis, ".", position)
  length_axis <- paste0("axis.ticks.length.", axis)
  length_general <- "axis.ticks.length"

  # Use calc_element to properly resolve length with inheritance and rel() handling
  resolved_length_element <- NULL
  for (element_name in c(length_specific, length_axis, length_general)) {
    element <- ggplot2::calc_element(element_name, current_theme, skip_blank = TRUE)
    if (!is.null(element) && !inherits(element, "element_blank")) {
      resolved_length_element <- element
      break
    }
  }

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

  # Handle length with proper unit and rel() support - FIXED VERSION
  if (rlang::is_null(length)) {
    # Use the resolved length element from calc_element
    tick_length <- resolved_length_element

    if (is.null(tick_length)) {
      # Fallback: manually calculate default rel(0.66)
      base_size <- current_theme$text$size %||% 11
      tick_length <- grid::unit(0.66 * base_size, "pt")
    } else if (inherits(tick_length, "rel")) {
      # calc_element returned a rel() object - we need to convert it
      base_size <- current_theme$text$size %||% 11
      tick_length <- grid::unit(as.numeric(tick_length) * base_size, "pt")
    } else if (!inherits(tick_length, "unit")) {
      # If calc_element didn't return a unit or rel, convert
      if (is.numeric(tick_length)) {
        tick_length <- grid::unit(tick_length, "pt")
      } else {
        # Ultimate fallback
        base_size <- current_theme$text$size %||% 11
        tick_length <- grid::unit(0.66 * base_size, "pt")
      }
    }
    # If tick_length is already a proper unit from calc_element, use as-is
  } else {
    # Handle user-provided length
    if (inherits(length, "rel")) {
      # Get the base length for rel() calculation - this should be the THEME value before conversion
      base_length_for_rel <- NULL

      # First try to get the raw theme element (before calc_element processing)
      for (element_name in c(length_specific, length_axis, length_general)) {
        raw_element <- current_theme[[element_name]]
        if (!is.null(raw_element) && !inherits(raw_element, "element_blank")) {
          base_length_for_rel <- raw_element
          break
        }
      }

      if (!is.null(base_length_for_rel)) {
        if (inherits(base_length_for_rel, "rel")) {
          # Theme is also rel() - apply user's rel to the theme's rel
          base_size <- current_theme$text$size %||% 11
          theme_abs_length <- as.numeric(base_length_for_rel) * base_size
          tick_length <- grid::unit(as.numeric(length) * theme_abs_length, "pt")
        } else if (inherits(base_length_for_rel, "unit")) {
          # Theme is absolute unit - apply user's rel to that
          theme_abs_length <- as.numeric(grid::convertUnit(base_length_for_rel, "pt"))
          tick_length <- grid::unit(as.numeric(length) * theme_abs_length, "pt")
        } else if (is.numeric(base_length_for_rel)) {
          # Theme is numeric - assume points
          tick_length <- grid::unit(as.numeric(length) * base_length_for_rel, "pt")
        } else {
          # Fallback to default base
          base_size <- current_theme$text$size %||% 11
          default_length <- 0.66 * base_size
          tick_length <- grid::unit(as.numeric(length) * default_length, "pt")
        }
      } else {
        # No theme element found - use default rel(0.66) as base
        base_size <- current_theme$text$size %||% 11
        default_length <- 0.66 * base_size
        tick_length <- grid::unit(as.numeric(length) * default_length, "pt")
      }
    } else if (inherits(length, "unit")) {
      # If already a unit, use as-is
      tick_length <- length
    } else if (is.numeric(length)) {
      # Convert numeric to unit (assume points)
      tick_length <- grid::unit(length, "pt")
    } else {
      # Fallback
      base_size <- current_theme$text$size %||% 11
      tick_length <- grid::unit(0.66 * base_size, "pt")
    }
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
  tick_annotations <- breaks |>
    purrr::imap(\(break_val, i) {
      tick_grob <- if (position == "bottom") {
        grid::segmentsGrob(
          x0 = grid::unit(0.5, "npc"),
          x1 = grid::unit(0.5, "npc"),
          y0 = grid::unit(0, "npc"),
          y1 = grid::unit(0, "npc") - tick_length,
          gp = grid::gpar(
            col = tick_colour,
            lwd = tick_linewidth * 72 / 25.4,
            lineend = "butt"
          )
        )
      } else if (position == "top") {
        grid::segmentsGrob(
          x0 = grid::unit(0.5, "npc"),
          x1 = grid::unit(0.5, "npc"),
          y0 = grid::unit(1, "npc"),
          y1 = grid::unit(1, "npc") + tick_length,
          gp = grid::gpar(
            col = tick_colour,
            lwd = tick_linewidth * 72 / 25.4,
            lineend = "butt"
          )
        )
      } else if (position == "left") {
        grid::segmentsGrob(
          x0 = grid::unit(0, "npc"),
          x1 = grid::unit(0, "npc") - tick_length,
          y0 = grid::unit(0.5, "npc"),
          y1 = grid::unit(0.5, "npc"),
          gp = grid::gpar(
            col = tick_colour,
            lwd = tick_linewidth * 72 / 25.4,
            lineend = "butt"
          )
        )
      } else {  # right
        grid::segmentsGrob(
          x0 = grid::unit(1, "npc"),
          x1 = grid::unit(1, "npc") + tick_length,
          y0 = grid::unit(0.5, "npc"),
          y1 = grid::unit(0.5, "npc"),
          gp = grid::gpar(
            col = tick_colour,
            lwd = tick_linewidth * 72 / 25.4,
            lineend = "butt"
          )
        )
      }

      # Set annotation position based on axis and position
      if (axis == "x") {
        annotation_position <- if (position == "bottom") {
          list(xmin = break_val, xmax = break_val, ymin = -Inf, ymax = -Inf)
        } else {  # top
          list(xmin = break_val, xmax = break_val, ymin = Inf, ymax = Inf)
        }
      } else {  # y axis
        annotation_position <- if (position == "left") {
          list(xmin = -Inf, xmax = -Inf, ymin = break_val, ymax = break_val)
        } else {  # right
          list(xmin = Inf, xmax = Inf, ymin = break_val, ymax = break_val)
        }
      }

      rlang::exec(
        ggplot2::annotation_custom,
        grob = tick_grob,
        !!!annotation_position
      )
    })

  stamp <- c(stamp, tick_annotations)

  return(stamp)
}
