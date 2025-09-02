#' Annotate axis ticks segments
#'
#' @description Create annotated segments of the axis ticks.
#'
#' This function is designed to work with a theme that is globally set with [ggblanket::set_blanket] or [ggplot2::set_theme].
#'
#' It should be used with a `coord` of `ggplot2::coord_cartesian(clip = "off")`.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param position The position of the axis ticks. One of `"top"`, `"bottom"`, `"left"`, or `"right"`.
#' @param x A vector of x-axis breaks for ticks positioning. Use `I()` to specify normalized coordinates (0-1).
#' @param y A vector of y-axis breaks for ticks positioning. Use `I()` to specify normalized coordinates (0-1).
#' @param minor `TRUE` or `FALSE` whether to relate to minor ticks. Defaults `FALSE`.
#' @param colour The colour of the ticks. Inherits from the current theme `axis.ticks` etc.
#' @param linewidth The linewidth of the ticks. Inherits from the current theme `axis.ticks` etc.
#' @param length The total distance from the axis line to the ticks as a grid unit. Defaults to the sum of set theme tick length and relevant margin part.
#' @param theme What to do with the equivalent theme elements. Either `"keep"`, `"transparent"`, or `"blank"`. Defaults `"keep"`.
#'
#' @return A list of annotation annotates and theme elements.
#' @export
annotate_axis_ticks <- function(
    ...,
    position = NULL,
    x = NULL,
    y = NULL,
    minor = FALSE,
    colour = NULL,
    linewidth = NULL,
    length = NULL,
    theme = "keep"
) {
  # Determine position from x/y if not specified
  if (rlang::is_null(position)) {
    if (!rlang::is_null(x) && !rlang::is_null(y)) {
      rlang::abort("Cannot specify both x and y. Use either x for top/bottom positions or y for left/right positions.")
    }
    if (!rlang::is_null(x)) {
      position <- "bottom"
    } else if (!rlang::is_null(y)) {
      position <- "left"
    } else {
      rlang::abort("Must specify either position, x, or y")
    }
  }

  # Validate position
  position <- rlang::arg_match(position, c("top", "bottom", "left", "right"))

  # Check if values are wrapped in I() to determine coordinate type
  x_is_normalized <- !rlang::is_null(x) && inherits(x, "AsIs")
  y_is_normalized <- !rlang::is_null(y) && inherits(y, "AsIs")

  # Unwrap I() values
  if (x_is_normalized) {
    x <- unclass(x)
    # Validate normalized coordinates are between 0 and 1
    if (any(x < 0 | x > 1)) {
      rlang::abort("Normalized x coordinates (specified with I()) must be between 0 and 1")
    }
  }
  if (y_is_normalized) {
    y <- unclass(y)
    # Validate normalized coordinates are between 0 and 1
    if (any(y < 0 | y > 1)) {
      rlang::abort("Normalized y coordinates (specified with I()) must be between 0 and 1")
    }
  }

  # Validate x/y based on position
  if (position %in% c("top", "bottom")) {
    # For top/bottom positions, only x should be provided
    if (!rlang::is_null(y)) {
      rlang::abort("For top or bottom positions, only x can be specified, not y")
    }
    if (rlang::is_null(x)) {
      rlang::abort("For top or bottom positions, x must be specified")
    }
    use_normalized <- x_is_normalized
  } else {  # left or right
    # For left/right positions, only y should be provided
    if (!rlang::is_null(x)) {
      rlang::abort("For left or right positions, only y can be specified, not x")
    }
    if (rlang::is_null(y)) {
      rlang::abort("For left or right positions, y must be specified")
    }
    use_normalized <- y_is_normalized
  }

  theme <- rlang::arg_match(theme, c("keep", "transparent", "blank"))

  # Determine axis from position
  axis <- if (position %in% c("top", "bottom")) "x" else "y"

  # Get breaks - keep original for positioning
  breaks <- if (!rlang::is_null(x)) x else y

  # Check for empty breaks
  if (length(breaks) == 0) {
    return(list())
  }

  # Get current theme
  current_theme <- ggplot2::theme_get()

  # Build hierarchy for axis ticks based on whether minor or major
  if (minor) {
    # For minor ticks, check minor first then fall back to major
    tick_minor_specific <- paste0("axis.minor.ticks.", axis, ".", position)
    tick_specific <- paste0("axis.ticks.", axis, ".", position)
    tick_axis <- paste0("axis.ticks.", axis)
    tick_general <- "axis.ticks"

    tick_hierarchy <- c(tick_minor_specific, tick_specific, tick_axis, tick_general)
  } else {
    # For major ticks, use standard hierarchy
    tick_specific <- paste0("axis.ticks.", axis, ".", position)
    tick_axis <- paste0("axis.ticks.", axis)
    tick_general <- "axis.ticks"

    tick_hierarchy <- c(tick_specific, tick_axis, tick_general)
  }

  # Use calc_element to properly resolve tick properties with inheritance
  resolved_tick_element <- NULL
  for (element_name in tick_hierarchy) {
    element <- ggplot2::calc_element(element_name, current_theme, skip_blank = TRUE)
    if (!rlang::is_null(element) && !inherits(element, "element_blank")) {
      resolved_tick_element <- element
      break
    }
  }

  # If still no element found, create a minimal fallback (matching theme_grey)
  if (rlang::is_null(resolved_tick_element)) {
    resolved_tick_element <- list(colour = "black", linewidth = 0.5)
  }

  # Build hierarchy for axis ticks length based on whether minor or major
  if (minor) {
    # For minor ticks length
    length_minor_specific <- paste0("axis.minor.ticks.length.", axis, ".", position)
    length_minor_axis <- paste0("axis.minor.ticks.length.", axis)
    length_minor_general <- "axis.minor.ticks.length"
    length_specific <- paste0("axis.ticks.length.", axis, ".", position)
    length_axis <- paste0("axis.ticks.length.", axis)
    length_general <- "axis.ticks.length"

    length_hierarchy <- c(length_minor_specific, length_minor_axis, length_minor_general,
                          length_specific, length_axis, length_general)
  } else {
    # For major ticks length
    length_specific <- paste0("axis.ticks.length.", axis, ".", position)
    length_axis <- paste0("axis.ticks.length.", axis)
    length_general <- "axis.ticks.length"

    length_hierarchy <- c(length_specific, length_axis, length_general)
  }

  # Use calc_element to properly resolve length with inheritance and rel() handling
  resolved_length_element <- NULL
  for (element_name in length_hierarchy) {
    element <- ggplot2::calc_element(element_name, current_theme, skip_blank = TRUE)
    if (!rlang::is_null(element) && !inherits(element, "element_blank")) {
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

  # Handle length with proper unit and rel() support
  if (rlang::is_null(length)) {
    # For minor ticks, we need to check if the raw theme element is rel()
    # because calc_element will have already resolved it
    if (minor) {
      # Check the raw theme elements for rel() before calc_element processes them
      raw_minor_length <- NULL
      for (element_name in length_hierarchy) {
        # Only check minor-specific elements for raw rel()
        if (grepl("minor", element_name)) {
          raw_element <- current_theme[[element_name]]
          if (!rlang::is_null(raw_element) && inherits(raw_element, "rel")) {
            # Found a rel() in minor tick length - need to apply it to major tick length
            raw_minor_length <- raw_element
            break
          }
        }
      }

      if (!rlang::is_null(raw_minor_length)) {
        # Minor tick has rel() - calculate relative to axis.ticks.length
        major_length <- ggplot2::calc_element("axis.ticks.length", current_theme, skip_blank = TRUE)

        if (rlang::is_null(major_length)) {
          # No axis.ticks.length found, use default from spacing
          spacing <- current_theme$spacing %||% grid::unit(5.5, "pt")
          if (inherits(spacing, "unit")) {
            major_tick_length_pts <- as.numeric(grid::convertUnit(spacing, "pt"))
          } else {
            major_tick_length_pts <- 5.5
          }
        } else if (inherits(major_length, "unit")) {
          major_tick_length_pts <- as.numeric(grid::convertUnit(major_length, "pt"))
        } else if (is.numeric(major_length)) {
          major_tick_length_pts <- major_length
        } else {
          # Fallback
          major_tick_length_pts <- 5.5
        }

        # Apply the rel() to major tick length
        tick_length <- grid::unit(as.numeric(raw_minor_length) * major_tick_length_pts, "pt")
      } else {
        # No rel() found, use the resolved element
        tick_length <- resolved_length_element

        if (rlang::is_null(tick_length)) {
          # Fallback for minor
          text_size <- current_theme$text$size %||% 11
          tick_length <- grid::unit(0.375 * text_size, "pt")
        } else if (!inherits(tick_length, "unit")) {
          if (is.numeric(tick_length)) {
            tick_length <- grid::unit(tick_length, "pt")
          } else {
            text_size <- current_theme$text$size %||% 11
            tick_length <- grid::unit(0.375 * text_size, "pt")
          }
        }
      }
    } else {
      # Major tick length handling
      tick_length <- resolved_length_element

      if (rlang::is_null(tick_length)) {
        # Fallback for major
        text_size <- current_theme$text$size %||% 11
        tick_length <- grid::unit(0.5 * text_size, "pt")
      } else if (inherits(tick_length, "rel")) {
        # Major tick rel() is relative to spacing
        spacing <- current_theme$spacing %||% grid::unit(5.5, "pt")
        if (inherits(spacing, "unit")) {
          spacing_pts <- as.numeric(grid::convertUnit(spacing, "pt"))
        } else {
          spacing_pts <- 5.5
        }
        tick_length <- grid::unit(as.numeric(tick_length) * spacing_pts, "pt")
      } else if (!inherits(tick_length, "unit")) {
        if (is.numeric(tick_length)) {
          tick_length <- grid::unit(tick_length, "pt")
        } else {
          text_size <- current_theme$text$size %||% 11
          tick_length <- grid::unit(0.5 * text_size, "pt")
        }
      }
    }
  } else {
    # Handle user-provided length
    if (inherits(length, "unit")) {
      tick_length <- length
    } else if (is.numeric(length)) {
      tick_length <- grid::unit(length, "pt")
    } else {
      # Fallback
      text_size <- current_theme$text$size %||% 11
      if (minor) {
        tick_length <- grid::unit(0.375 * text_size, "pt")
      } else {
        tick_length <- grid::unit(0.5 * text_size, "pt")
      }
    }
  }

  stamp <- list()

  # Add theme modification if requested
  if (theme != "keep") {
    # Determine which theme element to modify based on minor flag
    if (minor) {
      theme_name <- paste0("axis.minor.ticks.", axis, ".", position)
    } else {
      theme_name <- paste0("axis.ticks.", axis, ".", position)
    }

    theme_mod <- list()
    if (theme == "transparent") {
      theme_mod[[theme_name]] <- ggplot2::element_line(colour = "transparent")
    } else if (theme == "blank") {
      theme_mod[[theme_name]] <- ggplot2::element_blank()
    }
    stamp <- c(stamp, list(rlang::exec(ggplot2::theme, !!!theme_mod)))
  }

  # Create tick annotations
  tick_annotations <- breaks |>
    purrr::imap(\(break_val, i) {
      # For normalized coordinates, use break_val directly as npc unit
      if (use_normalized) {
        tick_grob <- if (position == "bottom") {
          grid::segmentsGrob(
            x0 = grid::unit(break_val, "npc"),
            x1 = grid::unit(break_val, "npc"),
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
            x0 = grid::unit(break_val, "npc"),
            x1 = grid::unit(break_val, "npc"),
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
            y0 = grid::unit(break_val, "npc"),
            y1 = grid::unit(break_val, "npc"),
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
            y0 = grid::unit(break_val, "npc"),
            y1 = grid::unit(break_val, "npc"),
            gp = grid::gpar(
              col = tick_colour,
              lwd = tick_linewidth * 72 / 25.4,
              lineend = "butt"
            )
          )
        }

        # For normalized coordinates, span the full plot area
        rlang::exec(
          ggplot2::annotation_custom,
          grob = tick_grob,
          xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
        )
      } else {
        # Original behavior for data coordinates
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
      }
    })

  stamp <- c(stamp, tick_annotations)

  return(stamp)
}
