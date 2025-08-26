#' Annotate axis text segments
#'
#' @description Create annotated segments of the axis text.
#'
#' This function is designed to work with a theme that is globally set with [ggblanket::set_blanket] or [ggplot2::set_theme].
#'
#' It should be used with a `coord` of `ggplot2::coord_cartesian(clip = "off")`.
#'
#' It only works when panel dimensions are set in the theme when using axis positioning.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param position The position of the axis text. One of "top", "bottom", "left", or "right". Ignored if both x and y are provided.
#' @param x A vector of x-axis breaks for text positioning. Use I() to specify normalized coordinates (0-1).
#' @param y A vector of y-axis breaks for text positioning. Use I() to specify normalized coordinates (0-1).
#' @param label A vector of text labels or a function that takes breaks and returns labels. If NULL, uses appropriate formatting.
#' @param colour The colour of the text. Inherits from the current theme axis.text etc.
#' @param size The size of the text. Inherits from the current theme axis.text etc.
#' @param family The font family of the text. Inherits from the current theme axis.text etc.
#' @param length The total distance from the axis line to the text as a grid unit. Defaults to the sum of set theme tick length and relevant margin part.
#' @param margin The margin around the background rectangle. Can be a single unit value (applied to all sides) or a margin object with top, right, bottom, left components.
#' @param fill The fill colour of the background rectangle. If NULL, defaults to "transparent".
#' @param hjust,vjust Horizontal and vertical justification. Auto-calculated based on position if NULL.
#' @param angle Text rotation angle. Defaults to 0.
#' @param theme What to do with the equivalent theme elements. Either "keep", "transparent", or "blank". Defaults to "keep".
#'
#' @return A list of annotation layers and theme elements.
#' @export
#'
annotate_axis_text <- function(
    ...,
    position = NULL,
    x = NULL,
    y = NULL,
    label = NULL,
    colour = NULL,
    size = NULL,
    family = NULL,
    length = NULL,
    margin = NULL,
    fill = NULL,
    hjust = NULL,
    vjust = NULL,
    angle = 0,
    theme = "keep"
) {
  # Check if both x and y are provided (arbitrary positioning mode)
  arbitrary_position <- !rlang::is_null(x) && !rlang::is_null(y)

  # Determine position from x/y if not specified
  if (!arbitrary_position) {
    if (rlang::is_null(position)) {
      if (!rlang::is_null(x)) {
        position <- "bottom"
      } else if (!rlang::is_null(y)) {
        position <- "left"
      } else {
        rlang::abort("Must specify either position, x, y, or both x and y")
      }
    }
    # Validate position for axis mode
    position <- rlang::arg_match(position, c("top", "bottom", "left", "right"))
  }

  # Check if values are wrapped in I() to determine coordinate type
  x_is_normalized <- !rlang::is_null(x) && inherits(x, "AsIs")
  y_is_normalized <- !rlang::is_null(y) && inherits(y, "AsIs")

  # Unwrap I() values
  if (x_is_normalized) {
    x <- unclass(x)
    if (any(x < 0 | x > 1)) {
      rlang::abort("Normalized x coordinates (specified with I()) must be between 0 and 1")
    }
  }
  if (y_is_normalized) {
    y <- unclass(y)
    if (any(y < 0 | y > 1)) {
      rlang::abort("Normalized y coordinates (specified with I()) must be between 0 and 1")
    }
  }

  if (arbitrary_position) {
    # Validate that x and y have same length
    if (length(x) != length(y)) {
      rlang::abort("x and y must have the same length when both are specified")
    }
    use_normalized <- x_is_normalized || y_is_normalized
    axis <- "x" # Default to x-axis styling when arbitrary positioning
    breaks <- list(x = x, y = y) # Store as list for easier access
  } else {
    # Original validation for axis mode
    if (position %in% c("top", "bottom")) {
      if (!rlang::is_null(y)) {
        rlang::abort("For top or bottom positions, only x can be specified, not y")
      }
      if (rlang::is_null(x)) {
        rlang::abort("For top or bottom positions, x must be specified")
      }
      use_normalized <- x_is_normalized
    } else {  # left or right
      if (!rlang::is_null(x)) {
        rlang::abort("For left or right positions, only y can be specified, not x")
      }
      if (rlang::is_null(y)) {
        rlang::abort("For left or right positions, y must be specified")
      }
      use_normalized <- y_is_normalized
    }
    axis <- if (position %in% c("top", "bottom")) "x" else "y"
    breaks <- if (!rlang::is_null(x)) x else y
  }

  theme <- rlang::arg_match(theme, c("keep", "transparent", "blank"))

  # Get current theme
  current_theme <- ggplot2::theme_get()

  # Skip panel dimension checks for arbitrary positioning
  if (!arbitrary_position) {
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
          "panel.heights must be set in theme for x-axis text annotation"
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
          "panel.widths must be set in theme for y-axis text annotation"
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
  }

  # Check for empty breaks
  n_breaks <- if (arbitrary_position) length(breaks$x) else length(breaks)
  if (n_breaks == 0) {
    return(list())
  }

  # Process labels
  if (rlang::is_null(label)) {
    if (use_normalized) {
      if (arbitrary_position) {
        labels <- paste0("(", breaks$x, ", ", breaks$y, ")")
      } else {
        labels <- as.character(breaks)
      }
    } else {
      if (arbitrary_position) {
        # Format each coordinate appropriately
        x_labels <- if (inherits(breaks$x, "Date")) {
          format(breaks$x, "%d-%m-%Y")
        } else if (inherits(breaks$x, "POSIXct") || inherits(breaks$x, "POSIXlt")) {
          format(breaks$x, "%d-%m-%Y %H:%M:%S")
        } else if (inherits(breaks$x, "hms") || inherits(breaks$x, "difftime")) {
          as.character(breaks$x)
        } else if (is.numeric(breaks$x)) {
          scales::comma(breaks$x)
        } else {
          as.character(breaks$x)
        }

        y_labels <- if (inherits(breaks$y, "Date")) {
          format(breaks$y, "%d-%m-%Y")
        } else if (inherits(breaks$y, "POSIXct") || inherits(breaks$y, "POSIXlt")) {
          format(breaks$y, "%d-%m-%Y %H:%M:%S")
        } else if (inherits(breaks$y, "hms") || inherits(breaks$y, "difftime")) {
          as.character(breaks$y)
        } else if (is.numeric(breaks$y)) {
          scales::comma(breaks$y)
        } else {
          as.character(breaks$y)
        }

        labels <- paste0("(", x_labels, ", ", y_labels, ")")
      } else {
        # Check data type and format appropriately
        if (inherits(breaks, "Date")) {
          labels <- format(breaks, "%d-%m-%Y")
        } else if (inherits(breaks, "POSIXct") || inherits(breaks, "POSIXlt")) {
          labels <- format(breaks, "%d-%m-%Y %H:%M:%S")
        } else if (inherits(breaks, "hms") || inherits(breaks, "difftime")) {
          labels <- as.character(breaks)
        } else if (is.numeric(breaks)) {
          labels <- scales::comma(breaks)
        } else {
          labels <- as.character(breaks)
        }
      }
    }
  } else if (is.function(label)) {
    labels <- label(breaks)
  } else {
    labels <- label
  }

  # Ensure labels match breaks length
  if (length(labels) != n_breaks) {
    rlang::abort("Length of labels must match length of breaks")
  }

  # Build hierarchy for axis text from most specific to least specific
  if (arbitrary_position) {
    # For arbitrary positioning, just use general axis.text
    text_hierarchy <- c("axis.text.x", "axis.text")
  } else {
    text_specific <- paste0("axis.text.", axis, ".", position)
    text_axis <- paste0("axis.text.", axis)
    text_general <- "axis.text"
    text_hierarchy <- c(text_specific, text_axis, text_general)
  }

  # Use calc_element to properly resolve text properties with inheritance
  resolved_text_element <- NULL
  for (element_name in text_hierarchy) {
    element <- ggplot2::calc_element(element_name, current_theme, skip_blank = TRUE)
    if (!rlang::is_null(element) && !inherits(element, "element_blank")) {
      resolved_text_element <- element
      break
    }
  }

  # If still no element found, create a minimal fallback
  if (rlang::is_null(resolved_text_element)) {
    resolved_text_element <- ggplot2::element_text(
      colour = "black",
      size = 11,
      family = ""
    )
  }

  # Extract theme properties with proper resolution
  text_colour <- colour %||% resolved_text_element$colour %||% "black"
  text_size <- size %||% resolved_text_element$size %||% 11
  text_family <- family %||% resolved_text_element$family %||% ""

  # For arbitrary positioning, skip length calculation
  if (!arbitrary_position) {
    # Calculate total length if not provided
    if (rlang::is_null(length)) {
      # First, get tick length from theme
      length_specific <- paste0("axis.ticks.length.", axis, ".", position)
      length_axis <- paste0("axis.ticks.length.", axis)
      length_general <- "axis.ticks.length"

      tick_length <- NULL
      for (element_name in c(length_specific, length_axis, length_general)) {
        element <- ggplot2::calc_element(element_name, current_theme, skip_blank = TRUE)
        if (!rlang::is_null(element)) {
          if (inherits(element, "rel")) {
            spacing <- current_theme$spacing %||% grid::unit(5.5, "pt")
            if (inherits(spacing, "unit")) {
              spacing_pts <- as.numeric(grid::convertUnit(spacing, "pt"))
            } else {
              spacing_pts <- 5.5
            }
            tick_length <- grid::unit(as.numeric(element) * spacing_pts, "pt")
          } else if (inherits(element, "unit")) {
            tick_length <- element
          } else if (is.numeric(element)) {
            tick_length <- grid::unit(element, "pt")
          }
          if (!rlang::is_null(tick_length)) break
        }
      }

      # Fallback tick length
      if (rlang::is_null(tick_length)) {
        spacing <- current_theme$spacing %||% grid::unit(5.5, "pt")
        if (inherits(spacing, "unit")) {
          spacing_pts <- as.numeric(grid::convertUnit(spacing, "pt"))
        } else {
          spacing_pts <- 5.5
        }
        tick_length <- grid::unit(0.5 * spacing_pts, "pt")
      }

      # Now get the text margin from the resolved text element
      text_margin <- resolved_text_element$margin
      margin_unit <- grid::unit(0, "pt")

      if (!rlang::is_null(text_margin)) {
        margin_index <- if (position == "bottom") {
          1
        } else if (position == "top") {
          3
        } else if (position == "left") {
          2
        } else {
          4
        }

        if (inherits(text_margin, "unit") && length(text_margin) >= margin_index) {
          margin_unit <- text_margin[margin_index]
        } else if (inherits(text_margin, "margin")) {
          margin_unit <- text_margin[margin_index]
        }
      }

      total_length <- tick_length + margin_unit
    } else {
      if (inherits(length, "unit")) {
        total_length <- length
      } else if (is.numeric(length)) {
        total_length <- grid::unit(length, "pt")
      } else {
        rlang::abort("length must be a grid unit or numeric value")
      }
    }
  }

  # Process margin for background rectangle
  if (!rlang::is_null(margin)) {
    if (inherits(margin, "margin") || (inherits(margin, "unit") && length(margin) == 4)) {
      rect_margin <- margin
    } else if (inherits(margin, "unit") && length(margin) == 1) {
      rect_margin <- rep(margin, 4)
    } else if (is.numeric(margin) && length(margin) == 1) {
      rect_margin <- rep(grid::unit(margin, "pt"), 4)
    } else if (is.numeric(margin) && length(margin) == 4) {
      rect_margin <- grid::unit(margin, "pt")
    } else {
      rect_margin <- grid::unit(c(0, 0, 0, 0), "pt")
    }
  } else {
    rect_margin <- grid::unit(c(0, 0, 0, 0), "pt")
  }

  # Set hjust and vjust based on position or use defaults for arbitrary
  if (arbitrary_position) {
    if (rlang::is_null(hjust)) hjust <- 0.5
    if (rlang::is_null(vjust)) vjust <- 0.5
  } else {
    if (rlang::is_null(hjust)) {
      hjust <- if (position %in% c("top", "bottom")) {
        0.5
      } else if (position == "left") {
        1
      } else {
        0
      }
    }

    if (rlang::is_null(vjust)) {
      vjust <- if (position == "bottom") {
        1
      } else if (position == "top") {
        0
      } else {
        0.5
      }
    }
  }

  # Set default fill
  text_fill <- fill %||% "transparent"

  stamp <- list()

  # Add theme modification if requested (only for axis positioning)
  if (!arbitrary_position && theme != "keep") {
    theme_name <- paste0("axis.text.", axis, ".", position)
    theme_mod <- list()
    if (theme == "transparent") {
      theme_mod[[theme_name]] <- ggplot2::element_text(colour = "transparent")
    } else if (theme == "blank") {
      theme_mod[[theme_name]] <- ggplot2::element_blank()
    }
    stamp <- c(stamp, list(rlang::exec(ggplot2::theme, !!!theme_mod)))
  }

  # Create annotations
  if (arbitrary_position) {
    # For arbitrary positioning, create text at specified x,y coordinates
    text_annotations <- seq_len(n_breaks) |>
      purrr::map(\(i) {
        label_val <- labels[i]

        if (use_normalized) {
          # Create normalized grob with optional background
          if (text_fill != "transparent") {
            text_grob <- grid::grobTree(
              grid::rectGrob(
                x = grid::unit(breaks$x[i], "npc"),
                y = grid::unit(breaks$y[i], "npc"),
                width = grid::grobWidth(grid::textGrob(label_val, gp = grid::gpar(fontsize = text_size, fontfamily = text_family))) +
                  rect_margin[2] + rect_margin[4],
                height = grid::unit(text_size, "pt") +
                  rect_margin[1] + rect_margin[3],
                just = c(hjust, vjust),
                gp = grid::gpar(fill = text_fill, col = NA)
              ),
              grid::textGrob(
                label_val,
                x = grid::unit(breaks$x[i], "npc"),
                y = grid::unit(breaks$y[i], "npc"),
                just = c(hjust, vjust),
                rot = angle,
                gp = grid::gpar(
                  col = text_colour,
                  fontsize = text_size,
                  fontfamily = text_family
                )
              )
            )
          } else {
            text_grob <- grid::textGrob(
              label_val,
              x = grid::unit(breaks$x[i], "npc"),
              y = grid::unit(breaks$y[i], "npc"),
              just = c(hjust, vjust),
              rot = angle,
              gp = grid::gpar(
                col = text_colour,
                fontsize = text_size,
                fontfamily = text_family
              )
            )
          }

          ggplot2::annotation_custom(
            grob = text_grob,
            xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
          )
        } else {
          # Use annotate for data coordinates
          ggplot2::annotate(
            "text",
            x = breaks$x[i],
            y = breaks$y[i],
            label = label_val,
            colour = text_colour,
            size = text_size / .pt,
            family = text_family,
            hjust = hjust,
            vjust = vjust,
            angle = angle
          )
        }
      })

    stamp <- c(stamp, text_annotations)
  } else {
    # Original axis-based annotation code
    text_annotations <- breaks |>
      purrr::imap(\(break_val, i) {
        label_val <- labels[i]

        # [Rest of the original axis positioning code remains unchanged...]
        # For normalized coordinates, use them directly as npc units
        if (use_normalized) {
          text_grob <- if (position == "bottom") {
            grid::grobTree(
              grid::rectGrob(
                x = grid::unit(break_val, "npc"),
                y = grid::unit(0, "npc") - total_length,
                width = grid::grobWidth(grid::textGrob(label_val, gp = grid::gpar(fontsize = text_size, fontfamily = text_family))) +
                  rect_margin[2] + rect_margin[4],
                height = grid::unit(text_size, "pt") +
                  rect_margin[1] + rect_margin[3],
                just = c(hjust, vjust),
                gp = grid::gpar(fill = text_fill, col = NA)
              ),
              grid::textGrob(
                label_val,
                x = grid::unit(break_val, "npc"),
                y = grid::unit(0, "npc") - total_length,
                just = c(hjust, vjust),
                rot = angle,
                gp = grid::gpar(
                  col = text_colour,
                  fontsize = text_size,
                  fontfamily = text_family
                )
              )
            )
          } else if (position == "top") {
            grid::grobTree(
              grid::rectGrob(
                x = grid::unit(break_val, "npc"),
                y = grid::unit(1, "npc") + total_length,
                width = grid::grobWidth(grid::textGrob(label_val, gp = grid::gpar(fontsize = text_size, fontfamily = text_family))) +
                  rect_margin[2] + rect_margin[4],
                height = grid::unit(text_size, "pt") +
                  rect_margin[1] + rect_margin[3],
                just = c(hjust, vjust),
                gp = grid::gpar(fill = text_fill, col = NA)
              ),
              grid::textGrob(
                label_val,
                x = grid::unit(break_val, "npc"),
                y = grid::unit(1, "npc") + total_length,
                just = c(hjust, vjust),
                rot = angle,
                gp = grid::gpar(
                  col = text_colour,
                  fontsize = text_size,
                  fontfamily = text_family
                )
              )
            )
          } else if (position == "left") {
            grid::grobTree(
              grid::rectGrob(
                x = grid::unit(0, "npc") - total_length,
                y = grid::unit(break_val, "npc"),
                width = grid::grobWidth(grid::textGrob(label_val, gp = grid::gpar(fontsize = text_size, fontfamily = text_family))) +
                  rect_margin[2] + rect_margin[4],
                height = grid::unit(text_size, "pt") +
                  rect_margin[1] + rect_margin[3],
                just = c(hjust, vjust),
                gp = grid::gpar(fill = text_fill, col = NA)
              ),
              grid::textGrob(
                label_val,
                x = grid::unit(0, "npc") - total_length,
                y = grid::unit(break_val, "npc"),
                just = c(hjust, vjust),
                rot = angle,
                gp = grid::gpar(
                  col = text_colour,
                  fontsize = text_size,
                  fontfamily = text_family
                )
              )
            )
          } else {  # right
            grid::grobTree(
              grid::rectGrob(
                x = grid::unit(1, "npc") + total_length,
                y = grid::unit(break_val, "npc"),
                width = grid::grobWidth(grid::textGrob(label_val, gp = grid::gpar(fontsize = text_size, fontfamily = text_family))) +
                  rect_margin[2] + rect_margin[4],
                height = grid::unit(text_size, "pt") +
                  rect_margin[1] + rect_margin[3],
                just = c(hjust, vjust),
                gp = grid::gpar(fill = text_fill, col = NA)
              ),
              grid::textGrob(
                label_val,
                x = grid::unit(1, "npc") + total_length,
                y = grid::unit(break_val, "npc"),
                just = c(hjust, vjust),
                rot = angle,
                gp = grid::gpar(
                  col = text_colour,
                  fontsize = text_size,
                  fontfamily = text_family
                )
              )
            )
          }

          rlang::exec(
            ggplot2::annotation_custom,
            grob = text_grob,
            xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
          )
        } else {
          # Original behavior for data coordinates
          text_grob <- if (position == "bottom") {
            grid::grobTree(
              grid::rectGrob(
                x = grid::unit(0.5, "npc"),
                y = grid::unit(0, "npc") - total_length,
                width = grid::grobWidth(grid::textGrob(label_val, gp = grid::gpar(fontsize = text_size, fontfamily = text_family))) +
                  rect_margin[2] + rect_margin[4],
                height = grid::unit(text_size, "pt") +
                  rect_margin[1] + rect_margin[3],
                just = c(hjust, vjust),
                gp = grid::gpar(fill = text_fill, col = NA)
              ),
              grid::textGrob(
                label_val,
                x = grid::unit(0.5, "npc"),
                y = grid::unit(0, "npc") - total_length,
                just = c(hjust, vjust),
                rot = angle,
                gp = grid::gpar(
                  col = text_colour,
                  fontsize = text_size,
                  fontfamily = text_family
                )
              )
            )
          } else if (position == "top") {
            grid::grobTree(
              grid::rectGrob(
                x = grid::unit(0.5, "npc"),
                y = grid::unit(1, "npc") + total_length,
                width = grid::grobWidth(grid::textGrob(label_val, gp = grid::gpar(fontsize = text_size, fontfamily = text_family))) +
                  rect_margin[2] + rect_margin[4],
                height = grid::unit(text_size, "pt") +
                  rect_margin[1] + rect_margin[3],
                just = c(hjust, vjust),
                gp = grid::gpar(fill = text_fill, col = NA)
              ),
              grid::textGrob(
                label_val,
                x = grid::unit(0.5, "npc"),
                y = grid::unit(1, "npc") + total_length,
                just = c(hjust, vjust),
                rot = angle,
                gp = grid::gpar(
                  col = text_colour,
                  fontsize = text_size,
                  fontfamily = text_family
                )
              )
            )
          } else if (position == "left") {
            grid::grobTree(
              grid::rectGrob(
                x = grid::unit(0, "npc") - total_length,
                y = grid::unit(0.5, "npc"),
                width = grid::grobWidth(grid::textGrob(label_val, gp = grid::gpar(fontsize = text_size, fontfamily = text_family))) +
                  rect_margin[2] + rect_margin[4],
                height = grid::unit(text_size, "pt") +
                  rect_margin[1] + rect_margin[3],
                just = c(hjust, vjust),
                gp = grid::gpar(fill = text_fill, col = NA)
              ),
              grid::textGrob(
                label_val,
                x = grid::unit(0, "npc") - total_length,
                y = grid::unit(0.5, "npc"),
                just = c(hjust, vjust),
                rot = angle,
                gp = grid::gpar(
                  col = text_colour,
                  fontsize = text_size,
                  fontfamily = text_family
                )
              )
            )
          } else {  # right
            grid::grobTree(
              grid::rectGrob(
                x = grid::unit(1, "npc") + total_length,
                y = grid::unit(0.5, "npc"),
                width = grid::grobWidth(grid::textGrob(label_val, gp = grid::gpar(fontsize = text_size, fontfamily = text_family))) +
                  rect_margin[2] + rect_margin[4],
                height = grid::unit(text_size, "pt") +
                  rect_margin[1] + rect_margin[3],
                just = c(hjust, vjust),
                gp = grid::gpar(fill = text_fill, col = NA)
              ),
              grid::textGrob(
                label_val,
                x = grid::unit(1, "npc") + total_length,
                y = grid::unit(0.5, "npc"),
                just = c(hjust, vjust),
                rot = angle,
                gp = grid::gpar(
                  col = text_colour,
                  fontsize = text_size,
                  fontfamily = text_family
                )
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
            grob = text_grob,
            !!!annotation_position
          )
        }
      })

    stamp <- c(stamp, text_annotations)
  }

  return(stamp)
}
