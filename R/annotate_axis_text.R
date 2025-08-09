#' Annotate axis text segments
#'
#' @description Create annotated segments of the axis text.
#'
#' This function is designed to work with a theme that is globally set with [ggblanket::set_blanket] or [ggplot2::set_theme].
#'
#' It should be used with a `coord` of `ggplot2::coord_cartesian(clip = "off")`.
#'
#' It only works when panel dimensions are set are set in the theme.
#'
#' @param position The position of the axis text. One of "top", "bottom", "left", or "right".
#' @param ... Require named arguments (and support trailing commas).
#' @param x A vector of x-axis breaks for text positioning. Cannot be used together with y.
#' @param y A vector of y-axis breaks for text positioning. Cannot be used together with x.
#' @param label A vector of text label or a function that takes breaks and returns label. If NULL, uses the `scales::comma` on the breaks as label.
#' @param colour The colour of the text. Inherits from the current theme axis.text etc.
#' @param size The size of the text. Inherits from the current theme axis.text etc.
#' @param family The font family of the text. Inherits from the current theme axis.text etc.
#' @param length The total distance from the axis line to the text as a grid unit. Defaults to the sum of set theme tick length and relevant margin part.
#' @param margin The margin around the background rectangle. Can be a single unit value (applied to all sides) or a margin object with top, right, bottom, left components.
#' @param fill The fill colour of the background rectangle. If NULL, defaults to "transparent".
#' @param hjust,vjust Horizontal and vertical justification. Auto-calculated based on position if NULL.
#' @param angle Text rotation angle. Defaults to 0.
#' @param theme_element What to do with the equivalent theme elements. Either "keep" , "transparent", or "blank". Defaults "keep".
#'
#' @return A list of annotation layers and theme elements.
#' @export
#'
annotate_axis_text <- function(
    position,
    ...,
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
    theme_element = "keep"
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

  # Get breaks
  breaks <- if (!is.null(x)) x else y

  # Check for empty breaks
  if (length(breaks) == 0) {
    return(list())
  }

  # Process label
  if (is.null(label)) {
    labels <- scales::comma(breaks)
  } else if (is.function(label)) {
    labels <- label(breaks)
  } else {
    labels <- label
  }

  # Ensure labels match breaks length
  if (length(labels) != length(breaks)) {
    rlang::abort("Length of labels must match length of breaks")
  }

  # Build hierarchy for axis text from most specific to least specific
  text_specific <- paste0("axis.text.", axis, ".", position)
  text_axis <- paste0("axis.text.", axis)
  text_general <- "axis.text"

  # Use calc_element to properly resolve text properties with inheritance
  resolved_text_element <- NULL
  for (element_name in c(text_specific, text_axis, text_general)) {
    element <- ggplot2::calc_element(element_name, current_theme, skip_blank = TRUE)
    if (!is.null(element) && !inherits(element, "element_blank")) {
      resolved_text_element <- element
      break
    }
  }

  # If still no element found, create a minimal fallback
  if (is.null(resolved_text_element)) {
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

  # Calculate total length if not provided
  if (is.null(length)) {
    # First, get tick length from theme
    length_specific <- paste0("axis.ticks.length.", axis, ".", position)
    length_axis <- paste0("axis.ticks.length.", axis)
    length_general <- "axis.ticks.length"

    tick_length <- NULL
    for (element_name in c(length_specific, length_axis, length_general)) {
      element <- ggplot2::calc_element(element_name, current_theme, skip_blank = TRUE)
      if (!is.null(element)) {
        if (inherits(element, "rel")) {
          base_size <- current_theme$text$size %||% 11
          tick_length <- grid::unit(as.numeric(element) * base_size, "pt")
        } else if (inherits(element, "unit")) {
          tick_length <- element
        } else if (is.numeric(element)) {
          tick_length <- grid::unit(element, "pt")
        }
        if (!is.null(tick_length)) break
      }
    }

    # Fallback tick length
    if (is.null(tick_length)) {
      base_size <- current_theme$text$size %||% 11
      tick_length <- grid::unit(0.66 * base_size, "pt")
    }

    # Now get the text margin from the resolved text element
    text_margin <- resolved_text_element$margin

    # Extract the appropriate margin component
    margin_unit <- grid::unit(0, "pt")  # default

    if (!is.null(text_margin)) {
      # The margin is a unit vector with 4 components: [1]=top, [2]=right, [3]=bottom, [4]=left
      # Based on the correct specification:
      # axis.text.x.bottom uses margin[1] (top)
      # axis.text.x.top uses margin[3] (bottom)
      # axis.text.y.left uses margin[2] (right)
      # axis.text.y.right uses margin[4] (left)
      margin_index <- if (position == "bottom") {
        1  # top margin
      } else if (position == "top") {
        3  # bottom margin
      } else if (position == "left") {
        2  # right margin
      } else {  # right
        4  # left margin
      }

      # Extract the specific margin component
      if (inherits(text_margin, "unit") && length(text_margin) >= margin_index) {
        margin_unit <- text_margin[margin_index]
      } else if (inherits(text_margin, "margin")) {
        # margin objects can be accessed like unit vectors
        margin_unit <- text_margin[margin_index]
      }
    }

    # Total length = tick length + margin
    total_length <- tick_length + margin_unit
  } else {
    # Use provided length
    if (inherits(length, "unit")) {
      total_length <- length
    } else if (is.numeric(length)) {
      total_length <- grid::unit(length, "pt")
    } else {
      rlang::abort("length must be a grid unit or numeric value")
    }
  }

  # Process margin for background rectangle
  if (!is.null(margin)) {
    if (inherits(margin, "margin") || (inherits(margin, "unit") && length(margin) == 4)) {
      # Already a proper margin object or unit vector of length 4
      rect_margin <- margin
    } else if (inherits(margin, "unit") && length(margin) == 1) {
      # Single unit value - apply to all sides
      rect_margin <- rep(margin, 4)
    } else if (is.numeric(margin) && length(margin) == 1) {
      # Single numeric value - convert to unit and apply to all sides
      rect_margin <- rep(grid::unit(margin, "pt"), 4)
    } else if (is.numeric(margin) && length(margin) == 4) {
      # Four numeric values - convert to unit vector
      rect_margin <- grid::unit(margin, "pt")
    } else {
      rect_margin <- grid::unit(c(0, 0, 0, 0), "pt")
    }
  } else {
    rect_margin <- grid::unit(c(0, 0, 0, 0), "pt")
  }

  # Set hjust and vjust based on position
  # We assume good themes use standard justification values
  # Users can override with explicit hjust/vjust arguments if needed
  if (is.null(hjust)) {
    hjust <- if (position %in% c("top", "bottom")) {
      0.5  # Centered for top/bottom
    } else if (position == "left") {
      1    # Right-aligned for left axis
    } else {  # right
      0    # Left-aligned for right axis
    }
  }

  if (is.null(vjust)) {
    vjust <- if (position == "bottom") {
      1    # Top-aligned for bottom axis
    } else if (position == "top") {
      0    # Bottom-aligned for top axis
    } else {  # left or right
      0.5  # Centered for left/right
    }
  }

  # Set default fill
  text_fill <- fill %||% "transparent"

  stamp <- list()

  # Add theme modification if requested
  if (theme_element == "transparent") {
    theme_element_name <- paste0("axis.text.", axis, ".", position)
    theme_mod <- list()
    theme_mod[[theme_element_name]] <- ggplot2::element_text(colour = "transparent")
    stamp <- c(stamp, list(rlang::exec(ggplot2::theme, !!!theme_mod)))
  } else if (theme_element == "blank") {
    theme_element_name <- paste0("axis.text.", axis, ".", position)
    theme_mod <- list()
    theme_mod[[theme_element_name]] <- ggplot2::element_blank()
    stamp <- c(stamp, list(rlang::exec(ggplot2::theme, !!!theme_mod)))
  }

  # Create text annotations
  text_annotations <- breaks |>
    purrr::imap(\(break_val, i) {
      label_val <- labels[i]

      # Create text grob with background
      text_grob <- if (position == "bottom") {
        grid::grobTree(
          grid::rectGrob(
            x = grid::unit(0.5, "npc"),
            y = grid::unit(0, "npc") - total_length,
            width = grid::grobWidth(grid::textGrob(label_val, gp = grid::gpar(fontsize = text_size, fontfamily = text_family))) +
              rect_margin[2] + rect_margin[4],  # right + left margins
            height = grid::unit(text_size, "pt") +
              rect_margin[1] + rect_margin[3],  # top + bottom margins
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
              rect_margin[2] + rect_margin[4],  # right + left margins
            height = grid::unit(text_size, "pt") +
              rect_margin[1] + rect_margin[3],  # top + bottom margins
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
              rect_margin[2] + rect_margin[4],  # right + left margins
            height = grid::unit(text_size, "pt") +
              rect_margin[1] + rect_margin[3],  # top + bottom margins
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
              rect_margin[2] + rect_margin[4],  # right + left margins
            height = grid::unit(text_size, "pt") +
              rect_margin[1] + rect_margin[3],  # top + bottom margins
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
    })

  stamp <- c(stamp, text_annotations)

  return(stamp)
}
