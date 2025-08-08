#' Annotated axis text labels
#'
#' @description Add text labels positioned relative to axis tick marks using absolute measurements.
#' This function only works when panel dimensions are set via panel.widths and panel.heights.
#' It requires a `coord` of `ggplot2::coord_cartesian(clip = "off")`.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param position The position of the axis text. One of "top", "bottom", "left", or "right".
#' @param x A vector of x-axis breaks for text positioning. Cannot be used together with y.
#' @param y A vector of y-axis breaks for text positioning. Cannot be used together with x.
#' @param labels A vector of text labels or a function that takes breaks and returns labels. If NULL, uses the breaks as labels.
#' @param colour The colour of the text. Inherits from the current theme axis.text etc.
#' @param size The size of the text. Inherits from the current theme axis.text etc.
#' @param family The font family of the text. Inherits from the current theme axis.text etc.
#' @param length The total distance from the axis line to the text as a grid unit. Defaults to the sum of tick length plus margin.
#' @param margin The margin around the background rectangle. Can be a single unit value (applied to all sides) or a margin object with top, right, bottom, left components.
#' @param fill The fill colour of the background rectangle. If NULL, defaults to "transparent".
#' @param hjust,vjust Horizontal and vertical justification. Auto-calculated based on position if NULL.
#' @param angle Text rotation angle. Defaults to 0.
#' @param theme_element What to do with the equivalent theme element. Either "transparent", "keep" or "blank". Defaults "transparent".
#'
#' @return A list of annotation layers and theme elements.
#' @export
#'
annotate_axis_text <- function(
    ...,
    position,
    x = NULL,
    y = NULL,
    labels = NULL,
    colour = NULL,
    size = NULL,
    family = NULL,
    length = NULL,
    margin = NULL,
    fill = NULL,
    hjust = NULL,
    vjust = NULL,
    angle = 0,
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

  # Determine axis from position and validate consistency with x/y
  axis <- if (position %in% c("top", "bottom")) "x" else "y"
  breaks <- if (!is.null(x)) x else y

  # Validate that axis matches the provided breaks
  if (axis == "x" && is.null(x)) {
    rlang::abort("For top/bottom positions, x breaks must be specified")
  }
  if (axis == "y" && is.null(y)) {
    rlang::abort("For left/right positions, y breaks must be specified")
  }

  # Process labels - support both vectors and functions
  if (rlang::is_null(labels)) {
    # Default: use breaks as labels
    labels <- as.character(breaks)
  } else if (is.function(labels)) {
    # Apply function to breaks to generate labels
    labels <- labels(breaks)
    labels <- as.character(labels)
  } else {
    # Convert vector labels to character
    labels <- as.character(labels)
  }

  # Ensure breaks and labels have same length
  if (length(breaks) != length(labels)) {
    rlang::abort("breaks and labels must have the same length")
  }

  # Get current theme and check if panel dimensions are explicitly set
  current_theme <- ggplot2::theme_get()
  panel_widths <- current_theme$panel.widths
  panel_heights <- current_theme$panel.heights

  if (rlang::is_null(panel_widths) && rlang::is_null(panel_heights)) {
    rlang::abort(
      "This function only works when panel dimensions are explicitly set via theme(panel.widths = ..., panel.heights = ...)"
    )
  }

  # Validate uniform panel dimensions for the specific axis
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
    resolved_text_element <- list(colour = "black", size = 11, family = "")
  }

  # Extract theme margin for the specific position using calc_element
  if (rlang::is_null(margin)) {
    # Build hierarchy for margin resolution
    margin_specific <- paste0("axis.text.", axis, ".", position)
    margin_axis <- paste0("axis.text.", axis)
    margin_general <- "axis.text"

    # Use calc_element to properly resolve margin with inheritance
    resolved_margin <- NULL
    for (element_name in c(margin_specific, margin_axis, margin_general)) {
      element <- ggplot2::calc_element(element_name, current_theme, skip_blank = TRUE)
      if (!is.null(element) && !inherits(element, "element_blank") && !is.null(element$margin)) {
        resolved_margin <- element$margin
        break
      }
    }

    # Extract the relevant margin side based on position
    if (!is.null(resolved_margin)) {
      if (position == "bottom") {
        margin <- resolved_margin[1]  # top margin (distance from axis line)
      } else if (position == "top") {
        margin <- resolved_margin[3]  # bottom margin (distance from axis line)
      } else if (position == "left") {
        margin <- resolved_margin[4]  # right margin (distance from axis line)
      } else { # right
        margin <- resolved_margin[2]  # left margin (distance from axis line)
      }
    } else {
      # No margin found in current theme, use theme_grey() fallbacks
      if (position == "bottom") {
        fallback_element <- ggplot2::calc_element("axis.text.x.bottom", ggplot2::theme_grey())
        margin <- fallback_element$margin[1]  # 2.2points
      } else if (position == "top") {
        fallback_element <- ggplot2::calc_element("axis.text.x.top", ggplot2::theme_grey())
        margin <- fallback_element$margin[3]  # 2.2points
      } else if (position == "left") {
        fallback_element <- ggplot2::calc_element("axis.text.y.left", ggplot2::theme_grey())
        margin <- fallback_element$margin[4]  # 0points
      } else { # right
        fallback_element <- ggplot2::calc_element("axis.text.y.right", ggplot2::theme_grey())
        margin <- fallback_element$margin[2]  # 0points
      }
    }

    # Final fallback if somehow still no margin
    if (is.null(margin) || !inherits(margin, "unit")) {
      margin <- grid::unit(2, "pt")
    }
  }

  # Handle margin - can be single value or margin object
  if (length(margin) == 4) {
    # margin object with 4 values (top, right, bottom, left)
    margin_top <- margin[1]
    margin_right <- margin[2]
    margin_bottom <- margin[3]
    margin_left <- margin[4]
  } else {
    # Single value applied to all sides
    margin_top <- margin_right <- margin_bottom <- margin_left <- margin
  }

  # Set default fill colour
  if (rlang::is_null(fill)) {
    fill <- "transparent"
  }

  # Extract text properties with proper resolution
  text_colour <- if (rlang::is_null(colour)) {
    resolved_text_element$colour %||% "black"
  } else {
    colour
  }

  # Handle size with proper rel() support
  if (rlang::is_null(size)) {
    text_size <- resolved_text_element$size %||% 11
    # Handle case where theme size might be rel()
    if (inherits(text_size, "rel")) {
      base_size <- current_theme$text$size %||% 11
      text_size <- as.numeric(text_size) * base_size
    }
  } else {
    if (inherits(size, "rel")) {
      # Apply user's rel() to the resolved theme size
      theme_size <- resolved_text_element$size %||% 11
      if (inherits(theme_size, "rel")) {
        base_size <- current_theme$text$size %||% 11
        theme_abs_size <- as.numeric(theme_size) * base_size
      } else {
        theme_abs_size <- theme_size
      }
      text_size <- as.numeric(size) * theme_abs_size
    } else {
      text_size <- size
    }
  }

  text_family <- if (rlang::is_null(family)) {
    resolved_text_element$family %||% ""
  } else {
    family
  }

  # Build hierarchy for axis ticks length from most specific to least specific
  length_specific <- paste0("axis.ticks.length.", axis, ".", position)
  length_axis <- paste0("axis.ticks.length.", axis)
  length_general <- "axis.ticks.length"

  # Use calc_element to properly resolve length with inheritance
  resolved_length_element <- NULL
  for (element_name in c(length_specific, length_axis, length_general)) {
    element <- ggplot2::calc_element(element_name, current_theme, skip_blank = TRUE)
    if (!is.null(element) && !inherits(element, "element_blank")) {
      resolved_length_element <- element
      break
    }
  }

  # Calculate default length with proper rel() handling
  if (rlang::is_null(length)) {
    # Get resolved tick length
    tick_length <- resolved_length_element

    if (is.null(tick_length)) {
      # Fallback: use default rel(0.66) equivalent
      base_size <- current_theme$text$size %||% 11
      tick_length <- grid::unit(0.66 * base_size, "pt")
    } else if (inherits(tick_length, "rel")) {
      # calc_element returned a rel() object - convert it
      base_size <- current_theme$text$size %||% 11
      tick_length <- grid::unit(as.numeric(tick_length) * base_size, "pt")
    } else if (!inherits(tick_length, "unit")) {
      # If not a unit or rel, convert assuming points
      if (is.numeric(tick_length)) {
        tick_length <- grid::unit(tick_length, "pt")
      } else {
        # Ultimate fallback
        base_size <- current_theme$text$size %||% 11
        tick_length <- grid::unit(0.66 * base_size, "pt")
      }
    }
    # If tick_length is already a proper unit, use as-is

    length <- tick_length + grid::unit(3, "pt")
  } else {
    # Handle user-provided length
    if (inherits(length, "rel")) {
      # Get the base length for rel() calculation - look at raw theme element
      base_length_for_rel <- NULL

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
          user_length <- grid::unit(as.numeric(length) * theme_abs_length, "pt")
        } else if (inherits(base_length_for_rel, "unit")) {
          # Theme is absolute unit - apply user's rel to that
          theme_abs_length <- as.numeric(grid::convertUnit(base_length_for_rel, "pt"))
          user_length <- grid::unit(as.numeric(length) * theme_abs_length, "pt")
        } else if (is.numeric(base_length_for_rel)) {
          # Theme is numeric - assume points
          user_length <- grid::unit(as.numeric(length) * base_length_for_rel, "pt")
        } else {
          # Fallback to default base
          base_size <- current_theme$text$size %||% 11
          default_length <- 0.66 * base_size
          user_length <- grid::unit(as.numeric(length) * default_length, "pt")
        }
      } else {
        # No theme element found - use default rel(0.66) as base
        base_size <- current_theme$text$size %||% 11
        default_length <- 0.66 * base_size
        user_length <- grid::unit(as.numeric(length) * default_length, "pt")
      }

      length <- user_length + grid::unit(3, "pt")
    } else if (inherits(length, "unit")) {
      # If already a unit, use as-is
      length <- length
    } else if (is.numeric(length)) {
      # Convert numeric to unit
      length <- grid::unit(length, "pt")
    } else {
      # Fallback
      base_size <- current_theme$text$size %||% 11
      length <- grid::unit(0.66 * base_size, "pt") + grid::unit(3, "pt")
    }
  }

  #added this in. Don't know why it works
  length <- length + grid::unit(2.2, "pt")

  # Set text justification based on position
  if (position %in% c("top", "bottom")) {
    # x-axis text
    text_hjust <- if (rlang::is_null(hjust)) 0.5 else hjust # center horizontally
    text_vjust <- if (rlang::is_null(vjust)) {
      if (position == "bottom") 1 else 0 # top-align for bottom, bottom-align for top
    } else {
      vjust
    }
  } else {
    # y-axis text
    text_hjust <- if (rlang::is_null(hjust)) {
      if (position == "left") 1 else 0 # right-align for left, left-align for right
    } else {
      hjust
    }
    text_vjust <- if (rlang::is_null(vjust)) 0.5 else vjust # center vertically
  }

  # Initialize list to store annotation layers and theme modifications
  stamp <- list()

  # Add theme modifications to hide/modify original axis text
  if (theme_element == "transparent") {
    theme_element_name <- paste0("axis.text.", axis, ".", position)
    theme_mod <- list()
    theme_mod[[theme_element_name]] <- ggplot2::element_text(
      colour = "transparent"
    )
    stamp <- c(stamp, list(rlang::exec(ggplot2::theme, !!!theme_mod)))
  } else if (theme_element == "blank") {
    theme_element_name <- paste0("axis.text.", axis, ".", position)
    theme_mod <- list()
    theme_mod[[theme_element_name]] <- ggplot2::element_blank()
    stamp <- c(stamp, list(rlang::exec(ggplot2::theme, !!!theme_mod)))
  }

  # Create text annotations for each break/label pair
  for (i in seq_along(breaks)) {
    if (position %in% c("top", "bottom")) {
      # x-axis text
      # Calculate text dimensions using a temporary text grob
      temp_text_grob <- grid::textGrob(
        label = labels[i],
        gp = grid::gpar(
          fontsize = text_size,
          fontfamily = text_family
        )
      )
      text_width <- grid::grobWidth(temp_text_grob)
      text_height <- grid::grobHeight(temp_text_grob)

      # Create background rectangle for x-axis text
      rect_grob <- grid::rectGrob(
        x = grid::unit(0.5, "npc"),
        y = if (position == "bottom") {
          grid::unit(0, "npc") - length
        } else {
          grid::unit(1, "npc") + length
        },
        width = text_width + margin_left + margin_right,
        height = text_height + margin_top + margin_bottom,
        hjust = text_hjust,
        vjust = text_vjust,
        gp = grid::gpar(
          fill = fill,
          col = NA
        )
      )

      # Create text grob for x-axis
      text_grob <- grid::textGrob(
        label = labels[i],
        x = grid::unit(0.5, "npc"), # centered horizontally
        y = if (position == "bottom") {
          grid::unit(0, "npc") - length # below panel
        } else {
          grid::unit(1, "npc") + length # above panel
        },
        hjust = text_hjust,
        vjust = text_vjust,
        rot = angle,
        gp = grid::gpar(
          col = text_colour,
          fontsize = text_size,
          fontfamily = text_family
        )
      )

      # Set annotation position for x-axis
      annotation_position <- if (position == "bottom") {
        list(xmin = breaks[i], xmax = breaks[i], ymin = -Inf, ymax = -Inf)
      } else {
        list(xmin = breaks[i], xmax = breaks[i], ymin = Inf, ymax = Inf)
      }
    } else {
      # y-axis text
      # Calculate text dimensions using a temporary text grob
      temp_text_grob <- grid::textGrob(
        label = labels[i],
        gp = grid::gpar(
          fontsize = text_size,
          fontfamily = text_family
        )
      )
      text_width <- grid::grobWidth(temp_text_grob)
      text_height <- grid::grobHeight(temp_text_grob)

      # Create background rectangle for y-axis text
      rect_grob <- grid::rectGrob(
        x = if (position == "left") {
          grid::unit(0, "npc") - length
        } else {
          grid::unit(1, "npc") + length
        },
        y = grid::unit(0.5, "npc"),
        width = text_width + margin_left + margin_right,
        height = text_height + margin_top + margin_bottom,
        hjust = text_hjust,
        vjust = text_vjust,
        gp = grid::gpar(
          fill = fill,
          col = NA
        )
      )

      # Create text grob for y-axis
      text_grob <- grid::textGrob(
        label = labels[i],
        x = if (position == "left") {
          grid::unit(0, "npc") - length # left of panel
        } else {
          grid::unit(1, "npc") + length # right of panel
        },
        y = grid::unit(0.5, "npc"), # centered vertically
        hjust = text_hjust,
        vjust = text_vjust,
        rot = angle,
        gp = grid::gpar(
          col = text_colour,
          fontsize = text_size,
          fontfamily = text_family
        )
      )

      # Set annotation position for y-axis
      annotation_position <- if (position == "left") {
        list(xmin = -Inf, xmax = -Inf, ymin = breaks[i], ymax = breaks[i])
      } else {
        list(xmin = Inf, xmax = Inf, ymin = breaks[i], ymax = breaks[i])
      }
    }

    # Add background rectangle first, then text on top
    stamp <- c(
      stamp,
      list(
        rlang::exec(
          ggplot2::annotation_custom,
          grob = rect_grob,
          !!!annotation_position
        )
      ),
      list(
        rlang::exec(
          ggplot2::annotation_custom,
          grob = text_grob,
          !!!annotation_position
        )
      )
    )
  }

  return(stamp)
}
