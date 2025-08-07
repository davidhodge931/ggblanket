#' Annotated axis text labels
#'
#' @description Add text labels positioned relative to axis tick marks using absolute measurements.
#' This function only works when panel dimensions are set via panel.widths and panel.heights.
#' It requires a `coord` of `ggplot2::coord_cartesian(clip = "off")`.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param axis The axis to annotate. One of "x" or "y".
#' @param breaks A vector of axis breaks for text positioning.
#' @param position The position of the axis text. For x-axis: "bottom" or "top". For y-axis: "left" or "right". Defaults to "bottom" for x-axis and "left" for y-axis.
#' @param labels A vector of text labels or a function that takes breaks and returns labels. If NULL, uses the breaks as labels.
#' @param colour The colour of the text. Inherits from the current theme axis.text etc.
#' @param size The size of the text. Inherits from the current theme axis.text etc.
#' @param family The font family of the text. Inherits from the current theme axis.text etc.
#' @param length The total distance from the axis line to the text as a grid unit. Defaults to the sum of tick length plus 3pt offset.
#' @param margin The margin around the background rectangle. Can be a single unit value (applied to all sides) or a margin object with top, right, bottom, left components. Defaults to unit(2, "pt").
#' @param fill The fill colour of the background rectangle. If NULL, intelligently derived from theme (panel background for light themes, text colour for dark themes).
#' @param hjust,vjust Horizontal and vertical justification. Auto-calculated based on position if NULL.
#' @param angle Text rotation angle. Defaults to 0.
#' @param theme_element What to do with the equivalent theme element. Either "transparent", "keep" or "blank". Defaults "transparent".
#'
#' @return A list of annotation layers and theme elements.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' set_blanket(
#'   theme = theme_lighter(
#'     panel_heights = rep(unit(50, "mm"), 100),
#'     panel_widths = rep(unit(75, "mm"), 100),
#'   ),
#' )
#'
#' p <- palmerpenguins::penguins |>
#'   gg_blanket(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     x_title = "Flipper length",
#'     y_title = "Body mass",
#'   )
#'
#' y_breaks <- seq(2500, 6500, 500)
#'
#' key <- 4873
#'
#' p +
#'   geom_hline(yintercept = key) +
#'   annotate_axis_text(
#'     axis = "y",
#'     breaks = key,
#'     position = "right",
#'     labels = scales::comma(key),
#'   ) +
#'   geom_point(
#'     colour = col_multiply(get_geom_defaults("point")$colour),
#'   )
#'
#' p +
#'   annotate_axis_text(
#'     axis = "y",
#'     breaks = 6500,
#'     labels = "kg",
#'     hjust = 0,
#'     length = unit(0, "pt"),
#'     theme_element = "keep",
#'   ) +
#'   annotate_axis_text(
#'     axis = "x",
#'     breaks = I(1),
#'     labels = "mm",
#'     hjust = 0,
#'     margin = unit(1, "pt"),
#'     theme_element = "keep",
#'   ) +
#'   geom_point(
#'     colour = col_multiply(get_geom_defaults("point")$colour),
#'   )
#'
#' p +
#'   annotate_axis_text(
#'     axis = "y",
#'     breaks = y_breaks[-length(y_breaks)],
#'     hjust = 0,
#'     vjust = -0.4,
#'     length = unit(0, "pt"),
#'     theme_element = "blank",
#'   ) +
#'   geom_point(
#'     colour = col_multiply(get_geom_defaults("point")$colour),
#'   )
#'
annotate_axis_text <- function(
    ...,
    axis,
    breaks,
    position = NULL,
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
  # Validate axis argument
  if (!axis %in% c("x", "y")) {
    rlang::abort("axis must be one of 'x' or 'y'")
  }

  # Set default position based on axis
  if (rlang::is_null(position)) {
    position <- if (axis == "x") "bottom" else "left"
  }

  # Validate position for each axis
  if (axis == "x" && !position %in% c("bottom", "top")) {
    rlang::abort("For x-axis, position must be one of 'bottom' or 'top'")
  }
  if (axis == "y" && !position %in% c("left", "right")) {
    rlang::abort("For y-axis, position must be one of 'left' or 'right'")
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

  # Set default fill colour based on theme
  if (rlang::is_null(fill)) {
    # Always use panel background to mask gridlines
    fill <- current_theme$panel.background$fill %||% "white"
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

  # Set default margin for background rectangle
  if (rlang::is_null(margin)) {
    margin <- grid::unit(2, "pt")
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

  # Use calc_element with the most specific element names
  text_element_name <- paste0("axis.text.", axis, ".", position)
  length_element_name <- paste0("axis.ticks.length.", axis, ".", position)

  resolved_text_element <- ggplot2::calc_element(text_element_name, current_theme)
  resolved_length_element <- ggplot2::calc_element(length_element_name, current_theme)

  # Calculate default length with proper rel() handling
  if (rlang::is_null(length)) {
    # Get resolved tick length
    tick_length <- resolved_length_element %||% grid::unit(11 / 3, "pt")

    # Handle rel() objects for tick length
    if (inherits(tick_length, "rel")) {
      # Convert rel() to absolute unit - use default base of 11/3 pt
      tick_length <- grid::unit(as.numeric(tick_length) * 11 / 3, "pt")
    } else if (!inherits(tick_length, "unit")) {
      # Ensure it's a proper unit object
      tick_length <- grid::unit(11 / 3, "pt")
    }

    length <- tick_length + grid::unit(3, "pt")
  } else {
    # Handle user-provided length
    if (inherits(length, "rel")) {
      # Get the resolved theme tick length as base
      theme_tick_length <- resolved_length_element %||% grid::unit(11 / 3, "pt")

      # Convert theme length to numeric points for rel() calculation
      if (inherits(theme_tick_length, "rel")) {
        # If theme length is also rel(), use default base
        base_length_pts <- 11 / 3
      } else if (inherits(theme_tick_length, "unit")) {
        # Convert unit to points
        base_length_pts <- as.numeric(grid::convertUnit(theme_tick_length, "pt"))
      } else {
        base_length_pts <- 11 / 3
      }

      # Apply user's rel() to the base length, then add 3pt offset
      length <- grid::unit(as.numeric(length) * base_length_pts, "pt") + grid::unit(3, "pt")
    } else if (!inherits(length, "unit")) {
      # Convert numeric to unit
      length <- grid::unit(length, "pt")
    }
    # If already a unit, use as-is
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
  } else {
    if (inherits(size, "rel")) {
      # Apply user's rel() to the resolved theme size
      base_size <- resolved_text_element$size %||% 11
      text_size <- as.numeric(size) * base_size
    } else {
      text_size <- size
    }
  }

  text_family <- if (rlang::is_null(family)) {
    resolved_text_element$family %||% ""
  } else {
    family
  }

  # Set text justification based on axis and position
  if (axis == "x") {
    text_hjust <- if (rlang::is_null(hjust)) 0.5 else hjust # center horizontally
    text_vjust <- if (rlang::is_null(vjust)) {
      if (position == "bottom") 1 else 0 # top-align for bottom, bottom-align for top
    } else {
      vjust
    }
  } else {
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
    if (axis == "x") {
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
        width = text_width + margin_left + margin_right, # Add left and right margins
        height = text_height + margin_top + margin_bottom, # Add top and bottom margins
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
        width = text_width + margin_left + margin_right, # Add left and right margins
        height = text_height + margin_top + margin_bottom, # Add top and bottom margins
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
