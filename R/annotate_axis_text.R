#' Annotated axis text labels
#'
#' @description Add text labels positioned relative to axis tick marks using absolute measurements.
#' This function only works when panel dimensions are set via panel.widths and panel.heights.
#' It requires a `coord` of `ggplot2::coord_cartesian(clip = "off")`.
#'
#' @param axis The axis to annotate. One of "x" or "y".
#' @param ... Provided to require argument naming, support trailing commas etc.
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
#' @param theme_elements What to do with theme axis text elements. Either "transparent", "keep" or "blank". Defaults "transparent".
#'
#' @return A list of annotation layers and theme elements.
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
#' # Using vector labels
#' penguins |>
#'   tidyr::drop_na(sex) |>
#'   mutate(across(sex, \(x) str_to_sentence(x))) |>
#'   gg_blanket(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = sex,
#'   ) +
#'   annotate_axis_text(
#'     axis = "x",
#'     breaks = c(185, 195, 205, 215, 225),
#'     labels = c("185mm", "195mm", "205mm", "215mm", "225mm")
#'   ) +
#'   geom_point()
#'
#' # Using function labels
#' penguins |>
#'   gg_blanket(x = flipper_length_mm, y = body_mass_g) +
#'   annotate_axis_text(
#'     axis = "x",
#'     breaks = c(185, 195, 205, 215, 225),
#'     labels = function(x) paste0(x, "mm")
#'   ) +
#'   annotate_axis_text(
#'     axis = "y",
#'     breaks = c(3500, 4500, 5500),
#'     labels = function(x) paste0(x/1000, "kg")
#'   ) +
#'   geom_point()
#'
annotate_axis_text <- function(
    axis,
    ...,
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
    theme_elements = "transparent"
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

  # Check if panel dimensions are explicitly set (required for positioning)
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
    # Get text colour from theme
    theme_text <- current_theme$axis.text.x$colour %||%
      current_theme$axis.text.y$colour %||%
      current_theme$axis.text$colour %||%
      current_theme$text$colour %||%
      "black"

    # Get panel background from theme
    theme_panel <- current_theme$panel.background$fill %||%
      "white"

    # Always use panel background to mask gridlines
    fill <- theme_panel
  }

  # Validate uniform panel dimensions for the specific axis
  if (axis == "x") {
    if (rlang::is_null(panel_heights)) {
      rlang::abort("panel.heights must be set in theme for x-axis text annotation")
    }
    if (length(panel_heights) > 1 && length(unique(as.numeric(panel_heights))) > 1) {
      rlang::abort("Different panel heights set. This function only works with uniform panel dimensions.")
    }
  } else {
    if (rlang::is_null(panel_widths)) {
      rlang::abort("panel.widths must be set in theme for y-axis text annotation")
    }
    if (length(panel_widths) > 1 && length(unique(as.numeric(panel_widths))) > 1) {
      rlang::abort("Different panel widths set. This function only works with uniform panel dimensions.")
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

    # Debug: print margin values
    # print(paste("Margin values - t:", margin_top, "r:", margin_right, "b:", margin_bottom, "l:", margin_left))
  } else {
    # Single value applied to all sides
    margin_top <- margin_right <- margin_bottom <- margin_left <- margin
  }
  if (rlang::is_null(length)) {
    # Calculate default as tick length + 3pt offset
    tick_length <- if (axis == "x") {
      current_theme[[paste0("axis.ticks.length.x.", position)]] %||%
        current_theme[["axis.ticks.length.x"]] %||%
        current_theme[["axis.ticks.length"]] %||%
        grid::unit(11 / 3, "pt")
    } else {
      current_theme[[paste0("axis.ticks.length.y.", position)]] %||%
        current_theme[["axis.ticks.length.y"]] %||%
        current_theme[["axis.ticks.length"]] %||%
        grid::unit(11 / 3, "pt")
    }
    length <- tick_length + grid::unit(3, "pt")
  }

  # Helper function to extract theme properties with fallback hierarchy
  extract_theme_property <- function(property, default) {
    if (axis == "x") {
      current_theme[[paste0("axis.text.x.", position)]][[property]] %||%
        current_theme[["axis.text.x"]][[property]] %||%
        current_theme[["axis.text"]][[property]] %||%
        current_theme[["text"]][[property]] %||%
        default
    } else {
      current_theme[[paste0("axis.text.y.", position)]][[property]] %||%
        current_theme[["axis.text.y"]][[property]] %||%
        current_theme[["axis.text"]][[property]] %||%
        current_theme[["text"]][[property]] %||%
        default
    }
  }

  # Helper function to resolve theme size (handles rel() objects)
  resolve_theme_size <- function(theme_size, base_theme_size, default = 11) {
    if (rlang::is_null(theme_size)) return(default)

    if (inherits(theme_size, "rel")) {
      base_size <- if (rlang::is_null(base_theme_size) || inherits(base_theme_size, "rel")) {
        default
      } else {
        base_theme_size
      }
      return(as.numeric(theme_size) * base_size)
    }

    return(theme_size)
  }

  # Extract text properties from theme or use provided values
  text_colour <- if (rlang::is_null(colour)) {
    extract_theme_property("colour", "black")
  } else {
    colour
  }

  text_size <- if (rlang::is_null(size)) {
    raw_size <- extract_theme_property("size", NULL)
    resolve_theme_size(raw_size, current_theme$text$size, 11)
  } else {
    size
  }

  text_family <- if (rlang::is_null(family)) {
    extract_theme_property("family", "")
  } else {
    family
  }

  # Set text justification based on axis and position
  if (axis == "x") {
    text_hjust <- if (rlang::is_null(hjust)) 0.5 else hjust  # center horizontally
    text_vjust <- if (rlang::is_null(vjust)) {
      if (position == "bottom") 1 else 0  # top-align for bottom, bottom-align for top
    } else vjust
  } else {
    text_hjust <- if (rlang::is_null(hjust)) {
      if (position == "left") 1 else 0  # right-align for left, left-align for right
    } else hjust
    text_vjust <- if (rlang::is_null(vjust)) 0.5 else vjust  # center vertically
  }

  # Initialize list to store annotation layers and theme modifications
  stamp <- list()

  # Add theme modifications to hide/modify original axis text
  if (theme_elements == "transparent") {
    theme_element <- paste0("axis.text.", axis, ".", position)
    theme_mod <- list()
    theme_mod[[theme_element]] <- ggplot2::element_text(colour = "transparent")
    stamp <- c(stamp, list(rlang::exec(ggplot2::theme, !!!theme_mod)))
  } else if (theme_elements == "blank") {
    theme_element <- paste0("axis.text.", axis, ".", position)
    theme_mod <- list()
    theme_mod[[theme_element]] <- ggplot2::element_blank()
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
        width = text_width + margin_left + margin_right,  # Add left and right margins
        height = text_height + margin_top + margin_bottom,  # Add top and bottom margins
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
        x = grid::unit(0.5, "npc"),  # centered horizontally
        y = if (position == "bottom") {
          grid::unit(0, "npc") - length  # below panel
        } else {
          grid::unit(1, "npc") + length  # above panel
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
        width = text_width + margin_left + margin_right,  # Add left and right margins
        height = text_height + margin_top + margin_bottom,  # Add top and bottom margins
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
          grid::unit(0, "npc") - length  # left of panel
        } else {
          grid::unit(1, "npc") + length  # right of panel
        },
        y = grid::unit(0.5, "npc"),  # centered vertically
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
