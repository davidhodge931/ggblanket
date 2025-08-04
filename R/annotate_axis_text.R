#' Annotated axis text labels
#'
#' @description Add text labels positioned relative to axis tick marks using absolute measurements.
#' This function only works when panel dimensions are set uniformly via panel.widths and panel.heights.
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
    hjust = NULL,
    vjust = NULL,
    angle = 0,
    theme_elements = "transparent"
) {
  # Inform user about clipping requirement
  rlang::inform(
    "Please use this function with ggplot2::coord_cartesian(clip = 'off')"
  )

  # Validate axis argument
  if (!axis %in% c("x", "y")) {
    rlang::abort("axis must be one of 'x' or 'y'")
  }

  # Set default position based on axis
  if (is.null(position)) {
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
  if (is.null(labels)) {
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
  current_theme <- ggplot2::get_theme()
  panel_widths <- current_theme$panel.widths
  panel_heights <- current_theme$panel.heights

  if (is.null(panel_widths) && is.null(panel_heights)) {
    rlang::abort(
      "This function only works when panel dimensions are explicitly set via theme(panel.widths = ..., panel.heights = ...)"
    )
  }

  # Validate uniform panel dimensions for the specific axis
  if (axis == "x") {
    if (is.null(panel_heights)) {
      rlang::abort("panel.heights must be set in theme for x-axis text annotation")
    }
    if (length(panel_heights) > 1 && length(unique(as.numeric(panel_heights))) > 1) {
      rlang::abort("Different panel heights set. This function only works with uniform panel dimensions.")
    }
  } else {
    if (is.null(panel_widths)) {
      rlang::abort("panel.widths must be set in theme for y-axis text annotation")
    }
    if (length(panel_widths) > 1 && length(unique(as.numeric(panel_widths))) > 1) {
      rlang::abort("Different panel widths set. This function only works with uniform panel dimensions.")
    }
  }

  # Set default total distance from axis to text
  if (is.null(length)) {
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
    if (is.null(theme_size)) return(default)

    if (inherits(theme_size, "rel")) {
      base_size <- if (is.null(base_theme_size) || inherits(base_theme_size, "rel")) {
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
    text_hjust <- if (is.null(hjust)) 0.5 else hjust  # center horizontally
    text_vjust <- if (is.null(vjust)) {
      if (position == "bottom") 1 else 0  # top-align for bottom, bottom-align for top
    } else vjust
  } else {
    text_hjust <- if (is.null(hjust)) {
      if (position == "left") 1 else 0  # right-align for left, left-align for right
    } else hjust
    text_vjust <- if (is.null(vjust)) 0.5 else vjust  # center vertically
  }

  # Initialize list to store annotation layers and theme modifications
  stamp <- list()

  # Add theme modifications to hide/modify original axis text
  if (theme_elements == "transparent") {
    theme_element <- paste0("axis.text.", axis, ".", position)
    theme_mod <- list()
    theme_mod[[theme_element]] <- ggplot2::element_text(colour = "transparent")
    stamp <- c(stamp, list(do.call(ggplot2::theme, theme_mod)))
  } else if (theme_elements == "blank") {
    theme_element <- paste0("axis.text.", axis, ".", position)
    theme_mod <- list()
    theme_mod[[theme_element]] <- ggplot2::element_blank()
    stamp <- c(stamp, list(do.call(ggplot2::theme, theme_mod)))
  }

  # Create text annotations for each break/label pair
  for (i in seq_along(breaks)) {
    if (axis == "x") {
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

    # Add annotation to stamp list
    stamp <- c(
      stamp,
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
