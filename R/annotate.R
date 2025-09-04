#' Annotate axis line segment
#'
#' @description Create an annotated segment of the axis line.
#'
#' This function is designed to work with a theme that is globally set with [ggblanket::set_blanket] or [ggplot2::set_theme].
#'
#' It should be used with a `coord` of `ggplot2::coord_cartesian(clip = "off")`.
#'
#' Note that this function does not support plots where either positional scale is of date or datetime class. Use [ggplot2::geom_segment], [ggplot2::geom_hline] or [ggplot2::geom_vline] instead.
#'
#' @param ... Arguments passed to `ggplot2::annotate("segment", ....)` (if normalised coordinates not used). Require named arguments (and support trailing commas).
#' @param position The position of the axis line. One of `"top"`, `"bottom"`, `"left"`, or `"right"`. Ignored if `x` or `y` is provided.
#' @param x A single x-axis value for a vertical line. Cannot be used together with `y` or `xmin`/`xmax`. Use `I()` for normalized coordinates (0-1).
#' @param y A single y-axis value for a horizontal line. Cannot be used together with `x` or `ymin`/`ymax`. Use `I()` for normalized coordinates (0-1).
#' @param xmin The starting x position for a horizontal line segment. Use `I()` for normalized coordinates (0-1).
#' @param xmax The ending x position for a horizontal line segment. Use `I()` for normalized coordinates (0-1).
#' @param ymin The starting y position for a vertical line segment. Use `I()` for normalized coordinates (0-1).
#' @param ymax The ending y position for a vertical line segment. Use `I()` for normalized coordinates (0-1).
#' @param colour The colour of the annotated segment. Inherits from the current theme axis.line etc.
#' @param linewidth The linewidth of the annotated segment. Inherits from the current theme axis.line etc.
#' @param theme How to modify the corresponding theme element. One of `"keep"`, `"transparent"`, or `"blank"`.
#'   Defaults to `"keep"`.
#'
#' @return A list of annotation annotates and theme elements.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' set_blanket(
#'   theme = theme_greyer(
#'     panel_heights = rep(unit(50, "mm"), 100),
#'     panel_widths = rep(unit(75, "mm"), 100),
#'   ),
#' )
#'
#' p <- palmerpenguins::penguins |>
#'   gg_blanket(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     border = TRUE,
#'   )
#'
#' # Full axis line at bottom
#' p +
#' annotate_axis_line(position = "bottom") +
#' geom_point()
#'
#' # Vertical line at x=200, partial height
#' p + annotate_axis_line(x = 200, ymin = I(0.25), ymax = I(0.75))
#'
#' # Horizontal line at y=4000, partial width
#' p + annotate_axis_line(y = 4000, xmin = 180, xmax = 220)
#'
annotate_axis_line <- function(
    ...,
    position = NULL,
    x = NULL,
    y = NULL,
    xmin = NULL,
    xmax = NULL,
    ymin = NULL,
    ymax = NULL,
    colour = NULL,
    linewidth = NULL,
    theme = "keep"
) {
  # Validate arguments - can't have both x and y
  if (!rlang::is_null(x) && !rlang::is_null(y)) {
    rlang::abort("Cannot specify both x and y. Use either x for a vertical line or y for a horizontal line.")
  }

  # Can't mix x with xmin/xmax or y with ymin/ymax
  if (!rlang::is_null(x) && (!rlang::is_null(xmin) || !rlang::is_null(xmax))) {
    rlang::abort("Cannot specify both x and xmin/xmax. Use either x for a single position or xmin/xmax for endpoints.")
  }
  if (!rlang::is_null(y) && (!rlang::is_null(ymin) || !rlang::is_null(ymax))) {
    rlang::abort("Cannot specify both y and ymin/ymax. Use either y for a single position or ymin/ymax for endpoints.")
  }

  # If x or y is provided, it overrides position
  use_xy_positioning <- !rlang::is_null(x) || !rlang::is_null(y)

  if (use_xy_positioning) {
    # Check if using normalized coordinates
    x_is_normalized <- !rlang::is_null(x) && inherits(x, "AsIs")
    y_is_normalized <- !rlang::is_null(y) && inherits(y, "AsIs")
    xmin_is_normalized <- !rlang::is_null(xmin) && inherits(xmin, "AsIs")
    xmax_is_normalized <- !rlang::is_null(xmax) && inherits(xmax, "AsIs")
    ymin_is_normalized <- !rlang::is_null(ymin) && inherits(ymin, "AsIs")
    ymax_is_normalized <- !rlang::is_null(ymax) && inherits(ymax, "AsIs")

    # Unwrap and validate I() values
    if (x_is_normalized) {
      x <- unclass(x)
      if (length(x) != 1 || x < 0 || x > 1) {
        rlang::abort("Normalized x (specified with I()) must be a single value between 0 and 1")
      }
    } else if (!rlang::is_null(x) && length(x) != 1) {
      rlang::abort("x must be a single value")
    }

    if (y_is_normalized) {
      y <- unclass(y)
      if (length(y) != 1 || y < 0 || y > 1) {
        rlang::abort("Normalized y (specified with I()) must be a single value between 0 and 1")
      }
    } else if (!rlang::is_null(y) && length(y) != 1) {
      rlang::abort("y must be a single value")
    }

    if (xmin_is_normalized) {
      xmin <- unclass(xmin)
      if (length(xmin) != 1 || xmin < 0 || xmin > 1) {
        rlang::abort("Normalized xmin (specified with I()) must be a single value between 0 and 1")
      }
    }
    if (xmax_is_normalized) {
      xmax <- unclass(xmax)
      if (length(xmax) != 1 || xmax < 0 || xmax > 1) {
        rlang::abort("Normalized xmax (specified with I()) must be a single value between 0 and 1")
      }
    }
    if (ymin_is_normalized) {
      ymin <- unclass(ymin)
      if (length(ymin) != 1 || ymin < 0 || ymin > 1) {
        rlang::abort("Normalized ymin (specified with I()) must be a single value between 0 and 1")
      }
    }
    if (ymax_is_normalized) {
      ymax <- unclass(ymax)
      if (length(ymax) != 1 || ymax < 0 || ymax > 1) {
        rlang::abort("Normalized ymax (specified with I()) must be a single value between 0 and 1")
      }
    }

    # Determine axis from x/y
    axis <- if (!rlang::is_null(x)) "y" else "x"  # Note: vertical line is on y axis, horizontal on x axis

    # Determine if we're using normalized coordinates based on ANY normalized input
    use_normalized <- x_is_normalized || y_is_normalized || xmin_is_normalized || xmax_is_normalized ||
      ymin_is_normalized || ymax_is_normalized
  } else {
    # Original position-based behavior
    if (rlang::is_null(position)) {
      rlang::abort("Must specify either position, x, or y")
    }

    position <- rlang::arg_match(position, c("top", "bottom", "left", "right"))

    # Determine axis from position
    axis <- if (position %in% c("top", "bottom")) "x" else "y"
    use_normalized <- FALSE
  }

  theme <- rlang::arg_match(theme, c("keep", "transparent", "blank"))

  # Get current theme and calculate resolved element properties
  current_theme <- ggplot2::theme_get()

  # Build hierarchy of element names from most specific to least specific
  if (use_xy_positioning) {
    element_hierarchy <- c(
      paste0("axis.line.", axis),
      "axis.line"
    )
  } else {
    specific_element <- paste0("axis.line.", axis, ".", position)
    axis_element <- paste0("axis.line.", axis)
    general_element <- "axis.line"
    element_hierarchy <- c(specific_element, axis_element, general_element)
  }

  # Find the first non-blank resolved element
  resolved_element <- element_hierarchy |>
    purrr::map(\(x) ggplot2::calc_element(x, current_theme, skip_blank = TRUE)) |>
    purrr::detect(\(x) !rlang::is_null(x) && !inherits(x, "element_blank"))

  # If still no element found, create a minimal fallback
  if (rlang::is_null(resolved_element)) {
    resolved_element <- list(colour = "black", linewidth = 0.5)
  }

  # Extract theme properties with proper resolution
  line_colour <- if (rlang::is_null(colour)) {
    resolved_element@colour %||% "black"
  } else {
    colour
  }

  # Handle linewidth with proper rel() support
  if (rlang::is_null(linewidth)) {
    line_linewidth <-  resolved_element@linewidth %||% 0.5
  } else {
    if (inherits(linewidth, "rel")) {
      base_linewidth <-  resolved_element@linewidth %||% 0.5
      line_linewidth <- as.numeric(linewidth) * base_linewidth
    } else {
      line_linewidth <- linewidth
    }
  }

  stamp <- list()

  # Create axis segment based on positioning method
  if (use_xy_positioning) {
    if (!rlang::is_null(x)) {
      # Vertical line
      # Set defaults for endpoints based on whether we're using normalized coordinates
      if (rlang::is_null(ymin)) {
        ymin <- if (use_normalized) 0 else -Inf
      }
      if (rlang::is_null(ymax)) {
        ymax <- if (use_normalized) 1 else Inf
      }

      if (use_normalized) {
        # Create normalized grob
        line_grob <- grid::linesGrob(
          x = grid::unit(c(x, x), "npc"),
          y = grid::unit(c(ymin, ymax), "npc"),
          gp = grid::gpar(
            col = line_colour,
            lwd = line_linewidth * 72 / 25.4
          )
        )
        stamp <- c(
          stamp,
          list(
            ggplot2::annotation_custom(
              grob = line_grob,
              xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
            )
          )
        )
      } else {
        # Data coordinates
        stamp <- c(
          stamp,
          list(
            rlang::exec(
              ggplot2::annotate,
              "segment",
              x = x,
              xend = x,
              y = ymin,
              yend = ymax,
              colour = line_colour,
              linewidth = line_linewidth,
              ...
            )
          )
        )
      }
    } else {
      # Horizontal line
      # Set defaults for endpoints based on whether we're using normalized coordinates
      if (rlang::is_null(xmin)) {
        xmin <- if (use_normalized) 0 else -Inf
      }
      if (rlang::is_null(xmax)) {
        xmax <- if (use_normalized) 1 else Inf
      }

      if (use_normalized) {
        # Create normalized grob
        line_grob <- grid::linesGrob(
          x = grid::unit(c(xmin, xmax), "npc"),
          y = grid::unit(c(y, y), "npc"),
          gp = grid::gpar(
            col = line_colour,
            lwd = line_linewidth * 72 / 25.4
          )
        )
        stamp <- c(
          stamp,
          list(
            ggplot2::annotation_custom(
              grob = line_grob,
              xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
            )
          )
        )
      } else {
        # Data coordinates
        stamp <- c(
          stamp,
          list(
            rlang::exec(
              ggplot2::annotate,
              "segment",
              x = xmin,
              xend = xmax,
              y = y,
              yend = y,
              colour = line_colour,
              linewidth = line_linewidth,
              ...
            )
          )
        )
      }
    }
  } else {
    # Original position-based behavior
    if (position == "bottom") {
      stamp <- c(
        stamp,
        list(
          rlang::exec(
            ggplot2::annotate,
            "segment",
            x = -Inf,
            xend = Inf,
            y = -Inf,
            yend = -Inf,
            colour = line_colour,
            linewidth = line_linewidth,
            ...
          )
        )
      )
    } else if (position == "top") {
      stamp <- c(
        stamp,
        list(
          rlang::exec(
            ggplot2::annotate,
            "segment",
            x = -Inf,
            xend = Inf,
            y = Inf,
            yend = Inf,
            colour = line_colour,
            linewidth = line_linewidth,
            ...
          )
        )
      )
    } else if (position == "left") {
      stamp <- c(
        stamp,
        list(
          rlang::exec(
            ggplot2::annotate,
            "segment",
            x = -Inf,
            xend = -Inf,
            y = -Inf,
            yend = Inf,
            colour = line_colour,
            linewidth = line_linewidth,
            ...
          )
        )
      )
    } else {
      # right
      stamp <- c(
        stamp,
        list(
          rlang::exec(
            ggplot2::annotate,
            "segment",
            x = Inf,
            xend = Inf,
            y = -Inf,
            yend = Inf,
            colour = line_colour,
            linewidth = line_linewidth,
            ...
          )
        )
      )
    }
  }

  # Add theme modification if requested (only for position-based, not x/y)
  if (!use_xy_positioning && theme != "keep") {
    if (theme == "transparent") {
      theme_name <- paste0("axis.line.", axis, ".", position)
      theme_mod <- list()
      theme_mod[[theme_name]] <- ggplot2::element_line(colour = "transparent")
      stamp <- c(stamp, list(rlang::exec(ggplot2::theme, !!!theme_mod)))
    } else if (theme == "blank") {
      theme_name <- paste0("axis.line.", axis, ".", position)
      theme_mod <- list()
      theme_mod[[theme_name]] <- ggplot2::element_blank()
      stamp <- c(stamp, list(rlang::exec(ggplot2::theme, !!!theme_mod)))
    }
  }

  return(stamp)
}

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

#' Annotate axis text segments
#'
#' @description Create annotated segments of the axis text.
#'
#' This function is designed to work with a theme that is globally set with [ggblanket::set_blanket] or [ggplot2::set_theme].
#'
#' It should be used with a `coord` of `ggplot2::coord_cartesian(clip = "off")`.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param position The position of the axis text. One of `"top"`, `"bottom"`, `"left"`, or `"right"`. Ignored if both `x` and `y` are provided.
#' @param x A vector of x-axis breaks for text positioning. Use `I()` to specify normalized coordinates (0-1).
#' @param y A vector of y-axis breaks for text positioning. Use `I()` to specify normalized coordinates (0-1).
#' @param label A vector of text labels or a function that takes breaks and returns labels. If `NULL`, uses appropriate formatting.
#' @param colour The colour of the text. Inherits from the current theme `axis.text` etc.
#' @param size The size of the text. Inherits from the current theme `axis.text` etc.
#' @param family The font family of the text. Inherits from the current theme `axis.text` etc.
#' @param length The total distance from the axis line to the text as a grid unit. Defaults to the sum of set theme tick length and relevant margin part.
#' @param margin The margin around the background rectangle. Can be a single unit value (applied to all sides) or a margin object with top, right, bottom, left components.
#' @param fill The fill colour of the background rectangle. If `NULL`, defaults to `"transparent"`.
#' @param hjust,vjust Horizontal and vertical justification. Auto-calculated based on position if `NULL`.
#' @param angle Text rotation angle. Defaults to `0`.
#' @param theme What to do with the equivalent theme elements. Either `"keep"`, `"transparent"`, or `"blank"`. Defaults to `"keep"`.
#'
#' @return A list of annotation annotates and theme elements.
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
            size = text_size / 2.845276,
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

#' Annotate panel grid segments
#'
#' @description Create annotated segments of the panel grid.
#'
#' This function is designed to work with a theme that is globally set with [ggblanket::set_blanket] or [ggplot2::set_theme].
#'
#' @param ... Arguments passed to `ggplot2::annotate("segment", ....)` (if normalised coordinates not used). Require named arguments (and support trailing commas).
#' @param x A vector of x-axis breaks for vertical grid lines. Cannot be used together with `y`. Use `I()` to specify normalized coordinates (0-1).
#' @param y A vector of y-axis breaks for horizontal grid lines. Cannot be used together with `x`. Use `I()` to specify normalized coordinates (0-1).
#' @param minor Logical. If `FALSE` (default), creates major grid lines. If `TRUE`, creates minor grid lines.
#' @param colour The colour of grid lines. Inherits from current theme `panel.grid.major` or `panel.grid.minor` etc.
#' @param linewidth The linewidth of grid lines. Inherits from current theme `panel.grid.major` or `panel.grid.minor` etc.
#' @param linetype The linetype of grid lines. Inherits from current theme `panel.grid.major` or `panel.grid.minor` etc.
#' @param theme What to do with the equivalent theme elements. Either `"keep"`, `"transparent"`, or `"blank"`. Defaults `"keep"`.
#'
#' @return A list of annotate annotates and theme elements.
#' @export
#'
annotate_panel_grid <- function(
    ...,
    x = NULL,
    y = NULL,
    minor = FALSE,
    colour = NULL,
    linewidth = NULL,
    linetype = NULL,
    theme = "keep"
) {
  # Validate arguments
  if (rlang::is_null(x) && rlang::is_null(y)) {
    rlang::abort("Either x or y must be specified")
  }

  if (!rlang::is_null(x) && !rlang::is_null(y)) {
    rlang::abort("Only one of x or y can be specified")
  }

  if (!theme %in% c("transparent", "keep", "blank")) {
    rlang::abort(
      "theme must be one of 'transparent', 'keep', or 'blank'"
    )
  }

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

  # Determine axis from x/y and whether using normalized coordinates
  axis <- if (!rlang::is_null(x)) "x" else "y"
  use_normalized <- if (axis == "x") x_is_normalized else y_is_normalized

  # Get breaks
  breaks <- if (!rlang::is_null(x)) x else y

  # Check for empty breaks
  if (length(breaks) == 0) {
    return(list())
  }

  # Get current theme
  current_theme <- ggplot2::theme_get()

  # Build hierarchy for panel grid based on whether minor or major
  if (minor) {
    # For minor grid
    grid_minor_specific <- paste0("panel.grid.minor.", axis)
    grid_minor <- "panel.grid.minor"
    grid_general <- "panel.grid"

    grid_hierarchy <- c(
      grid_minor_specific,
      grid_minor,
      grid_general
    )
  } else {
    # For major grid
    grid_major_specific <- paste0("panel.grid.major.", axis)
    grid_major <- "panel.grid.major"
    grid_general <- "panel.grid"

    grid_hierarchy <- c(
      grid_major_specific,
      grid_major,
      grid_general
    )
  }

  # Find the first non-blank resolved grid element
  resolved_grid_element <- grid_hierarchy |>
    purrr::map(\(x) ggplot2::calc_element(x, current_theme, skip_blank = TRUE)) |>
    purrr::detect(\(x) !rlang::is_null(x) && !inherits(x, "element_blank"))

  # If still no element found, create a minimal fallback
  if (rlang::is_null(resolved_grid_element)) {
    if (minor) {
      # Lighter defaults for minor grid
      resolved_grid_element <- list(
        colour = "grey95",
        linewidth = 0.25,
        linetype = "solid"
      )
    } else {
      # Standard defaults for major grid
      resolved_grid_element <- list(
        colour = "grey90",
        linewidth = 0.5,
        linetype = "solid"
      )
    }
  }

  # Extract theme properties with proper resolution
  grid_colour <- colour %||% resolved_grid_element$colour %||%
    (if (minor) "grey95" else "grey90")

  # Handle linewidth with proper rel() support
  if (rlang::is_null(linewidth)) {
    grid_linewidth <- resolved_grid_element$linewidth %||%
      (if (minor) 0.25 else 0.5)
  } else {
    if (inherits(linewidth, "rel")) {
      # Apply user's rel() to the resolved theme linewidth
      base_linewidth <- resolved_grid_element$linewidth %||%
        (if (minor) 0.25 else 0.5)
      grid_linewidth <- as.numeric(linewidth) * base_linewidth
    } else {
      grid_linewidth <- linewidth
    }
  }

  grid_linetype <- linetype %||% resolved_grid_element$linetype %||% "solid"

  stamp <- list()

  # Add theme modification if requested
  if (theme != "keep") {
    # Determine which theme element to modify based on minor flag
    if (minor) {
      element_name <- paste0("panel.grid.minor.", axis)
    } else {
      element_name <- paste0("panel.grid.major.", axis)
    }

    if (theme == "transparent") {
      stamp <- c(
        stamp,
        list(
          ggplot2::theme(
            !!element_name := ggplot2::element_line(colour = "transparent")
          )
        )
      )
    } else if (theme == "blank") {
      stamp <- c(
        stamp,
        list(
          ggplot2::theme(
            !!element_name := ggplot2::element_blank()
          )
        )
      )
    }
  }

  # Create grid lines based on coordinate type
  if (use_normalized) {
    # For normalized coordinates, we need to use annotation_custom with grobs
    grid_annotations <- breaks |>
      purrr::map(\(break_val) {
        if (axis == "x") {
          # Vertical grid line at normalized x position
          grid_grob <- grid::linesGrob(
            x = grid::unit(c(break_val, break_val), "npc"),
            y = grid::unit(c(0, 1), "npc"),
            gp = grid::gpar(
              col = grid_colour,
              lwd = grid_linewidth * 72 / 25.4,
              lty = grid_linetype
            )
          )
        } else {  # y axis
          # Horizontal grid line at normalized y position
          grid_grob <- grid::linesGrob(
            x = grid::unit(c(0, 1), "npc"),
            y = grid::unit(c(break_val, break_val), "npc"),
            gp = grid::gpar(
              col = grid_colour,
              lwd = grid_linewidth * 72 / 25.4,
              lty = grid_linetype
            )
          )
        }

        # For normalized coordinates, span the full plot area
        ggplot2::annotation_custom(
          grob = grid_grob,
          xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
        )
      })

    stamp <- c(stamp, grid_annotations)

  } else {
    # Original behavior for data coordinates
    if (axis == "x") {
      # Add vertical grid lines
      stamp <- c(
        stamp,
        list(
          ggplot2::annotate(
            "segment",
            x = breaks,
            xend = breaks,
            y = -Inf,
            yend = Inf,
            colour = grid_colour,
            linewidth = grid_linewidth,
            linetype = grid_linetype,
            ...
          )
        )
      )
    } else {  # y axis
      # Add horizontal grid lines
      stamp <- c(
        stamp,
        list(
          ggplot2::annotate(
            "segment",
            x = -Inf,
            xend = Inf,
            y = breaks,
            yend = breaks,
            colour = grid_colour,
            linewidth = grid_linewidth,
            linetype = grid_linetype,
            ...
          )
        )
      )
    }
  }

  return(stamp)
}

#' Annotate panel shade
#'
#' @description Create a subtle shaded rectangle to visually differentiate regions.
#'
#' It is designed to work with a theme that is globally set with [ggblanket::set_blanket] or [ggplot2::set_theme].
#'
#' @param ... Arguments passed to `ggplot2::annotate("rect", ....)` (if normalised coordinates not used). Require named arguments (and support trailing commas).
#' @param xmin A value of length 1. Defaults to `-Inf`. Use `I()` to specify normalized coordinates (0-1).
#' @param xmax A value of length 1. Defaults to `Inf`. Use `I()` to specify normalized coordinates (0-1).
#' @param ymin A value of length 1. Defaults to `-Inf`. Use `I()` to specify normalized coordinates (0-1).
#' @param ymax A value of length 1. Defaults to `Inf`. Use `I()` to specify normalized coordinates (0-1).
#' @param fill The fill colour of the rectangle. Defaults to a multiply/screen blended colour with `"#8991A1FF"` and the panel background fill.
#' @param alpha The transparency of the rectangle. Defaults to `0.2` (subtle overlay).
#' @param colour The border colour of the rectangle. Defaults to `"transparent"`.
#' @param linewidth The border linewidth of the rectangle. Inherits from the current theme `panel.border` linewidth. Supports `rel()` for relative sizing.
#' @param linetype The border linetype of the rectangle. Defaults to `1`.
#'
#' @return A list containing an annotation annotate.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' set_blanket()
#'
#' p <- palmerpenguins::penguins |>
#'   gg_blanket(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = species,
#'     border = TRUE,
#'   )
#'
#' # Using data coordinates with theme defaults
#' p +
#'   annotate_shade(
#'     xmin = 225,
#'   ) +
#'   geom_point()
#'
#' # Using normalized coordinates
#' p +
#'   annotate_shade(
#'     xmin = I(0.9),
#'   ) +
#'   geom_point()
#'
annotate_shade <- function(
    ...,
    xmin = -Inf,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf,
    fill = NULL,
    alpha = 0.2,
    colour = "transparent",
    linewidth = NULL,
    linetype = NULL
) {
  # Calculate adaptive fill if not specified
  if (rlang::is_null(fill)) {
    current_theme <- ggplot2::theme_get()
    panel_bg <- ggplot2::calc_element("panel.background", current_theme, skip_blank = TRUE)

    panel_bg_fill <- if (!rlang::is_null(panel_bg) && !inherits(panel_bg, "element_blank")) {
      panel_bg@fill %||% "#FFFFFFFF"
    } else {
      "#FFFFFFFF"
    }

    fill <- ifelse(is_col_dark(panel_bg_fill),
                   blend_screen("#8991A1FF", panel_bg_fill),
                   blend_multiply("#8991A1FF", panel_bg_fill))
  }

  # Check if coordinates are wrapped in I() for normalized positioning
  xmin_is_normalized <- inherits(xmin, "AsIs")
  xmax_is_normalized <- inherits(xmax, "AsIs")
  ymin_is_normalized <- inherits(ymin, "AsIs")
  ymax_is_normalized <- inherits(ymax, "AsIs")

  # Check for mixing of coordinate types
  x_uses_normalized <- xmin_is_normalized || xmax_is_normalized
  y_uses_normalized <- ymin_is_normalized || ymax_is_normalized

  # If using normalized, both min and max must be normalized or Inf
  if (x_uses_normalized) {
    if ((xmin_is_normalized || is.infinite(xmin)) && (xmax_is_normalized || is.infinite(xmax))) {
      # Valid - both are normalized or Inf
    } else {
      rlang::abort("Cannot mix normalized (I()) and data coordinates for x. Use I() for both xmin and xmax, or neither.")
    }
  }

  if (y_uses_normalized) {
    if ((ymin_is_normalized || is.infinite(ymin)) && (ymax_is_normalized || is.infinite(ymax))) {
      # Valid - both are normalized or Inf
    } else {
      rlang::abort("Cannot mix normalized (I()) and data coordinates for y. Use I() for both ymin and ymax, or neither.")
    }
  }

  # Unwrap and validate I() values
  if (xmin_is_normalized) {
    xmin <- unclass(xmin)
    if (length(xmin) != 1 || xmin < 0 || xmin > 1) {
      rlang::abort("Normalized xmin (specified with I()) must be a single value between 0 and 1")
    }
  }
  if (xmax_is_normalized) {
    xmax <- unclass(xmax)
    if (length(xmax) != 1 || xmax < 0 || xmax > 1) {
      rlang::abort("Normalized xmax (specified with I()) must be a single value between 0 and 1")
    }
  }
  if (ymin_is_normalized) {
    ymin <- unclass(ymin)
    if (length(ymin) != 1 || ymin < 0 || ymin > 1) {
      rlang::abort("Normalized ymin (specified with I()) must be a single value between 0 and 1")
    }
  }
  if (ymax_is_normalized) {
    ymax <- unclass(ymax)
    if (length(ymax) != 1 || ymax < 0 || ymax > 1) {
      rlang::abort("Normalized ymax (specified with I()) must be a single value between 0 and 1")
    }
  }

  # Determine if we need to use grob approach (if any coordinate is normalized)
  use_grob <- x_uses_normalized || y_uses_normalized

  # Get theme for linewidth default
  current_theme <- ggplot2::theme_get()
  panel_border <- ggplot2::calc_element("panel.border", current_theme, skip_blank = TRUE)
  panel_border_linewidth <- if (!rlang::is_null(panel_border) && !inherits(panel_border, "element_blank")) {
    panel_border$linewidth %||% 0.5
  } else {
    0.5  # fallback
  }

  # Set remaining defaults
  alpha <- alpha %||% 1

  # Handle linewidth with proper rel() support
  if (rlang::is_null(linewidth)) {
    linewidth <- panel_border_linewidth
  } else if (inherits(linewidth, "rel")) {
    linewidth <- as.numeric(linewidth) * panel_border_linewidth
  }

  linetype <- linetype %||% 1

  # Create rectangle based on coordinate type
  if (use_grob) {
    # For normalized coordinates, create a grob
    # Convert coordinates to npc units
    x_left <- if (xmin_is_normalized) {
      grid::unit(xmin, "npc")
    } else {
      grid::unit(0, "npc")  # -Inf defaults to 0
    }

    x_right <- if (xmax_is_normalized) {
      grid::unit(xmax, "npc")
    } else {
      grid::unit(1, "npc")  # Inf defaults to 1
    }

    y_bottom <- if (ymin_is_normalized) {
      grid::unit(ymin, "npc")
    } else {
      grid::unit(0, "npc")  # -Inf defaults to 0
    }

    y_top <- if (ymax_is_normalized) {
      grid::unit(ymax, "npc")
    } else {
      grid::unit(1, "npc")  # Inf defaults to 1
    }

    # Create rectangle grob
    rect_grob <- grid::rectGrob(
      x = x_left,
      y = y_bottom,
      width = x_right - x_left,
      height = y_top - y_bottom,
      just = c("left", "bottom"),
      gp = grid::gpar(
        fill = scales::alpha(fill, alpha),
        col = colour,
        lwd = linewidth * 72 / 25.4,
        lty = linetype
      )
    )

    stamp <- list(
      ggplot2::annotation_custom(
        grob = rect_grob,
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
      )
    )
  } else {
    # Original behavior for data coordinates
    stamp <- list(
      ggplot2::annotate(
        geom = "rect",
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        fill = fill,
        colour = colour,
        linewidth = linewidth,
        linetype = linetype,
        alpha = alpha,
        ...
      )
    )
  }

  return(stamp)
}


