#' Annotate axis line segment
#'
#' @description Create an annotated segment of the axis line.
#'
#' This function is designed to work with a theme that is globally set with [ggblanket::set_blanket] or [ggplot2::set_theme].
#'
#' It should be used with a `coord` of `ggplot2::coord_cartesian(clip = "off")`.
#'
#' @param ... Arguments passed to `ggplot2::annotate("segment", ....)` (if normalised coordinates not used). Require named arguments (and support trailing commas).
#' @param position The position of the axis line. One of "top", "bottom", "left", or "right". Ignored if x or y is provided.
#' @param x A single x-axis value for a vertical line. Cannot be used together with y or xmin/xmax. Use I() for normalized coordinates (0-1).
#' @param y A single y-axis value for a horizontal line. Cannot be used together with x or ymin/ymax. Use I() for normalized coordinates (0-1).
#' @param xmin The starting x position for a horizontal line segment. Use I() for normalized coordinates (0-1).
#' @param xmax The ending x position for a horizontal line segment. Use I() for normalized coordinates (0-1).
#' @param ymin The starting y position for a vertical line segment. Use I() for normalized coordinates (0-1).
#' @param ymax The ending y position for a vertical line segment. Use I() for normalized coordinates (0-1).
#' @param colour The colour of the annotated segment. Inherits from the current theme axis.line etc.
#' @param linewidth The linewidth of the annotated segment. Inherits from the current theme axis.line etc.
#' @param theme How to modify the corresponding theme element. One of "keep", "transparent", or "blank".
#'   Defaults to "transparent" if position specified, otherwise "keep".
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
    theme = NULL
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

  if (rlang::is_null(theme)) {
    theme <- if (!rlang::is_null(position)) "transparent" else "keep"
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
