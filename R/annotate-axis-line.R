#' Annotate axis line segment
#'
#' @description Create an annotated segment of the axis line.
#'
#' This function is designed to work with a theme that is globally set with [ggblanket::set_blanket] or [ggplot2::set_theme].
#'
#' It should be used with a `coord` of `ggplot2::coord_cartesian(clip = "off")`.
#'
#' @param ... Arguments passed to `ggplot2::annotate("segment", ....)`. Require named arguments (and support trailing commas).
#' @param position The position of the axis line. One of "top", "bottom", "left", or "right". Ignored if x or y is provided.
#' @param x A single x-axis value for a vertical line. Cannot be used together with y or position. Use I() for normalized coordinates (0-1).
#' @param y A single y-axis value for a horizontal line. Cannot be used together with x or position. Use I() for normalized coordinates (0-1).
#' @param colour The colour of the annotated segment. Inherits from the current theme axis.line etc.
#' @param linewidth The linewidth of the annotated segment. Inherits from the current theme axis.line etc.
#' @param theme_element What to do with the equivalent theme elements. Either "keep" , "transparent", or "blank". Defaults "transparent" if position specified. Otherwise defaults "keep".
#'
#' @return A list of a annotate layer and theme elements.
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
#' palmerpenguins::penguins |>
#'   tibble::add_row(flipper_length_mm = 195, body_mass_g = 2500, sex = "Female") |>
#'   gg_blanket(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'   ) +
#'   annotate_axis_line(
#'     position = "bottom",
#'     arrow = arrow(angle = 15, length = unit(5, "pt"), type = "closed"),
#'   ) +
#'   annotate_axis_line(
#'     position = "left",
#'     arrow = arrow(angle = 15, length = unit(5, "pt"), type = "closed"),
#'   ) +
#'   geom_point(
#'     colour = blend_multiply(get_geom_defaults("point")$colour),
#'     size = 3,
#'   )
#'
annotate_axis_line <- function(
    ...,
    position = NULL,
    x = NULL,
    y = NULL,
    colour = NULL,
    linewidth = NULL,
    theme_element = NULL
) {
  # Validate arguments - can't have both x and y
  if (!rlang::is_null(x) && !rlang::is_null(y)) {
    rlang::abort("Cannot specify both x and y. Use either x for a vertical line or y for a horizontal line.")
  }

  if (rlang::is_null(theme_element)) {
    theme_element <- if (!rlang::is_null(position)) "transparent" else "keep"
  }

  # If x or y is provided, it overrides position
  use_xy_positioning <- !rlang::is_null(x) || !rlang::is_null(y)

  if (use_xy_positioning) {
    # Check if using normalized coordinates
    x_is_normalized <- !rlang::is_null(x) && inherits(x, "AsIs")
    y_is_normalized <- !rlang::is_null(y) && inherits(y, "AsIs")

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

    # Determine axis from x/y
    axis <- if (!rlang::is_null(x)) "y" else "x"  # Note: vertical line is on y axis, horizontal on x axis
    use_normalized <- x_is_normalized || y_is_normalized
  } else {
    # Original position-based behavior
    if (rlang::is_null(position)) {
      rlang::abort("Must specify either position, x, or y")
    }

    if (!position %in% c("top", "bottom", "left", "right")) {
      rlang::abort("position must be one of 'top', 'bottom', 'left', or 'right'")
    }

    # Determine axis from position
    axis <- if (position %in% c("top", "bottom")) "x" else "y"
    use_normalized <- FALSE
  }

  if (!theme_element %in% c("transparent", "keep", "blank")) {
    rlang::abort(
      "theme_element must be one of 'transparent', 'keep', or 'blank'"
    )
  }

  # Get current theme and calculate resolved element properties
  current_theme <- ggplot2::theme_get()

  # Build hierarchy of element names from most specific to least specific
  # When using x/y positioning, we don't have a specific position, so just use axis
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
    resolved_element$colour %||% "black"
  } else {
    colour
  }

  # Handle linewidth with proper rel() support
  if (rlang::is_null(linewidth)) {
    line_linewidth <- resolved_element$linewidth %||% 0.5
  } else {
    if (inherits(linewidth, "rel")) {
      # Apply user's rel() to the resolved theme linewidth
      base_linewidth <- resolved_element$linewidth %||% 0.5
      line_linewidth <- as.numeric(linewidth) * base_linewidth
    } else {
      line_linewidth <- linewidth
    }
  }

  stamp <- list()

  # Create axis segment based on positioning method
  if (use_xy_positioning) {
    if (use_normalized) {
      # For normalized coordinates, use annotation_custom with grob
      if (!rlang::is_null(x)) {
        # Vertical line at normalized x position
        line_grob <- grid::linesGrob(
          x = grid::unit(c(x, x), "npc"),
          y = grid::unit(c(0, 1), "npc"),
          gp = grid::gpar(
            col = line_colour,
            lwd = line_linewidth * 72 / 25.4
          )
        )
      } else {
        # Horizontal line at normalized y position
        line_grob <- grid::linesGrob(
          x = grid::unit(c(0, 1), "npc"),
          y = grid::unit(c(y, y), "npc"),
          gp = grid::gpar(
            col = line_colour,
            lwd = line_linewidth * 72 / 25.4
          )
        )
      }

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
      if (!rlang::is_null(x)) {
        # Vertical line at x position
        stamp <- c(
          stamp,
          list(
            rlang::exec(
              ggplot2::annotate,
              "segment",
              x = x,
              xend = x,
              y = -Inf,
              yend = Inf,
              colour = line_colour,
              linewidth = line_linewidth,
              ...
            )
          )
        )
      } else {
        # Horizontal line at y position
        stamp <- c(
          stamp,
          list(
            rlang::exec(
              ggplot2::annotate,
              "segment",
              x = -Inf,
              xend = Inf,
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
  if (!use_xy_positioning && theme_element != "keep") {
    if (theme_element == "transparent") {
      theme_element_name <- paste0("axis.line.", axis, ".", position)
      theme_mod <- list()
      theme_mod[[theme_element_name]] <- ggplot2::element_line(colour = "transparent")
      stamp <- c(stamp, list(rlang::exec(ggplot2::theme, !!!theme_mod)))
    } else if (theme_element == "blank") {
      theme_element_name <- paste0("axis.line.", axis, ".", position)
      theme_mod <- list()
      theme_mod[[theme_element_name]] <- ggplot2::element_blank()
      stamp <- c(stamp, list(rlang::exec(ggplot2::theme, !!!theme_mod)))
    }
  }

  return(stamp)
}
