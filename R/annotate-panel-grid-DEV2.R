#' Annotate panel grid segments
#'
#' @description Create annotated segments of the panel grid.
#'
#' This function is designed to work with a theme that is globally set with [ggblanket::set_blanket] or [ggplot2::set_theme].
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param x A vector of x-axis breaks for vertical grid lines. Cannot be used together with y. Use I() to specify normalized coordinates (0-1).
#' @param y A vector of y-axis breaks for horizontal grid lines. Cannot be used together with x. Use I() to specify normalized coordinates (0-1).
#' @param minor Logical. If FALSE (default), creates major grid lines. If TRUE, creates minor grid lines.
#' @param colour The colour of grid lines. Inherits from current theme panel.grid.major or panel.grid.minor etc.
#' @param linewidth The linewidth of grid lines. Inherits from current theme panel.grid.major or panel.grid.minor etc.
#' @param linetype The linetype of grid lines. Inherits from current theme panel.grid.major or panel.grid.minor etc.
#' @param theme_element What to do with the equivalent theme elements. Either "keep" , "transparent", or "blank". Defaults "keep".
#'
#' @return A list of annotate layers and theme elements.
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
    theme_element = "keep"
) {
  # Validate arguments
  if (rlang::is_null(x) && rlang::is_null(y)) {
    rlang::abort("Either x or y must be specified")
  }

  if (!rlang::is_null(x) && !rlang::is_null(y)) {
    rlang::abort("Only one of x or y can be specified")
  }

  if (!theme_element %in% c("transparent", "keep", "blank")) {
    rlang::abort(
      "theme_element must be one of 'transparent', 'keep', or 'blank'"
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
  if (theme_element != "keep") {
    # Determine which theme element to modify based on minor flag
    if (minor) {
      element_name <- paste0("panel.grid.minor.", axis)
    } else {
      element_name <- paste0("panel.grid.major.", axis)
    }

    if (theme_element == "transparent") {
      stamp <- c(
        stamp,
        list(
          ggplot2::theme(
            !!element_name := ggplot2::element_line(colour = "transparent")
          )
        )
      )
    } else if (theme_element == "blank") {
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
            linetype = grid_linetype
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
            linetype = grid_linetype
          )
        )
      )
    }
  }

  return(stamp)
}
