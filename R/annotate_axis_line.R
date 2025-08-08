#' Annotate axis line segment
#'
#' @description Create an axis line with an annotated segment.
#'
#' This function is designed to work with a theme that is globally set, so that the annotated line can be made consistent by default.
#'
#' It should be used with a `coord` of `ggplot2::coord_cartesian(clip = "off")`.
#'
#' @param ... Arguments passed to `ggplot2::annotate("segment", ....)`. Require named arguments (and support trailing commas).
#' @param position The position of the axis line. One of "top", "bottom", "left", or "right".
#' @param colour The colour of the annotated segment. Inherits from the current theme axis.line etc.
#' @param linewidth The linewidth of the annotated segment. Inherits from the current theme axis.line etc.
#' @param theme_element What to do with the equivalent theme element. Either "transparent", "keep" or "blank". Defaults "transparent".
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
#'     colour = col_multiply(get_geom_defaults("point")$colour),
#'     size = 3,
#'   )
#'
annotate_axis_line <- function(
    ...,
    position,
    colour = NULL,
    linewidth = NULL,
    theme_element = "transparent"
) {
  # Validate arguments
  if (!position %in% c("top", "bottom", "left", "right")) {
    rlang::abort("position must be one of 'top', 'bottom', 'left', or 'right'")
  }

  if (!theme_element %in% c("transparent", "keep", "blank")) {
    rlang::abort(
      "theme_element must be one of 'transparent', 'keep', or 'blank'"
    )
  }

  # Determine axis from position
  axis <- if (position %in% c("top", "bottom")) "x" else "y"

  # Get current theme and calculate resolved element properties
  current_theme <- ggplot2::theme_get()

  # Build hierarchy of element names from most specific to least specific
  specific_element <- paste0("axis.line.", axis, ".", position)
  axis_element <- paste0("axis.line.", axis)
  general_element <- "axis.line"

  element_hierarchy <- c(specific_element, axis_element, general_element)

  # Find the first non-blank resolved element
  resolved_element <- element_hierarchy |>
    purrr::map(\(x) ggplot2::calc_element(x, current_theme, skip_blank = TRUE)) |>
    purrr::detect(\(x) !is.null(x) && !inherits(x, "element_blank"))

  # If still no element found, create a minimal fallback
  if (is.null(resolved_element)) {
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

  # Create axis segment based on position
  if (position == "bottom") {
    stamp <- c(
      stamp,
      list(
        rlang::exec(
          ggplot2::annotate,
          "segment",
          x = I(-Inf),
          xend = I(Inf),
          y = I(-Inf),
          yend = I(-Inf),
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
          x = I(-Inf),
          xend = I(Inf),
          y = I(Inf),
          yend = I(Inf),
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
          x = I(-Inf),
          xend = I(-Inf),
          y = I(-Inf),
          yend = I(Inf),
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
          x = I(Inf),
          xend = I(Inf),
          y = I(-Inf),
          yend = I(Inf),
          colour = line_colour,
          linewidth = line_linewidth,
          ...
        )
      )
    )
  }

  # Add theme modification if requested
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

  return(stamp)
}
