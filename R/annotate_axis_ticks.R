#' Annotated axis ticks segments
#'
#' @description Replace axis ticks with annotated segments using absolute measurements.
#' This function only works when panel dimensions are set uniformly via panel.widths and panel.heights.
#' It requires a `coord` of `ggplot2::coord_cartesian(clip = "off")`.
#'
#' @param ... Provided to require argument naming, support trailing commas etc.
#' @param x_position,y_position The position of the axis ticks. One of `"bottom"`/`"top"` or `"left"`/`"right"`.
#' @param x_breaks,y_breaks A vector of axis breaks for axis ticks.
#' @param colour The colour of the annotated segment. Inherits from the current theme axis.ticks etc.
#' @param linewidth The linewidth of the annotated segment. Inherits from the current theme axis.ticks etc.
#' @param length The absolute length of the annotated segment as a grid unit. Defaults to unit(11/3, "pt").
#' @param theme_elements What to do with theme axis tick elements. Either "transparent", "keep" or "blank". Defaults "transparent".
#'
#' @return A list of a annotate layer and theme elements.
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
#' penguins |>
#'   tidyr::drop_na(sex) |>
#'   mutate(across(sex, \(x) str_to_sentence(x))) |>
#'   gg_histogram(
#'     x = flipper_length_mm,
#'     col = sex,
#'   ) +
#'   annotate_axis_ticks(x_breaks = c(185, 195), x_position = "bottom")
#'
#' penguins |>
#'   tidyr::drop_na(sex) |>
#'   mutate(across(sex, \(x) str_to_sentence(x))) |>
#'   gg_blanket(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = sex,
#'   ) +
#'   annotate_axis_ticks(
#'     x_breaks = c(185, 195, 205, 215, 225),
#'     y_breaks = c(3500, 4500, 5500),
#'     x_position = "bottom",
#'     y_position = "left"
#'   ) +
#'   geom_point()
#'
annotate_axis_ticks <- function(
    ...,
    x_breaks = NULL,
    y_breaks = NULL,
    x_position = NULL,
    y_position = NULL,
    colour = NULL,
    linewidth = NULL,
    length = NULL,
    theme_elements = "transparent"
) {
  rlang::inform(
    "Please use this function with ggplot2::coord_cartesian(clip = 'off')"
  )

  # Check if panel dimensions are set
  current_theme <- ggplot2::get_theme()
  panel_widths <- current_theme$panel.widths
  panel_heights <- current_theme$panel.heights

  if (is.null(panel_widths) && is.null(panel_heights)) {
    rlang::abort(
      "This function only works when panel dimensions are explicitly set via theme(panel.widths = ..., panel.heights = ...)"
    )
  }

  # Return early if no breaks provided
  if (is.null(x_breaks) && is.null(y_breaks)) {
    return(list())
  }

  # Set default positions if not provided
  if (!is.null(x_breaks) && is.null(x_position)) {
    x_position <- "bottom"
  }
  if (!is.null(y_breaks) && is.null(y_position)) {
    y_position <- "left"
  }

  # Validate panel dimensions
  if (!is.null(x_breaks)) {
    if (is.null(panel_heights)) {
      rlang::abort(
        "panel.heights must be set in theme for x-axis tick annotation"
      )
    }
    if (length(panel_heights) > 1 && length(unique(as.numeric(panel_heights))) > 1) {
      rlang::abort(
        "Different panel heights set. This function only works with uniform panel dimensions."
      )
    }
    if (!x_position %in% c("bottom", "top")) {
      rlang::abort("x_position must be one of 'bottom' or 'top'")
    }
  }

  if (!is.null(y_breaks)) {
    if (is.null(panel_widths)) {
      rlang::abort(
        "panel.widths must be set in theme for y-axis tick annotation"
      )
    }
    if (length(panel_widths) > 1 && length(unique(as.numeric(panel_widths))) > 1) {
      rlang::abort(
        "Different panel widths set. This function only works with uniform panel dimensions."
      )
    }
    if (!y_position %in% c("left", "right")) {
      rlang::abort("y_position must be one of 'left' or 'right'")
    }
  }

  stamp <- list()

  # Process x-axis ticks
  if (!is.null(x_breaks)) {
    # Extract theme properties for x-axis ticks
    if (rlang::is_null(length)) {
      length <- if (x_position == "bottom") {
        current_theme$axis.ticks.length.x.bottom %||%
          current_theme$axis.ticks.length.x %||%
          current_theme$axis.ticks.length %||%
          grid::unit(11 / 3, "pt")
      } else {
        current_theme$axis.ticks.length.x.top %||%
          current_theme$axis.ticks.length.x %||%
          current_theme$axis.ticks.length %||%
          grid::unit(11 / 3, "pt")
      }
    }

    if (rlang::is_null(colour)) {
      colour <- if (x_position == "bottom") {
        current_theme$axis.ticks.x.bottom$colour %||%
          current_theme$axis.ticks.x$colour %||%
          current_theme$axis.ticks$colour %||%
          "#121B24FF"
      } else {
        current_theme$axis.ticks.x.top$colour %||%
          current_theme$axis.ticks.x$colour %||%
          current_theme$axis.ticks$colour %||%
          "#121B24FF"
      }
    }

    if (rlang::is_null(linewidth)) {
      linewidth <- if (x_position == "bottom") {
        current_theme$axis.ticks.x.bottom$linewidth %||%
          current_theme$axis.ticks.x$linewidth %||%
          current_theme$axis.ticks$linewidth %||%
          0.5
      } else {
        current_theme$axis.ticks.x.top$linewidth %||%
          current_theme$axis.ticks.x$linewidth %||%
          current_theme$axis.ticks$linewidth %||%
          0.5
      }
    }

    # Add theme modifications for x-axis
    if (theme_elements == "transparent") {
      stamp <- c(stamp, list(
        ggplot2::theme(
          axis.ticks.x.bottom = ggplot2::element_line(colour = "transparent"),
          axis.ticks.x.top = ggplot2::element_line(colour = "transparent")
        )
      ))
    } else if (theme_elements == "blank") {
      stamp <- c(stamp, list(
        ggplot2::theme(
          axis.ticks.x.bottom = ggplot2::element_blank(),
          axis.ticks.x.top = ggplot2::element_blank()
        )
      ))
    }

    # Create x-axis tick annotations
    for (break_val in x_breaks) {
      tick_grob <- if (x_position == "bottom") {
        grid::segmentsGrob(
          x0 = grid::unit(0.5, "npc"),
          x1 = grid::unit(0.5, "npc"),
          y0 = grid::unit(0, "npc"),
          y1 = grid::unit(0, "npc") - length,
          gp = grid::gpar(
            col = colour,
            lwd = linewidth * 72 / 25.4,
            lineend = "butt"
          )
        )
      } else {
        grid::segmentsGrob(
          x0 = grid::unit(0.5, "npc"),
          x1 = grid::unit(0.5, "npc"),
          y0 = grid::unit(1, "npc"),
          y1 = grid::unit(1, "npc") + length,
          gp = grid::gpar(
            col = colour,
            lwd = linewidth * 72 / 25.4,
            lineend = "butt"
          )
        )
      }

      annotation_position <- if (x_position == "bottom") {
        list(xmin = break_val, xmax = break_val, ymin = -Inf, ymax = -Inf)
      } else {
        list(xmin = break_val, xmax = break_val, ymin = Inf, ymax = Inf)
      }

      stamp <- c(
        stamp,
        list(
          do.call(
            ggplot2::annotation_custom,
            c(list(grob = tick_grob), annotation_position)
          )
        )
      )
    }
  }

  # Process y-axis ticks
  if (!is.null(y_breaks)) {
    # Extract theme properties for y-axis ticks
    if (rlang::is_null(length)) {
      length <- if (y_position == "left") {
        current_theme$axis.ticks.length.y.left %||%
          current_theme$axis.ticks.length.y %||%
          current_theme$axis.ticks.length %||%
          grid::unit(11 / 3, "pt")
      } else {
        current_theme$axis.ticks.length.y.right %||%
          current_theme$axis.ticks.length.y %||%
          current_theme$axis.ticks.length %||%
          grid::unit(11 / 3, "pt")
      }
    }

    if (rlang::is_null(colour)) {
      colour <- if (y_position == "left") {
        current_theme$axis.ticks.y.left$colour %||%
          current_theme$axis.ticks.y$colour %||%
          current_theme$axis.ticks$colour %||%
          "#121B24FF"
      } else {
        current_theme$axis.ticks.y.right$colour %||%
          current_theme$axis.ticks.y$colour %||%
          current_theme$axis.ticks$colour %||%
          "#121B24FF"
      }
    }

    if (rlang::is_null(linewidth)) {
      linewidth <- if (y_position == "left") {
        current_theme$axis.ticks.y.left$linewidth %||%
          current_theme$axis.ticks.y$linewidth %||%
          current_theme$axis.ticks$linewidth %||%
          0.5
      } else {
        current_theme$axis.ticks.y.right$linewidth %||%
          current_theme$axis.ticks.y$linewidth %||%
          current_theme$axis.ticks$linewidth %||%
          0.5
      }
    }

    # Add theme modifications for y-axis
    if (theme_elements == "transparent") {
      stamp <- c(stamp, list(
        ggplot2::theme(
          axis.ticks.y.left = ggplot2::element_line(colour = "transparent"),
          axis.ticks.y.right = ggplot2::element_line(colour = "transparent")
        )
      ))
    } else if (theme_elements == "blank") {
      stamp <- c(stamp, list(
        ggplot2::theme(
          axis.ticks.y.left = ggplot2::element_blank(),
          axis.ticks.y.right = ggplot2::element_blank()
        )
      ))
    }

    # Create y-axis tick annotations
    for (break_val in y_breaks) {
      tick_grob <- if (y_position == "left") {
        grid::segmentsGrob(
          x0 = grid::unit(0, "npc"),
          x1 = grid::unit(0, "npc") - length,
          y0 = grid::unit(0.5, "npc"),
          y1 = grid::unit(0.5, "npc"),
          gp = grid::gpar(
            col = colour,
            lwd = linewidth * 72 / 25.4,
            lineend = "butt"
          )
        )
      } else {
        grid::segmentsGrob(
          x0 = grid::unit(1, "npc"),
          x1 = grid::unit(1, "npc") + length,
          y0 = grid::unit(0.5, "npc"),
          y1 = grid::unit(0.5, "npc"),
          gp = grid::gpar(
            col = colour,
            lwd = linewidth * 72 / 25.4,
            lineend = "butt"
          )
        )
      }

      annotation_position <- if (y_position == "left") {
        list(xmin = -Inf, xmax = -Inf, ymin = break_val, ymax = break_val)
      } else {
        list(xmin = Inf, xmax = Inf, ymin = break_val, ymax = break_val)
      }

      stamp <- c(
        stamp,
        list(
          do.call(
            ggplot2::annotation_custom,
            c(list(grob = tick_grob), annotation_position)
          )
        )
      )
    }
  }

  return(stamp)
}
