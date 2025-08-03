#' Annotated axis ticks segments
#'
#' @description Replace axis ticks with annotated segments using absolute measurements.
#' This function only works when panel dimensions are set uniformly via panel.widths and panel.heights.
#' It requires a `coord` of `ggplot2::coord_cartesian(clip = "off")`.
#'
#' @param axis The axis to annotate. One of "x" or "y".
#' @param ... Provided to require argument naming, support trailing commas etc.
#' @param breaks A vector of axis breaks for axis ticks.
#' @param position The position of the axis ticks. For x-axis: "bottom" or "top". For y-axis: "left" or "right". Defaults to "bottom" for x-axis and "left" for y-axis.
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
#'   annotate_axis_ticks(axis = "x", breaks = c(185, 195), position = "bottom")
#'
#' penguins |>
#'   tidyr::drop_na(sex) |>
#'   mutate(across(sex, \(x) str_to_sentence(x))) |>
#'   gg_blanket(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = sex,
#'   ) +
#'   annotate_axis_ticks(axis = "x", breaks = c(185, 195, 205, 215, 225)) +
#'   annotate_axis_ticks(axis = "y", breaks = c(3500, 4500, 5500)) +
#'   geom_point()
#'
annotate_axis_ticks <- function(
    axis,
    ...,
    breaks,
    position = NULL,
    colour = NULL,
    linewidth = NULL,
    length = NULL,
    theme_elements = "transparent"
) {
  rlang::inform(
    "Please use this function with ggplot2::coord_cartesian(clip = 'off')"
  )

  # Validate arguments
  if (!axis %in% c("x", "y")) {
    rlang::abort("axis must be one of 'x' or 'y'")
  }

  # Set default position if not provided
  if (is.null(position)) {
    position <- if (axis == "x") "bottom" else "left"
  }

  if (axis == "x" && !position %in% c("bottom", "top")) {
    rlang::abort("For x-axis, position must be one of 'bottom' or 'top'")
  }

  if (axis == "y" && !position %in% c("left", "right")) {
    rlang::abort("For y-axis, position must be one of 'left' or 'right'")
  }

  if (!theme_elements %in% c("transparent", "keep", "blank")) {
    rlang::abort("theme_elements must be one of 'transparent', 'keep', or 'blank'")
  }

  # Check if panel dimensions are set
  current_theme <- ggplot2::get_theme()
  panel_widths <- current_theme$panel.widths
  panel_heights <- current_theme$panel.heights

  if (is.null(panel_widths) && is.null(panel_heights)) {
    rlang::abort(
      "This function only works when panel dimensions are explicitly set via theme(panel.widths = ..., panel.heights = ...)"
    )
  }

  # Validate panel dimensions for the specific axis
  if (axis == "x") {
    if (is.null(panel_heights)) {
      rlang::abort("panel.heights must be set in theme for x-axis tick annotation")
    }
    if (length(panel_heights) > 1 && length(unique(as.numeric(panel_heights))) > 1) {
      rlang::abort("Different panel heights set. This function only works with uniform panel dimensions.")
    }
  } else {
    if (is.null(panel_widths)) {
      rlang::abort("panel.widths must be set in theme for y-axis tick annotation")
    }
    if (length(panel_widths) > 1 && length(unique(as.numeric(panel_widths))) > 1) {
      rlang::abort("Different panel widths set. This function only works with uniform panel dimensions.")
    }
  }

  # Helper function to extract theme properties
  extract_theme_property <- function(property, default) {
    if (axis == "x") {
      current_theme[[paste0("axis.ticks.x.", position)]][[property]] %||%
        current_theme[["axis.ticks.x"]][[property]] %||%
        current_theme[["axis.ticks"]][[property]] %||%
        default
    } else {
      current_theme[[paste0("axis.ticks.y.", position)]][[property]] %||%
        current_theme[["axis.ticks.y"]][[property]] %||%
        current_theme[["axis.ticks"]][[property]] %||%
        default
    }
  }

  # Extract theme properties
  if (rlang::is_null(length)) {
    length <- if (axis == "x") {
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
  }

  tick_colour <- if (rlang::is_null(colour)) {
    extract_theme_property("colour", "#121B24FF")
  } else {
    colour
  }

  tick_linewidth <- if (rlang::is_null(linewidth)) {
    extract_theme_property("linewidth", 0.5)
  } else {
    linewidth
  }

  stamp <- list()

  # Add theme modification if requested
  if (theme_elements == "transparent") {
    theme_element <- paste0("axis.ticks.", axis, ".", position)
    theme_mod <- list()
    theme_mod[[theme_element]] <- ggplot2::element_line(colour = "transparent")
    stamp <- c(stamp, list(do.call(ggplot2::theme, theme_mod)))
  } else if (theme_elements == "blank") {
    theme_element <- paste0("axis.ticks.", axis, ".", position)
    theme_mod <- list()
    theme_mod[[theme_element]] <- ggplot2::element_blank()
    stamp <- c(stamp, list(do.call(ggplot2::theme, theme_mod)))
  }

  # Create tick annotations
  for (break_val in breaks) {
    if (axis == "x") {
      tick_grob <- if (position == "bottom") {
        grid::segmentsGrob(
          x0 = grid::unit(0.5, "npc"),
          x1 = grid::unit(0.5, "npc"),
          y0 = grid::unit(0, "npc"),
          y1 = grid::unit(0, "npc") - length,
          gp = grid::gpar(
            col = tick_colour,
            lwd = tick_linewidth * 72 / 25.4,
            lineend = "butt"
          )
        )
      } else { # top
        grid::segmentsGrob(
          x0 = grid::unit(0.5, "npc"),
          x1 = grid::unit(0.5, "npc"),
          y0 = grid::unit(1, "npc"),
          y1 = grid::unit(1, "npc") + length,
          gp = grid::gpar(
            col = tick_colour,
            lwd = tick_linewidth * 72 / 25.4,
            lineend = "butt"
          )
        )
      }

      annotation_position <- if (position == "bottom") {
        list(xmin = break_val, xmax = break_val, ymin = -Inf, ymax = -Inf)
      } else {
        list(xmin = break_val, xmax = break_val, ymin = Inf, ymax = Inf)
      }
    } else { # y-axis
      tick_grob <- if (position == "left") {
        grid::segmentsGrob(
          x0 = grid::unit(0, "npc"),
          x1 = grid::unit(0, "npc") - length,
          y0 = grid::unit(0.5, "npc"),
          y1 = grid::unit(0.5, "npc"),
          gp = grid::gpar(
            col = tick_colour,
            lwd = tick_linewidth * 72 / 25.4,
            lineend = "butt"
          )
        )
      } else { # right
        grid::segmentsGrob(
          x0 = grid::unit(1, "npc"),
          x1 = grid::unit(1, "npc") + length,
          y0 = grid::unit(0.5, "npc"),
          y1 = grid::unit(0.5, "npc"),
          gp = grid::gpar(
            col = tick_colour,
            lwd = tick_linewidth * 72 / 25.4,
            lineend = "butt"
          )
        )
      }

      annotation_position <- if (position == "left") {
        list(xmin = -Inf, xmax = -Inf, ymin = break_val, ymax = break_val)
      } else {
        list(xmin = Inf, xmax = Inf, ymin = break_val, ymax = break_val)
      }
    }

    stamp <- c(
      stamp,
      list(
        rlang::exec(
          ggplot2::annotation_custom,
          grob = tick_grob,
          !!!annotation_position
        )
      )
    )
  }

  return(stamp)
}
