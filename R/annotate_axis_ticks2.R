#' #' Annotated axis ticks segments
#' #'
#' #' @description Replace axis ticks with annotated segments using absolute measurements.
#' #' This function only works when panel dimensions are set uniformly via panel.widths and panel.heights.
#' #' It requires a `coord` of `ggplot2::coord_cartesian(clip = "off")`.
#' #'
#' #' @param ... Provided to require argument naming, support trailing commas etc.
#' #' @param x_breaks A vector of x-axis breaks for bottom/top axis ticks. If NULL, no x-axis ticks are drawn.
#' #' @param y_breaks A vector of y-axis breaks for left/right axis ticks. If NULL, no y-axis ticks are drawn.
#' #' @param position The position of the axis. One of "bottom", "top", "left" or "right".
#' #' @param colour The colour of the annotated segment. Inherits from the current theme axis.ticks etc.
#' #' @param linewidth The linewidth of the annotated segment. Inherits from the current theme axis.ticks etc.
#' #' @param length The absolute length of the annotated segment as a grid unit. Defaults to unit(11/3, "pt").
#' #' @param theme_elements What to do with theme axis tick elements. Either "transparent", "keep" or "blank". Defaults "transparent".
#' #'
#' #' @return A list of a annotate layer and theme elements.
#' #' @export
#' #'
#' #' @examples
#' #' library(ggplot2)
#' #' library(dplyr)
#' #' library(stringr)
#' #' library(palmerpenguins)
#' #'
#' #' set_blanket(
#' #'   theme = theme_lighter(
#' #'     panel_heights = rep(unit(50, "mm"), 100),
#' #'     panel_widths = rep(unit(75, "mm"), 100),
#' #'   ),
#' #' )
#' #'
#' #' penguins |>
#' #'   tidyr::drop_na(sex) |>
#' #'   mutate(across(sex, \(x) str_to_sentence(x))) |>
#' #'   gg_histogram(
#' #'     x = flipper_length_mm,
#' #'     col = sex,
#' #'   ) +
#' #'   annotate_axis_ticks(x_breaks = c(185, 195))
#' #'
#' #' penguins |>
#' #'   tidyr::drop_na(sex) |>
#' #'   mutate(across(sex, \(x) str_to_sentence(x))) |>
#' #'   gg_bar(
#' #'     x = island,
#' #'     col = sex,
#' #'     width = 0.75,
#' #'   ) +
#' #'   annotate_axis_ticks(x_breaks = c(0.5, 1.5, 2.5, 3.5))
#' #'
#' annotate_axis_ticks <- function(
#'     ...,
#'     x_breaks = NULL,
#'     y_breaks = NULL,
#'     position = NULL,
#'     colour = NULL,
#'     linewidth = NULL,
#'     length = NULL,
#'     theme_elements = "transparent"
#' ) {
#'   rlang::inform(
#'     "Please use this function with ggplot2::coord_cartesian(clip = 'off')"
#'   )
#'
#'   # Check if panel dimensions are set
#'   current_theme <- ggplot2::get_theme()
#'   panel_widths <- current_theme$panel.widths
#'   panel_heights <- current_theme$panel.heights
#'
#'   if (is.null(panel_widths) && is.null(panel_heights)) {
#'     rlang::abort(
#'       "This function only works when panel dimensions are explicitly set via theme(panel.widths = ..., panel.heights = ...)"
#'     )
#'   }
#'
#'   if (!rlang::is_null(x_breaks)) breaks <- x_breaks
#'   else if (!rlang::is_null(y_breaks)) breaks <- y_breaks
#'
#'   if (rlang::is_null(position)) {
#'     if (!rlang::is_null(x_breaks)) position <- "bottom"
#'     else if (!rlang::is_null(y_breaks)) position <- "left"
#'   }
#'
#'   # Return early if no breaks provided for this axis
#'   if (is.null(breaks)) {
#'     return(list())
#'   }
#'
#'   # Set default length from current theme
#'   if (rlang::is_null(length)) {
#'     if (!rlang::is_null(x_breaks)) {
#'       length <- if (position == "bottom") {
#'         current_theme$axis.ticks.length.x.bottom %||%
#'           current_theme$axis.ticks.length.x %||%
#'           current_theme$axis.ticks.length %||%
#'           grid::unit(11 / 3, "pt")
#'       } else {
#'         current_theme$axis.ticks.length.x.top %||%
#'           current_theme$axis.ticks.length.x %||%
#'           current_theme$axis.ticks.length %||%
#'           grid::unit(11 / 3, "pt")
#'       }
#'     }
#'     else if (!rlang::is_null(y_breaks)) {
#'       length <- if (position == "left") {
#'         current_theme$axis.ticks.length.y.left %||%
#'           current_theme$axis.ticks.length.y %||%
#'           current_theme$axis.ticks.length %||%
#'           grid::unit(11 / 3, "pt")
#'       } else {
#'         current_theme$axis.ticks.length.y.right %||%
#'           current_theme$axis.ticks.length.y %||%
#'           current_theme$axis.ticks.length %||%
#'           grid::unit(11 / 3, "pt")
#'       }
#'     }
#'   }
#'
#'   # Get the relevant panel dimension
#'   if (!rlang::is_null(x_breaks)) {
#'     if (is.null(panel_heights)) {
#'       rlang::abort(
#'         "panel.heights must be set in theme for horizontal axis tick annotation"
#'       )
#'     }
#'     if (
#'       length(panel_heights) > 1 && length(unique(as.numeric(panel_heights))) > 1
#'     ) {
#'       rlang::abort(
#'         "Different panel heights set. This function only works with uniform panel dimensions."
#'       )
#'     }
#'     panel_dimension <- panel_heights[1] # Use first panel height
#'   }
#'   else if (!rlang::is_null(y_breaks)){
#'     if (is.null(panel_widths)) {
#'       rlang::abort(
#'         "panel.widths must be set in theme for vertical axis tick annotation"
#'       )
#'     }
#'     if (
#'       length(panel_widths) > 1 && length(unique(as.numeric(panel_widths))) > 1
#'     ) {
#'       rlang::abort(
#'         "Different panel widths set. This function only works with uniform panel dimensions."
#'       )
#'     }
#'     panel_dimension <- panel_widths[1] # Use first panel width
#'   }
#'
#'   # We can't use relative length directly with Inf positioning
#'   # Instead, we'll use the absolute length in the annotation
#'
#'   # Extract theme properties for axis ticks
#'   if (rlang::is_null(colour)) {
#'     if (!rlang::is_null(x_breaks)) {
#'       colour <- if (position == "bottom") {
#'         current_theme$axis.ticks.x.bottom$colour %||%
#'           current_theme$axis.ticks.x$colour %||%
#'           current_theme$axis.ticks$colour %||%
#'           "#121B24FF"
#'       } else {
#'         current_theme$axis.ticks.x.top$colour %||%
#'           current_theme$axis.ticks.x$colour %||%
#'           current_theme$axis.ticks$colour %||%
#'           "#121B24FF"
#'       }
#'     }
#'     else if (!rlang::is_null(y_breaks)) {
#'       colour <- if (position == "left") {
#'         current_theme$axis.ticks.y.left$colour %||%
#'           current_theme$axis.ticks.y$colour %||%
#'           current_theme$axis.ticks$colour %||%
#'           "#121B24FF"
#'       } else {
#'         current_theme$axis.ticks.y.right$colour %||%
#'           current_theme$axis.ticks.y$colour %||%
#'           current_theme$axis.ticks$colour %||%
#'           "#121B24FF"
#'       }
#'     }
#'   }
#'
#'   if (rlang::is_null(linewidth)) {
#'     if (!rlang::is_null(x_breaks)) {
#'       linewidth <- if (position == "bottom") {
#'         current_theme$axis.ticks.x.bottom$linewidth %||%
#'           current_theme$axis.ticks.x$linewidth %||%
#'           current_theme$axis.ticks$linewidth %||%
#'           0.5
#'       } else {
#'         current_theme$axis.ticks.x.top$linewidth %||%
#'           current_theme$axis.ticks.x$linewidth %||%
#'           current_theme$axis.ticks$linewidth %||%
#'           0.5
#'       }
#'     }
#'     else if (!rlang::is_null(y_breaks)) {
#'       linewidth <- if (position == "left") {
#'         current_theme$axis.ticks.y.left$linewidth %||%
#'           current_theme$axis.ticks.y$linewidth %||%
#'           current_theme$axis.ticks$linewidth %||%
#'           0.5
#'       } else {
#'         current_theme$axis.ticks.y.right$linewidth %||%
#'           current_theme$axis.ticks.y$linewidth %||%
#'           current_theme$axis.ticks$linewidth %||%
#'           0.5
#'       }
#'     }
#'   }
#'
#'   # Create list to store output
#'   stamp <- list()
#'
#'   # Add theme modification if requested
#'   if (theme_elements == "transparent") {
#'     if (position == "bottom") {
#'       stamp <- c(
#'         stamp,
#'         list(
#'           ggplot2::theme(
#'             axis.ticks.x.bottom = ggplot2::element_line(colour = "transparent")
#'           )
#'         )
#'       )
#'     } else if (position == "top") {
#'       stamp <- c(
#'         stamp,
#'         list(
#'           ggplot2::theme(
#'             axis.ticks.x.top = ggplot2::element_line(colour = "transparent")
#'           )
#'         )
#'       )
#'     } else if (position == "left") {
#'       stamp <- c(
#'         stamp,
#'         list(
#'           ggplot2::theme(
#'             axis.ticks.y.left = ggplot2::element_line(colour = "transparent")
#'           )
#'         )
#'       )
#'     } else if (position == "right") {
#'       stamp <- c(
#'         stamp,
#'         list(
#'           ggplot2::theme(
#'             axis.ticks.y.right = ggplot2::element_line(colour = "transparent")
#'           )
#'         )
#'       )
#'     }
#'   } else if (theme_elements == "blank") {
#'     if (position == "bottom") {
#'       stamp <- c(
#'         stamp,
#'         list(
#'           ggplot2::theme(axis.ticks.x.bottom = ggplot2::element_blank())
#'         )
#'       )
#'     } else if (position == "top") {
#'       stamp <- c(
#'         stamp,
#'         list(
#'           ggplot2::theme(axis.ticks.x.top = ggplot2::element_blank())
#'         )
#'       )
#'     } else if (position == "left") {
#'       stamp <- c(
#'         stamp,
#'         list(
#'           ggplot2::theme(axis.ticks.y.left = ggplot2::element_blank())
#'         )
#'       )
#'     } else if (position == "right") {
#'       stamp <- c(
#'         stamp,
#'         list(
#'           ggplot2::theme(axis.ticks.y.right = ggplot2::element_blank())
#'         )
#'       )
#'     }
#'   } else if (theme_elements == "keep") {
#'     stamp <- c(stamp, list())
#'   }
#'
#'   # Create annotation with segments at the edge
#'   # We use annotation_custom with grobs instead of annotate to avoid scale issues
#'   for (break_val in breaks) {
#'     if (position == "bottom") {
#'       # Create a tick mark grob
#'       tick_grob <- grid::segmentsGrob(
#'         x0 = grid::unit(0.5, "npc"),
#'         x1 = grid::unit(0.5, "npc"),
#'         y0 = grid::unit(0, "npc"),
#'         y1 = grid::unit(0, "npc") - length,
#'         gp = grid::gpar(
#'           col = colour,
#'           lwd = linewidth * 72 / 25.4,
#'           lineend = "butt"
#'         )
#'       )
#'
#'       stamp <- c(
#'         stamp,
#'         list(
#'           ggplot2::annotation_custom(
#'             grob = tick_grob,
#'             xmin = break_val,
#'             xmax = break_val,
#'             ymin = -Inf,
#'             ymax = -Inf
#'           )
#'         )
#'       )
#'     } else if (position == "top") {
#'       tick_grob <- grid::segmentsGrob(
#'         x0 = grid::unit(0.5, "npc"),
#'         x1 = grid::unit(0.5, "npc"),
#'         y0 = grid::unit(1, "npc"),
#'         y1 = grid::unit(1, "npc") + length,
#'         gp = grid::gpar(
#'           col = colour,
#'           lwd = linewidth * 72 / 25.4,
#'           lineend = "butt"
#'         )
#'       )
#'
#'       stamp <- c(
#'         stamp,
#'         list(
#'           ggplot2::annotation_custom(
#'             grob = tick_grob,
#'             xmin = break_val,
#'             xmax = break_val,
#'             ymin = Inf,
#'             ymax = Inf
#'           )
#'         )
#'       )
#'     } else if (position == "left") {
#'       tick_grob <- grid::segmentsGrob(
#'         x0 = grid::unit(0, "npc"),
#'         x1 = grid::unit(0, "npc") - length,
#'         y0 = grid::unit(0.5, "npc"),
#'         y1 = grid::unit(0.5, "npc"),
#'         gp = grid::gpar(
#'           col = colour,
#'           lwd = linewidth * 72 / 25.4,
#'           lineend = "butt"
#'         )
#'       )
#'
#'       stamp <- c(
#'         stamp,
#'         list(
#'           ggplot2::annotation_custom(
#'             grob = tick_grob,
#'             xmin = -Inf,
#'             xmax = -Inf,
#'             ymin = break_val,
#'             ymax = break_val
#'           )
#'         )
#'       )
#'     } else if (position == "right") {
#'       tick_grob <- grid::segmentsGrob(
#'         x0 = grid::unit(1, "npc"),
#'         x1 = grid::unit(1, "npc") + length,
#'         y0 = grid::unit(0.5, "npc"),
#'         y1 = grid::unit(0.5, "npc"),
#'         gp = grid::gpar(
#'           col = colour,
#'           lwd = linewidth * 72 / 25.4,
#'           lineend = "butt"
#'         )
#'       )
#'
#'       stamp <- c(
#'         stamp,
#'         list(
#'           ggplot2::annotation_custom(
#'             grob = tick_grob,
#'             xmin = Inf,
#'             xmax = Inf,
#'             ymin = break_val,
#'             ymax = break_val
#'           )
#'         )
#'       )
#'     }
#'   }
#'
#'   return(stamp)
#' }
