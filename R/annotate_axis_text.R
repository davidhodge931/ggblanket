#' Annotated axis text labels
#'
#' @description Add text labels positioned relative to axis tick marks using absolute measurements.
#' This function only works when panel dimensions are set uniformly via panel.widths and panel.heights.
#' It requires a `coord` of `ggplot2::coord_cartesian(clip = "off")`.
#'
#' @param ... Provided to require argument naming, support trailing commas etc.
#' @param x_position,y_position The position of the axis text. One of `"bottom"`/`"top"` or `"left"`/`"right"`.
#' @param x_breaks,y_breaks A vector of axis breaks for text positioning.
#' @param x_labels,y_labels A vector of text labels. If NULL, uses the breaks as labels.
#' @param colour The colour of the text. Inherits from the current theme axis.text etc.
#' @param size The size of the text. Inherits from the current theme axis.text etc.
#' @param family The font family of the text. Inherits from the current theme axis.text etc.
#' @param length The length of the tick marks as a grid unit. Used to position text relative to ticks.
#' @param offset Additional offset for text positioning as a grid unit. Defaults to unit(3, "pt").
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
#'   annotate_axis_text(
#'     x_breaks = c(185, 195, 205, 215, 225),
#'     y_breaks = c(3500, 4500, 5500),
#'     x_labels = c("185mm", "195mm", "205mm", "215mm", "225mm"),
#'     y_labels = c("3.5kg", "4.5kg", "5.5kg"),
#'     x_position = "bottom",
#'     y_position = "left"
#'   ) +
#'   geom_point()
#'
annotate_axis_text <- function(
    ...,
    x_breaks = NULL,
    y_breaks = NULL,
    x_labels = NULL,
    y_labels = NULL,
    x_position = NULL,
    y_position = NULL,
    colour = NULL,
    size = NULL,
    family = NULL,
    length = NULL,
    offset = NULL,
    hjust = NULL,
    vjust = NULL,
    angle = 0,
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

  # Set default labels if not provided
  if (!is.null(x_breaks) && is.null(x_labels)) {
    x_labels <- as.character(x_breaks)
  }
  if (!is.null(y_breaks) && is.null(y_labels)) {
    y_labels <- as.character(y_breaks)
  }

  # Validate panel dimensions
  if (!is.null(x_breaks)) {
    if (is.null(panel_heights)) {
      rlang::abort(
        "panel.heights must be set in theme for x-axis text annotation"
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
    if (length(x_breaks) != length(x_labels)) {
      rlang::abort("x_breaks and x_labels must have the same length")
    }
  }

  if (!is.null(y_breaks)) {
    if (is.null(panel_widths)) {
      rlang::abort(
        "panel.widths must be set in theme for y-axis text annotation"
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
    if (length(y_breaks) != length(y_labels)) {
      rlang::abort("y_breaks and y_labels must have the same length")
    }
  }

  # Set default text offset
  if (is.null(offset)) {
    offset <- grid::unit(3, "pt")
  }

  stamp <- list()

  # Process x-axis text
  if (!is.null(x_breaks)) {
    # Extract theme properties for x-axis text
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
      x_colour <- if (x_position == "bottom") {
        current_theme$axis.text.x.bottom$colour %||%
          current_theme$axis.text.x$colour %||%
          current_theme$axis.text$colour %||%
          current_theme$text$colour %||%
          "black"
      } else {
        current_theme$axis.text.x.top$colour %||%
          current_theme$axis.text.x$colour %||%
          current_theme$axis.text$colour %||%
          current_theme$text$colour %||%
          "black"
      }
    } else {
      x_colour <- colour
    }

    if (rlang::is_null(size)) {
      # Get the raw size value from theme hierarchy
      raw_size <- if (x_position == "bottom") {
        current_theme$axis.text.x.bottom$size %||%
          current_theme$axis.text.x$size %||%
          current_theme$axis.text$size %||%
          current_theme$text$size
      } else {
        current_theme$axis.text.x.top$size %||%
          current_theme$axis.text.x$size %||%
          current_theme$axis.text$size %||%
          current_theme$text$size
      }
      # If we found a theme size, handle rel() objects
      if (!is.null(raw_size)) {
        if (inherits(raw_size, "rel")) {
          base_size <- current_theme$text$size
          # If base_size is also rel() or NULL, use default
          if (is.null(base_size) || inherits(base_size, "rel")) {
            base_size <- 11
          }
          x_size <- as.numeric(raw_size) * base_size
        } else {
          x_size <- raw_size
        }
      } else {
        x_size <- 11
      }
    } else {
      x_size <- size
    }

    if (rlang::is_null(family)) {
      x_family <- if (x_position == "bottom") {
        current_theme$axis.text.x.bottom$family %||%
          current_theme$axis.text.x$family %||%
          current_theme$axis.text$family %||%
          current_theme$text$family %||%
          ""
      } else {
        current_theme$axis.text.x.top$family %||%
          current_theme$axis.text.x$family %||%
          current_theme$axis.text$family %||%
          current_theme$text$family %||%
          ""
      }
    } else {
      x_family <- family
    }

    # Set justification for x-axis text
    x_hjust <- if (is.null(hjust)) 0.5 else hjust  # center horizontally for x-axis text
    x_vjust <- if (is.null(vjust)) {
      if (x_position == "bottom") 1 else 0  # top-align for bottom, bottom-align for top
    } else vjust

    # Add theme modifications for x-axis text
    if (theme_elements == "transparent") {
      if (x_position == "bottom") {
        stamp <- c(stamp, list(
          ggplot2::theme(axis.text.x.bottom = ggplot2::element_text(colour = "transparent"))
        ))
      } else {
        stamp <- c(stamp, list(
          ggplot2::theme(axis.text.x.top = ggplot2::element_text(colour = "transparent"))
        ))
      }
    } else if (theme_elements == "blank") {
      if (x_position == "bottom") {
        stamp <- c(stamp, list(
          ggplot2::theme(axis.text.x.bottom = ggplot2::element_blank())
        ))
      } else {
        stamp <- c(stamp, list(
          ggplot2::theme(axis.text.x.top = ggplot2::element_blank())
        ))
      }
    }

    # Create x-axis text annotations
    for (i in seq_along(x_breaks)) {
      text_grob <- grid::textGrob(
        label = x_labels[i],
        x = grid::unit(0.5, "npc"),
        y = if (x_position == "bottom") {
          grid::unit(0, "npc") - length - offset
        } else {
          grid::unit(1, "npc") + length + offset
        },
        hjust = x_hjust,
        vjust = x_vjust,
        rot = angle,
        gp = grid::gpar(
          col = x_colour,
          fontsize = x_size,
          fontfamily = x_family
        )
      )

      annotation_position <- if (x_position == "bottom") {
        list(xmin = x_breaks[i], xmax = x_breaks[i], ymin = -Inf, ymax = -Inf)
      } else {
        list(xmin = x_breaks[i], xmax = x_breaks[i], ymin = Inf, ymax = Inf)
      }

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
  }

  # Process y-axis text
  if (!is.null(y_breaks)) {
    # Extract theme properties for y-axis text
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
      y_colour <- if (y_position == "left") {
        current_theme$axis.text.y.left$colour %||%
          current_theme$axis.text.y$colour %||%
          current_theme$axis.text$colour %||%
          "#121B24FF"
      } else {
        current_theme$axis.text.y.right$colour %||%
          current_theme$axis.text.y$colour %||%
          current_theme$axis.text$colour %||%
          "#121B24FF"
      }
    } else {
      y_colour <- colour
    }

    if (rlang::is_null(size)) {
      # Get the raw size value from theme hierarchy
      raw_size <- if (y_position == "left") {
        current_theme$axis.text.y.left$size %||%
          current_theme$axis.text.y$size %||%
          current_theme$axis.text$size %||%
          current_theme$text$size
      } else {
        current_theme$axis.text.y.right$size %||%
          current_theme$axis.text.y$size %||%
          current_theme$axis.text$size %||%
          current_theme$text$size
      }
      # If we found a theme size, handle rel() objects
      if (!is.null(raw_size)) {
        if (inherits(raw_size, "rel")) {
          base_size <- current_theme$text$size
          # If base_size is also rel() or NULL, use default
          if (is.null(base_size) || inherits(base_size, "rel")) {
            base_size <- 11
          }
          y_size <- as.numeric(raw_size) * base_size
        } else {
          y_size <- raw_size
        }
      } else {
        y_size <- 11
      }
    } else {
      y_size <- size
    }

    if (rlang::is_null(family)) {
      y_family <- if (y_position == "left") {
        current_theme$axis.text.y.left$family %||%
          current_theme$axis.text.y$family %||%
          current_theme$axis.text$family %||%
          ""
      } else {
        current_theme$axis.text.y.right$family %||%
          current_theme$axis.text.y$family %||%
          current_theme$axis.text$family %||%
          ""
      }
    } else {
      y_family <- family
    }

    # Set justification for y-axis text
    y_hjust <- if (is.null(hjust)) {
      if (y_position == "left") 1 else 0  # right-align for left, left-align for right
    } else hjust
    y_vjust <- if (is.null(vjust)) 0.5 else vjust  # center vertically for y-axis text

    # Add theme modifications for y-axis text
    if (theme_elements == "transparent") {
      if (y_position == "left") {
        stamp <- c(stamp, list(
          ggplot2::theme(axis.text.y.left = ggplot2::element_text(colour = "transparent"))
        ))
      } else {
        stamp <- c(stamp, list(
          ggplot2::theme(axis.text.y.right = ggplot2::element_text(colour = "transparent"))
        ))
      }
    } else if (theme_elements == "blank") {
      if (y_position == "left") {
        stamp <- c(stamp, list(
          ggplot2::theme(axis.text.y.left = ggplot2::element_blank())
        ))
      } else {
        stamp <- c(stamp, list(
          ggplot2::theme(axis.text.y.right = ggplot2::element_blank())
        ))
      }
    }

    # Create y-axis text annotations
    for (i in seq_along(y_breaks)) {
      text_grob <- grid::textGrob(
        label = y_labels[i],
        x = if (y_position == "left") {
          grid::unit(0, "npc") - length - offset
        } else {
          grid::unit(1, "npc") + length + offset
        },
        y = grid::unit(0.5, "npc"),
        hjust = y_hjust,
        vjust = y_vjust,
        rot = angle,
        gp = grid::gpar(
          col = y_colour,
          fontsize = y_size,
          fontfamily = y_family
        )
      )

      annotation_position <- if (y_position == "left") {
        list(xmin = -Inf, xmax = -Inf, ymin = y_breaks[i], ymax = y_breaks[i])
      } else {
        list(xmin = Inf, xmax = Inf, ymin = y_breaks[i], ymax = y_breaks[i])
      }

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
  }

  return(stamp)
}
