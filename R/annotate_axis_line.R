#' Annotated axis line segment
#'
#' @description Replace axis line with an annotated segment.
#'
#' @param ... Extra parameters passed to `ggplot2::annotate("segment", ...)`.
#' @param x_position The position of the x-axis. One of "bottom" or "top". Leave NULL if not drawing x-axis line.
#' @param y_position The position of the y-axis. One of "left" or "right". Leave NULL if not drawing y-axis line.
#' @param colour The colour of the annotated segment. Inherits from the current theme axis.line etc.
#' @param linewidth The linewidth of the annotated segment. Inherits from the current theme axis.line etc.
#' @param theme_elements What to do with theme axis line elements. Either "transparent", "keep" or "blank". Defaults "transparent".
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
#'   add_row(flipper_length_mm = 195, body_mass_g = 2500, sex = "Female") |>
#'   gg_blanket(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = sex,
#'   ) +
#'   annotate_axis_line(x_position = "bottom") +
#'   geom_point()
#'
annotate_axis_line <- function(
    ...,
    x_position = NULL,
    y_position = NULL,
    colour = NULL,
    linewidth = NULL,
    theme_elements = "transparent"
) {
  rlang::inform(
    "Please use this function with ggplot2::coord_cartesian(clip = 'off')"
  )

  # Validate theme_elements parameter
  if (!theme_elements %in% c("transparent", "keep", "blank")) {
    rlang::abort("theme_elements must be one of 'transparent', 'keep', or 'blank'")
  }

  # Check that at least one position is specified
  if (is.null(x_position) && is.null(y_position)) {
    rlang::abort("Must specify at least one of x_position or y_position")
  }

  # Validate position arguments
  if (!is.null(x_position) && !x_position %in% c("bottom", "top")) {
    rlang::abort("x_position must be one of 'bottom' or 'top'")
  }
  if (!is.null(y_position) && !y_position %in% c("left", "right")) {
    rlang::abort("y_position must be one of 'left' or 'right'")
  }

  # Get current theme
  current_theme <- ggplot2::get_theme()

  stamp <- list()

  # Process x-axis line
  if (!is.null(x_position)) {
    # Extract theme properties for x-axis line
    x_colour <- if (rlang::is_null(colour)) {
      if (x_position == "bottom") {
        current_theme$axis.line.x.bottom$colour %||%
          current_theme$axis.line.x$colour %||%
          current_theme$axis.line$colour %||%
          "#121B24FF"
      } else {
        current_theme$axis.line.x.top$colour %||%
          current_theme$axis.line.x$colour %||%
          current_theme$axis.line$colour %||%
          "#121B24FF"
      }
    } else {
      colour
    }

    x_linewidth <- if (rlang::is_null(linewidth)) {
      if (x_position == "bottom") {
        current_theme$axis.line.x.bottom$linewidth %||%
          current_theme$axis.line.x$linewidth %||%
          current_theme$axis.line$linewidth %||%
          0.5
      } else {
        current_theme$axis.line.x.top$linewidth %||%
          current_theme$axis.line.x$linewidth %||%
          current_theme$axis.line$linewidth %||%
          0.5
      }
    } else {
      linewidth
    }

    # Create x-axis segment
    if (x_position == "bottom") {
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
            colour = x_colour,
            linewidth = x_linewidth,
            !!!list(...)
          )
        )
      )

      # Add theme modification if requested - only for bottom
      if (theme_elements == "transparent") {
        stamp <- c(
          stamp,
          list(ggplot2::theme(axis.line.x.bottom = ggplot2::element_line(colour = "transparent")))
        )
      } else if (theme_elements == "blank") {
        stamp <- c(
          stamp,
          list(ggplot2::theme(axis.line.x.bottom = ggplot2::element_blank()))
        )
      }
    } else { # top
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
            colour = x_colour,
            linewidth = x_linewidth,
            !!!list(...)
          )
        )
      )

      # Add theme modification if requested - only for top
      if (theme_elements == "transparent") {
        stamp <- c(
          stamp,
          list(ggplot2::theme(axis.line.x.top = ggplot2::element_line(colour = "transparent")))
        )
      } else if (theme_elements == "blank") {
        stamp <- c(
          stamp,
          list(ggplot2::theme(axis.line.x.top = ggplot2::element_blank()))
        )
      }
    }
  }

  # Process y-axis line
  if (!is.null(y_position)) {
    # Extract theme properties for y-axis line
    y_colour <- if (rlang::is_null(colour)) {
      if (y_position == "left") {
        current_theme$axis.line.y.left$colour %||%
          current_theme$axis.line.y$colour %||%
          current_theme$axis.line$colour %||%
          "#121B24FF"
      } else {
        current_theme$axis.line.y.right$colour %||%
          current_theme$axis.line.y$colour %||%
          current_theme$axis.line$colour %||%
          "#121B24FF"
      }
    } else {
      colour
    }

    y_linewidth <- if (rlang::is_null(linewidth)) {
      if (y_position == "left") {
        current_theme$axis.line.y.left$linewidth %||%
          current_theme$axis.line.y$linewidth %||%
          current_theme$axis.line$linewidth %||%
          0.5
      } else {
        current_theme$axis.line.y.right$linewidth %||%
          current_theme$axis.line.y$linewidth %||%
          current_theme$axis.line$linewidth %||%
          0.5
      }
    } else {
      linewidth
    }

    # Create y-axis segment
    if (y_position == "left") {
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
            colour = y_colour,
            linewidth = y_linewidth,
            !!!list(...)
          )
        )
      )

      # Add theme modification if requested - only for left
      if (theme_elements == "transparent") {
        stamp <- c(
          stamp,
          list(ggplot2::theme(axis.line.y.left = ggplot2::element_line(colour = "transparent")))
        )
      } else if (theme_elements == "blank") {
        stamp <- c(
          stamp,
          list(ggplot2::theme(axis.line.y.left = ggplot2::element_blank()))
        )
      }
    } else { # right
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
            colour = y_colour,
            linewidth = y_linewidth,
            !!!list(...)
          )
        )
      )

      # Add theme modification if requested - only for right
      if (theme_elements == "transparent") {
        stamp <- c(
          stamp,
          list(ggplot2::theme(axis.line.y.right = ggplot2::element_line(colour = "transparent")))
        )
      } else if (theme_elements == "blank") {
        stamp <- c(
          stamp,
          list(ggplot2::theme(axis.line.y.right = ggplot2::element_blank()))
        )
      }
    }
  }

  return(stamp)
}
