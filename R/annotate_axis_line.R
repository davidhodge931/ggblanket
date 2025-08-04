#' Annotated axis line segment
#'
#' @description Replace axis line with an annotated segment.
#'
#' @param axis The axis to annotate. One of "x" or "y".
#' @param ... Provided to require argument naming, support trailing commas etc.
#' @param position The position of the axis. For x-axis: "bottom" or "top". For y-axis: "left" or "right". Defaults to "bottom" for x-axis and "left" for y-axis.
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
#'   annotate_axis_line(axis = "x") +
#'   annotate_axis_line(axis = "y") +
#'   geom_point()
#'
annotate_axis_line <- function(
    axis,
    ...,
    position = NULL,
    colour = NULL,
    linewidth = NULL,
    theme_elements = "transparent"
) {

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

  # Get current theme
  current_theme <- ggplot2::get_theme()

  # Helper function to extract theme properties
  extract_theme_property <- function(property, default) {
    if (axis == "x") {
      current_theme[[paste0("axis.line.x.", position)]][[property]] %||%
        current_theme[["axis.line.x"]][[property]] %||%
        current_theme[["axis.line"]][[property]] %||%
        default
    } else {
      current_theme[[paste0("axis.line.y.", position)]][[property]] %||%
        current_theme[["axis.line.y"]][[property]] %||%
        current_theme[["axis.line"]][[property]] %||%
        default
    }
  }

  # Extract theme properties
  line_colour <- if (rlang::is_null(colour)) {
    extract_theme_property("colour", "#121B24FF")
  } else {
    colour
  }

  line_linewidth <- if (rlang::is_null(linewidth)) {
    extract_theme_property("linewidth", 0.5)
  } else {
    linewidth
  }

  stamp <- list()

  # Create axis segment
  if (axis == "x") {
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
            colour = line_colour,
            linewidth = line_linewidth,
            ...
          )
        )
      )
    }
  } else { # y-axis
    if (position == "left") {
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
            colour = line_colour,
            linewidth = line_linewidth,
            ...
          )
        )
      )
    }
  }

  # Add theme modification if requested
  if (theme_elements == "transparent") {
    theme_element <- paste0("axis.line.", axis, ".", position)
    theme_mod <- list()
    theme_mod[[theme_element]] <- ggplot2::element_line(colour = "transparent")
    stamp <- c(stamp, list(do.call(ggplot2::theme, theme_mod)))
  } else if (theme_elements == "blank") {
    theme_element <- paste0("axis.line.", axis, ".", position)
    theme_mod <- list()
    theme_mod[[theme_element]] <- ggplot2::element_blank()
    stamp <- c(stamp, list(do.call(ggplot2::theme, theme_mod)))
  }

  return(stamp)
}
