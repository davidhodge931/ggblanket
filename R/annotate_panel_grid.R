#' Annotated panel grid lines
#'
#' @description Replace panel grid lines with annotated segments.
#' It requires a `coord` of `ggplot2::coord_cartesian(clip = "off")`.
#'
#' @param axis The axis to annotate. One of "x" or "y".
#' @param ... Provided to require argument naming, support trailing commas etc.
#' @param breaks A vector of axis breaks for the panel grid.
#' @param colour The colour of grid lines. Inherits from current theme panel.grid.major etc.
#' @param linewidth The linewidth of grid lines. Inherits from current theme panel.grid.major etc.
#' @param linetype The linetype of grid lines. Inherits from current theme panel.grid.major etc.
#' @param theme_element What to do with the equivalent theme element. Either "transparent", "keep" or "blank". Defaults "transparent".
#'
#' @return A list of annotate layers and theme elements.
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(stringr)
#' library(palmerpenguins)
#'
#' set_blanket()
#'
#' penguins |>
#'   tidyr::drop_na(sex) |>
#'   mutate(across(sex, \(x) str_to_sentence(x))) |>
#'   gg_blanket(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = sex,
#'   ) +
#'   annotate_panel_grid(axis = "x", breaks = seq(190, 230, 15)) +
#'   annotate_panel_grid(axis = "y", breaks = seq(3000, 6500, 250)) +
#'   geom_point()
#'
annotate_panel_grid <- function(
    axis,
    ...,
    breaks,
    colour = NULL,
    linewidth = NULL,
    linetype = NULL,
    theme_element = "transparent"
) {

  # Validate arguments
  if (!axis %in% c("x", "y")) {
    rlang::abort("axis must be one of 'x' or 'y'")
  }

  if (!theme_element %in% c("transparent", "keep", "blank")) {
    rlang::abort("theme_element must be one of 'transparent', 'keep', or 'blank'")
  }

  # Get current theme
  current_theme <- ggplot2::get_theme()

  # Helper function to extract theme properties
  extract_theme_property <- function(property, default) {
    current_theme[["panel.grid.major"]][[property]] %||%
      current_theme[["panel.grid"]][[property]] %||%
      default
  }

  # Extract theme properties
  grid_colour <- if (rlang::is_null(colour)) {
    extract_theme_property("colour", "#E6E9EFFF")
  } else {
    colour
  }

  grid_linewidth <- if (rlang::is_null(linewidth)) {
    extract_theme_property("linewidth", 0.5)
  } else {
    linewidth
  }

  grid_linetype <- if (rlang::is_null(linetype)) {
    extract_theme_property("linetype", "solid")
  } else {
    linetype
  }

  stamp <- list()

  # Add theme modification if requested
  if (theme_element == "transparent") {
    if (axis == "x") {
      stamp <- c(stamp, list(
        ggplot2::theme(
          panel.grid.major.x = ggplot2::element_line(colour = "transparent"),
          panel.grid.minor.x = ggplot2::element_line(colour = "transparent")
        )
      ))
    } else { # y
      stamp <- c(stamp, list(
        ggplot2::theme(
          panel.grid.major.y = ggplot2::element_line(colour = "transparent"),
          panel.grid.minor.y = ggplot2::element_line(colour = "transparent")
        )
      ))
    }
  } else if (theme_element == "blank") {
    if (axis == "x") {
      stamp <- c(stamp, list(
        ggplot2::theme(
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor.x = ggplot2::element_blank()
        )
      ))
    } else { # y
      stamp <- c(stamp, list(
        ggplot2::theme(
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank()
        )
      ))
    }
  }

  # Create grid lines
  if (axis == "x") {
    # Add vertical grid lines
    stamp <- c(
      stamp,
      list(
        rlang::exec(
          ggplot2::annotate,
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
  } else { # y-axis
    # Add horizontal grid lines
    stamp <- c(
      stamp,
      list(
        rlang::exec(
          ggplot2::annotate,
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

  return(stamp)
}
