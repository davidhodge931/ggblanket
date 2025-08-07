#' Annotated panel grid lines
#'
#' @description Replace panel grid lines with annotated segments.
#' It requires a `coord` of `ggplot2::coord_cartesian(clip = "off")`.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param axis The axis to annotate. One of "x" or "y".
#' @param minor TRUE or FALSE as whether it applies to the panel.grid.minor.
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
#'
#' set_blanket()
#'
#' palmerpenguins::penguins |>
#'   gg_blanket(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     facet = species,
#'     y_breaks_n = 20,
#'     y_labels = label_every_nth(),
#'   ) +
#'   annotate_panel_grid(
#'     axis = "y",
#'     breaks = seq(2800, 6400, 400),
#'     linewidth = rel(0.5),
#'   ) +
#'   annotate_panel_grid(
#'     axis = "y",
#'     breaks = seq(2600, 6200, 400),
#'   ) +
#'   geom_point(
#'     colour = col_multiply(get_geom_defaults("point")$colour),
#'   )
#'
annotate_panel_grid <- function(
    ...,
    axis,
    breaks,
    minor = FALSE,
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
    rlang::abort(
      "theme_element must be one of 'transparent', 'keep', or 'blank'"
    )
  }

  # Get current theme
  current_theme <- ggplot2::theme_get()

  # Helper function to resolve rel() objects for numeric properties
  resolve_rel_property <- function(property_value, base_property, default_value) {
    if (rlang::is_null(property_value)) {
      return(default_value)
    }

    if (inherits(property_value, "rel")) {
      base_value <- if (rlang::is_null(base_property) || inherits(base_property, "rel")) {
        default_value
      } else {
        base_property
      }
      return(as.numeric(property_value) * base_value)
    }

    return(property_value)
  }

  # Helper function to extract theme properties
  if (!minor) {
    extract_theme_property <- function(property, default) {
      current_theme[["panel.grid.major"]][[property]] %||%
        current_theme[["panel.grid"]][[property]] %||%
        default
    }
  } else if (minor) {
    extract_theme_property <- function(property, default) {
      current_theme[["panel.grid.minor"]][[property]] %||%
        current_theme[["panel.grid"]][[property]] %||%
        default
    }
  }

  # Extract theme properties with proper rel() handling
  grid_colour <- if (rlang::is_null(colour)) {
    extract_theme_property("colour", "#E6E9EFFF")
  } else {
    colour
  }

  # Handle rel() objects for linewidth
  if (rlang::is_null(linewidth)) {
    raw_linewidth <- extract_theme_property("linewidth", NULL)
    base_linewidth <- current_theme[["line"]]$linewidth %||% 0.5
    grid_linewidth <- resolve_rel_property(raw_linewidth, base_linewidth, 0.5)
  } else {
    # Handle user-provided rel() objects
    if (inherits(linewidth, "rel")) {
      base_linewidth <- current_theme[["line"]]$linewidth %||% 0.5
      grid_linewidth <- as.numeric(linewidth) * base_linewidth
    } else {
      grid_linewidth <- linewidth
    }
  }

  grid_linetype <- if (rlang::is_null(linetype)) {
    extract_theme_property("linetype", "solid")
  } else {
    linetype
  }

  stamp <- list()

  # Add theme modification if requested
  if (!minor) {
    if (theme_element == "transparent") {
      if (axis == "x") {
        stamp <- c(
          stamp,
          list(
            ggplot2::theme(
              panel.grid.major.x = ggplot2::element_line(colour = "transparent")
            )
          )
        )
      } else {
        # y
        stamp <- c(
          stamp,
          list(
            ggplot2::theme(
              panel.grid.major.y = ggplot2::element_line(colour = "transparent")
            )
          )
        )
      }
    } else if (theme_element == "blank") {
      if (axis == "x") {
        stamp <- c(
          stamp,
          list(
            ggplot2::theme(
              panel.grid.major.x = ggplot2::element_blank()
            )
          )
        )
      } else {
        # y
        stamp <- c(
          stamp,
          list(
            ggplot2::theme(
              panel.grid.major.y = ggplot2::element_blank()
            )
          )
        )
      }
    }
  } else if (minor) {
    if (theme_element == "transparent") {
      if (axis == "x") {
        stamp <- c(
          stamp,
          list(
            ggplot2::theme(
              panel.grid.minor.x = ggplot2::element_line(colour = "transparent")
            )
          )
        )
      } else {
        # y
        stamp <- c(
          stamp,
          list(
            ggplot2::theme(
              panel.grid.minor.y = ggplot2::element_line(colour = "transparent")
            )
          )
        )
      }
    } else if (theme_element == "blank") {
      if (axis == "x") {
        stamp <- c(
          stamp,
          list(
            ggplot2::theme(
              panel.grid.minor.x = ggplot2::element_blank()
            )
          )
        )
      } else {
        # y
        stamp <- c(
          stamp,
          list(
            ggplot2::theme(
              panel.grid.minor.y = ggplot2::element_blank()
            )
          )
        )
      }
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
  } else {
    # y-axis
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
