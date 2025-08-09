#' Annotate panel grid segments
#'
#' @description Create annotated segments of the panel grid.
#'
#' This function is designed to work with a theme that is globally set with [ggblanket::set_blanket] or [ggplot2::set_theme].
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param x A vector of x-axis breaks for vertical grid lines. Cannot be used together with y.
#' @param y A vector of y-axis breaks for horizontal grid lines. Cannot be used together with x.
#' @param colour The colour of grid lines. Inherits from current theme panel.grid.major etc.
#' @param linewidth The linewidth of grid lines. Inherits from current theme panel.grid.major etc.
#' @param linetype The linetype of grid lines. Inherits from current theme panel.grid.major etc.
#' @param theme_element What to do with the equivalent theme elements. Either "keep" , "transparent", or "blank". Defaults "keep".
#'
#' @return A list of annotate layers and theme elements.
#' @export
#'
annotate_panel_grid <- function(
    ...,
    x = NULL,
    y = NULL,
    colour = NULL,
    linewidth = NULL,
    linetype = NULL,
    theme_element = "keep"
) {
  # Validate arguments
  if (is.null(x) && is.null(y)) {
    rlang::abort("Either x or y must be specified")
  }

  if (!is.null(x) && !is.null(y)) {
    rlang::abort("Only one of x or y can be specified")
  }

  if (!theme_element %in% c("transparent", "keep", "blank")) {
    rlang::abort(
      "theme_element must be one of 'transparent', 'keep', or 'blank'"
    )
  }

  # Determine axis from x/y
  axis <- if (!is.null(x)) "x" else "y"

  # Get breaks
  breaks <- if (!is.null(x)) x else y

  # Check for empty breaks
  if (length(breaks) == 0) {
    return(list())
  }

  # Get current theme
  current_theme <- ggplot2::theme_get()

  # Build hierarchy for panel grid from most specific to least specific
  # Only use major grid elements for styling
  grid_major_specific <- paste0("panel.grid.major.", axis)
  grid_major <- "panel.grid.major"
  grid_general <- "panel.grid"

  # Use only major grid hierarchy for styling
  grid_hierarchy <- c(
    grid_major_specific,
    grid_major,
    grid_general
  )

  # Find the first non-blank resolved grid element
  resolved_grid_element <- grid_hierarchy |>
    purrr::map(\(x) ggplot2::calc_element(x, current_theme, skip_blank = TRUE)) |>
    purrr::detect(\(x) !is.null(x) && !inherits(x, "element_blank"))

  # If still no element found, create a minimal fallback
  if (is.null(resolved_grid_element)) {
    resolved_grid_element <- list(
      colour = "grey90",
      linewidth = 0.5,
      linetype = "solid"
    )
  }

  # Extract theme properties with proper resolution
  grid_colour <- colour %||% resolved_grid_element$colour %||% "grey90"

  # Handle linewidth with proper rel() support
  if (rlang::is_null(linewidth)) {
    grid_linewidth <- resolved_grid_element$linewidth %||% 0.5
  } else {
    if (inherits(linewidth, "rel")) {
      # Apply user's rel() to the resolved theme linewidth
      base_linewidth <- resolved_grid_element$linewidth %||% 0.5
      grid_linewidth <- as.numeric(linewidth) * base_linewidth
    } else {
      grid_linewidth <- linewidth
    }
  }

  grid_linetype <- linetype %||% resolved_grid_element$linetype %||% "solid"

  stamp <- list()

  # Add theme modification if requested - applies to both major and minor grid
  if (theme_element == "transparent") {
    if (axis == "x") {
      stamp <- c(
        stamp,
        list(
          ggplot2::theme(
            panel.grid.major.x = ggplot2::element_line(colour = "transparent"),
            panel.grid.minor.x = ggplot2::element_line(colour = "transparent")
          )
        )
      )
    } else {  # y
      stamp <- c(
        stamp,
        list(
          ggplot2::theme(
            panel.grid.major.y = ggplot2::element_line(colour = "transparent"),
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
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor.x = ggplot2::element_blank()
          )
        )
      )
    } else {  # y
      stamp <- c(
        stamp,
        list(
          ggplot2::theme(
            panel.grid.major.y = ggplot2::element_blank(),
            panel.grid.minor.y = ggplot2::element_blank()
          )
        )
      )
    }
  }

  # Create grid lines
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

  return(stamp)
}
