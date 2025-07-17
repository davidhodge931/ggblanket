#' Move legend position with theme-aware adjustments
#'
#' @description Moves the legend to a new position while attempting to preserve
#' the visual balance of the current theme by making proportional adjustments.
#'
#' @param position The position of the legend. Either "right", "top" or "bottom".
#' @param theme_type Optional hint about the theme type. If NULL, will attempt to detect.
#'   Can be "lighter", "darker", or "custom".
#'
#' @return A theme object that can be added to a ggplot.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Works with any theme
#' theme_set(theme_minimal())
#'
#' # Create plot with legend moved to top
#' palmerpenguins::penguins |>
#'   gg_point(
#'     x = flipper_length_mm,
#'     y = body_mass_g,
#'     col = species
#'   ) +
#'   move_legend("top")
#'
move_legend <- function(position = c("right", "top", "bottom"), theme_type = NULL) {
  position <- match.arg(position)

  # Get current theme
  current_theme <- ggplot2::theme_get()

  # Try to detect if it's a blanket theme by checking for specific margin patterns
  is_blanket_theme <- FALSE
  if (is.null(theme_type)) {
    # Check if plot margins follow the blanket pattern (11-based units)
    plot_margin <- current_theme$plot.margin
    if (!is.null(plot_margin)) {
      # Blanket themes use multiples of 11
      margin_values <- as.numeric(plot_margin)
      if (any(margin_values %% 11 == 0 | (margin_values * 3) %% 11 == 0)) {
        is_blanket_theme <- TRUE
      }
    }
  } else {
    is_blanket_theme <- theme_type %in% c("lighter", "darker")
  }

  # If it's a blanket theme, use the specific adjustments
  if (is_blanket_theme) {
    return(move_legend_blanket(position))
  } else {
    # For other themes, use more generic adjustments
    return(move_legend_generic(position))
  }
}

#' Move legend for blanket themes
#' @noRd
move_legend_blanket <- function(position) {
  if (position == "right") {
    ggplot2::theme(
      legend.position = "right",
      # legend.direction = "vertical",
      # legend.justification = c(0, 1),
      legend.location = "panel",
      legend.margin = ggplot2::margin(
        t = 0,
        r = 11 * -1,
        b = 11 * 0.75,
        l = 11 * 0.75
      ),
      legend.box.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
      legend.title = ggplot2::element_text(
        margin = ggplot2::margin(t = 0, r = 0, b = 11 * 0.5, l = 0)
      ),
      legend.byrow = FALSE,

      # Reset axis margins to default
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 11 * -0.33, r = 0, b = 11 * 0.75, l = 0)
      ),
      axis.title.x.top = ggplot2::element_text(
        margin = ggplot2::margin(t = 11 * -1, r = 0, b = 11 * 1, l = 0)
      ),
      axis.text.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 11 * 0.3, r = 0, b = 11 * 1, l = 0)
      ),
      axis.text.x.top = ggplot2::element_text(
        margin = ggplot2::margin(t = 11 * -0.5, r = 0, b = 11 * 0.3, l = 0)
      ),
      axis.title.y.right = ggplot2::element_text(
        margin = ggplot2::margin(t = 0, r = 11 * -0.5, b = 0, l = 11 * 1)
      )
    )
  } else if (position == "top") {
    ggplot2::theme(
      legend.position = "top",
      # legend.direction = "horizontal",
      # legend.justification = c(0, 0.5),
      legend.location = "plot",
      legend.margin = ggplot2::margin(
        t = 11 * -1.5,
        r = 11 * 2,
        b = 11 * 0.5,
        l = 0
      ),
      legend.box.margin = ggplot2::margin(
        t = 11 * 0.5,
        r = 0,
        b = 11 * 0.5,
        l = 0
      ),
      legend.title = ggplot2::element_text(
        margin = ggplot2::margin(t = 11 * 0.25, r = 0, b = 11 * 0.5, l = 0)
      ),
      legend.byrow = TRUE,

      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 11 * -0.33, r = 0, b = 11 * 0.75, l = 0)
      ),
      axis.title.x.top = ggplot2::element_text(
        margin = ggplot2::margin(t = 0, r = 0, b = 11 * 0.5, l = 0)
      ),
      axis.text.x.top = ggplot2::element_text(
        vjust = 0,
        margin = ggplot2::margin(t = 0, r = 0, b = 11 * 0.3, l = 0)
      ),
      axis.title.y.right = ggplot2::element_text(
        margin = ggplot2::margin(t = 0, r = 11 * -0.5, b = 0, l = 11 * 1),
        angle = -90
      )
    )
  } else if (position == "bottom") {
    ggplot2::theme(
      legend.position = "bottom",
      # legend.direction = "horizontal",
      # legend.justification = c(0, 0.5),
      legend.location = "plot",
      legend.margin = ggplot2::margin(t = 0, r = 11 * 2, b = 11 * 0.75, l = 0),
      legend.box.margin = ggplot2::margin(t = 11 * -0.5, r = 0, b = 0, l = 0),
      legend.title = ggplot2::element_text(
        margin = ggplot2::margin(t = 11 * 0.25, r = 0, b = 11 * 0.5, l = 0)
      ),
      legend.byrow = TRUE,

      axis.text.x = ggplot2::element_text(
        vjust = 1,
        margin = ggplot2::margin(t = 11 * 0.3, r = 0, b = 11 * 1, l = 0)
      ),
      axis.text.x.top = ggplot2::element_text(
        vjust = 0,
        margin = ggplot2::margin(t = 11 * -0.5, r = 0, b = 11 * 0.3, l = 0)
      )
    )
  }
}

#' Move legend for generic themes
#' @noRd
move_legend_generic <- function(position) {
  # Get current theme to extract base unit if possible
  current_theme <- ggplot2::theme_get()

  # Try to extract a base unit from text size
  base_unit <- 11  # Default fallback
  if (!is.null(current_theme$text$size)) {
    base_unit <- current_theme$text$size
  }

  if (position == "right") {
    ggplot2::theme(
      legend.position = "right",
      # legend.direction = "vertical",
      # legend.justification = "center",
      legend.box = "vertical",
      legend.byrow = FALSE
    )
  } else if (position == "top") {
    ggplot2::theme(
      legend.position = "top",
      # legend.direction = "horizontal",
      # legend.justification = "center",
      legend.box = "horizontal",
      legend.byrow = TRUE,
      legend.margin = ggplot2::margin(b = base_unit * 0.5),
      legend.box.margin = ggplot2::margin(b = base_unit * 0.5)
    )
  } else if (position == "bottom") {
    ggplot2::theme(
      legend.position = "bottom",
      # legend.direction = "horizontal",
      # legend.justification = "center",
      legend.box = "horizontal",
      legend.byrow = TRUE,
      legend.margin = ggplot2::margin(t = base_unit * 0.5),
      legend.box.margin = ggplot2::margin(t = base_unit * 0.5)
    )
  }
}

#' Alternative: Create theme-specific move_legend functions
#' @rdname move_legend
#' @export
move_legend_lighter <- function(position = c("right", "top", "bottom")) {
  position <- match.arg(position)
  move_legend_blanket(position)
}

#' @rdname move_legend
#' @export
move_legend_darker <- function(position = c("right", "top", "bottom")) {
  position <- match.arg(position)
  move_legend_blanket(position)
}
