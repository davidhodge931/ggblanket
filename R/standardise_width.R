#' Standardise width
#'
#' Calculate widths that are standardised, accounting for panel dimensions.
#'
#' @param from_width In the reference, width value. Required.
#' @param from_n In the reference, number of x aesthetic groups. Required.
#' @param from_dodge_n In the reference, number of fill aesthetic etc groups dodged. Defaults to 1.
#' @param from_dodge_padding In the reference, amount of padding (in position_dodge2). Defaults to 0.
#' @param from_orientation In the reference, orientation of the plot. Either "x" (default) or "y".
#' @param from_panel_widths In the reference, unit vector of individual panel widths. If NULL, panels assumed equal.
#' @param from_panel_heights In the reference, unit vector of individual panel heights. If NULL, panels assumed equal.
#' @param to_n Number of x aesthetic groups.
#' @param to_dodge_n Number of fill aesthetic etc groups dodged. If NULL, inherits from from_dodge_n.
#' @param to_dodge_padding Amount of padding (in position_dodge2). If NULL, inherits from from_dodge_padding.
#' @param to_orientation Orientation of the plot. Either "x" or "y". If NULL, inherits from from_orientation.
#' @param to_panel_widths Unit vector of individual panel widths. If NULL, inherits from from_panel_widths.
#' @param to_panel_heights Unit vector of individual panel heights. If NULL, inherits from from_panel_heights.
#'
#' @returns A numeric value or vector of values (one per panel if panel dimensions vary)
#' @noRd
standardise_width <- function(from_width,
                              from_n,
                              from_dodge_n = 1,
                              from_dodge_padding = 0,
                              from_orientation = "x",
                              from_panel_widths = NULL,
                              from_panel_heights = NULL,
                              to_n = from_n,
                              to_dodge_n = from_dodge_n,
                              to_dodge_padding = from_dodge_padding,
                              to_orientation = from_orientation,
                              to_panel_widths = from_panel_widths,
                              to_panel_heights = from_panel_heights) {

  # Check required arguments
  if (missing(from_width) || missing(from_n)) {
    rlang::abort("Both from_width and from_n must be specified")
  }

  # Base category scaling
  base_width <- (to_n / from_n) * from_width

  # Dodge scaling: maintain consistent individual bar width
  if (to_dodge_n > 1 || from_dodge_n > 1) {
    width <- base_width * (to_dodge_n / from_dodge_n)
  } else {
    width <- base_width
  }

  # dodge_padding adjustment for position_dodge2
  if (to_dodge_padding > 0 || from_dodge_padding > 0) {
    # Adjust for difference in dodge_padding - more dodge_padding requires wider total width
    dodge_padding_factor <- (1 + to_dodge_padding) / (1 + from_dodge_padding)
    width <- width * dodge_padding_factor
  }

  # Panel dimension adjustment for visual consistency
  if (!is.null(to_panel_widths) || !is.null(to_panel_heights) ||
      !is.null(from_panel_widths) || !is.null(from_panel_heights)) {

    # Get the relevant dimension for each orientation
    # For orientation "x" (vertical bars): width depends on panel width
    # For orientation "y" (horizontal bars): width depends on panel height

    current_relevant_dim <- if (to_orientation == "x") {
      if (!is.null(to_panel_widths)) as.numeric(to_panel_widths)[1] else 1
    } else {
      if (!is.null(to_panel_heights)) as.numeric(to_panel_heights)[1] else 1
    }

    reference_relevant_dim <- if (from_orientation == "x") {
      if (!is.null(from_panel_widths)) as.numeric(from_panel_widths)[1] else 1
    } else {
      if (!is.null(from_panel_heights)) as.numeric(from_panel_heights)[1] else 1
    }

    # Scale to maintain visual consistency
    scaling_factor <- reference_relevant_dim / current_relevant_dim
    width <- width * scaling_factor
  }

  # Safety check
  if (any(width >= 1)) {
    rlang::abort("width cannot be greater than or equal to 1")
  }

  return(width)
}
