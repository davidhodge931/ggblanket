#' Standardise width
#'
#' Calculate widths that are standardised for bars, boxplots, errorbars etc.
#'
#' @param ... Provided to force user argument naming etc.
#' @param from_n Number of x aesthetic groups in the current plot. Required.
#' @param from_dodge_n Number of fill aesthetic etc groups dodged in the current plot. Defaults to 1.
#' @param from_aspect aspect of the current plot. Either "x" (default) or "y".
#' @param from_panel_widths Unit vector of individual panel widths in the current plot. If NULL, panels assumed equal.
#' @param from_panel_heights Unit vector of individual panel heights in the current plot. If NULL, panels assumed equal.
#' @param to_width Width value in the reference standard. Required.
#' @param to_n Number of x aesthetic groups in the reference standard. Required.
#' @param to_dodge_n Number of fill aesthetic etc groups dodged in the reference standard. Defaults to 1.
#' @param to_aspect aspect of the reference standard plot. Either "x" (default) or "y".
#' @param to_panel_widths Unit vector of individual panel widths in the reference standard. If NULL, panels assumed equal.
#' @param to_panel_heights Unit vector of individual panel heights in the reference standard. If NULL, panels assumed equal.
#'
#' @return A numeric value
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
#'   )
#' )
#'
#' # create reference standard
#' penguins |>
#'   tidyr::drop_na(sex) |>
#'   mutate(across(sex, \(x) str_to_sentence(x))) |>
#'   gg_bar(
#'     x = species,
#'     col = sex,
#'     width = 0.25,
#'   )
#'
#' # align width with reference standard
#' penguins |>
#'   tidyr::drop_na(sex) |>
#'   mutate(across(sex, \(x) str_to_sentence(x))) |>
#'   gg_bar(
#'     x = sex,
#'     col = species,
#'     width = standardise_width(from_n = 2, to_width = 0.25, to_n = 3)
#'   )
#'
#' penguins |>
#'   tidyr::drop_na(sex) |>
#'   mutate(across(sex, \(x) str_to_sentence(x))) |>
#'   gg_bar(
#'     x = sex,
#'     col = species,
#'     position = position_dodge(),
#'     width = standardise_width(from_n = 2, from_dodge_n = 3, to_width = 0.25, to_n = 3)
#'   )
#'
#' penguins |>
#'   tidyr::drop_na(sex) |>
#'   mutate(across(sex, \(x) str_to_sentence(x))) |>
#'   gg_bar(
#'     y = sex,
#'     col = species,
#'     position = position_dodge(),
#'     width = standardise_width(
#'       from_n = 2,
#'       from_dodge_n = 3,
#'       from_aspect = "y",
#'       to_width = 0.25,
#'       to_n = 3,
#'     )
#'   )
#'
#'
standardise_width <- function(
    ...,
    from_n,
    from_dodge_n = 1,
    from_aspect = "x",
    from_panel_widths = NULL,
    from_panel_heights = NULL,
    to_width,
    to_n,
    to_dodge_n = 1,
    to_aspect = "x",
    to_panel_widths = NULL,
    to_panel_heights = NULL
) {


  # Check required arguments
  if (missing(to_width) | missing(to_n) | missing(from_n)) {
    rlang::abort("to_width, to_n, and from_n must all be specified")
  }

  # Get current theme
  current_theme <- ggplot2::theme_get()

  # Extract panel dimensions from theme if not provided
  if (is.null(from_panel_widths) && !is.null(current_theme$panel.widths)) {
    from_panel_widths <- current_theme$panel.widths
  }
  if (is.null(from_panel_heights) && !is.null(current_theme$panel.heights)) {
    from_panel_heights <- current_theme$panel.heights
  }
  if (is.null(to_panel_widths) && !is.null(current_theme$panel.widths)) {
    to_panel_widths <- current_theme$panel.widths
  }
  if (is.null(to_panel_heights) && !is.null(current_theme$panel.heights)) {
    to_panel_heights <- current_theme$panel.heights
  }

  if (rlang::is_null(from_panel_widths) |
      rlang::is_null(to_panel_heights) |
      rlang::is_null(to_panel_widths) |
      rlang::is_null(to_panel_heights)) {
    rlang::abort("panel widths and heights must be set or specified")
  }

  # Validate panel dimensions - if any provided, all must be provided
  if (!is.null(from_panel_widths) || !is.null(from_panel_heights) ||
      !is.null(to_panel_widths) || !is.null(to_panel_heights)) {
    if (is.null(from_panel_widths) || is.null(from_panel_heights) ||
        is.null(to_panel_widths) || is.null(to_panel_heights)) {
      rlang::abort("If any panel dimension is provided, all four (from_panel_widths, from_panel_heights, to_panel_widths, to_panel_heights) must be provided")
    }
  }

  # Check for mixed units in panel dimensions
  panel_dims <- list(
    from_panel_widths,
    from_panel_heights,
    to_panel_widths,
    to_panel_heights
  )
  panel_dims <- panel_dims[!purrr::map_lgl(panel_dims, is.null)] # Remove NULL values

  if (length(panel_dims) > 1) {
    # Extract units from each non-NULL panel dimension
    units_list <- purrr::map_chr(panel_dims, function(x) {
      if (inherits(x, "unit")) {
        as.character(attr(x, "unit"))
      } else {
        "numeric" # Plain numeric values
      }
    })

    # Check if all units are the same
    if (length(unique(units_list)) > 1) {
      rlang::abort(
        "All panel dimensions must use the same units. Mixed units detected."
      )
    }
  }

  # Base category scaling
  base_width <- (from_n / to_n) * to_width

  # Dodge scaling: maintain consistent individual bar width
  if (from_dodge_n > 1 | to_dodge_n > 1) {
    width <- base_width * (from_dodge_n / to_dodge_n)
  } else {
    width <- base_width
  }

  # Panel dimension adjustment for visual consistency
  if (
    !is.null(to_panel_widths) |
    !is.null(to_panel_heights) |
    !is.null(from_panel_widths) |
    !is.null(from_panel_heights)
  ) {
    # Get the relevant dimension for each aspect
    # For aspect "x" (vertical bars): width depends on panel width
    # For aspect "y" (horizontal bars): width depends on panel height

    current_relevant_dim <- if (from_aspect == "x") {
      if (!is.null(from_panel_widths)) as.numeric(from_panel_widths)[1] else 1
    } else {
      if (!is.null(from_panel_heights)) as.numeric(from_panel_heights)[1] else 1
    }

    reference_relevant_dim <- if (to_aspect == "x") {
      if (!is.null(to_panel_widths)) as.numeric(to_panel_widths)[1] else 1
    } else {
      if (!is.null(to_panel_heights)) as.numeric(to_panel_heights)[1] else 1
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
