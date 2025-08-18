#' Default width reference
#' @noRd
width_reference <- list(
  width = 0.2,
  n = 3,
  dodge_n = 1,
  aspect = "x",
  panel_heights = rep(grid::unit(50, "mm"), 100),
  panel_widths = rep(grid::unit(75, "mm"), 100)
)

#' Update width reference
#'
#' @description
#' Update the width reference used by `standardise_width`.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param width Width value for the reference standard.
#' @param n Number of categories (excluding dodge groups) in the reference standard.
#' @param dodge_n Number of dodge groups in reference standard.
#' @param aspect Aspect of reference standard ("x" or "y").
#' @param panel_heights Panel heights for reference standard.
#' @param panel_widths Panel widths for reference standard.
#'
#' @export
update_width_reference <- function(
  ...,
  width = NULL,
  n = NULL,
  dodge_n = NULL,
  aspect = NULL,
  panel_heights = NULL,
  panel_widths = NULL
) {
  width_reference <- getOption("ggblanket.width_reference", width_reference)

  if (!rlang::is_null(width)) {
    width_reference$width <- width
  }
  if (!rlang::is_null(n)) {
    width_reference$n <- n
  }
  if (!rlang::is_null(dodge_n)) {
    width_reference$dodge_n <- dodge_n
  }
  if (!rlang::is_null(aspect)) {
    width_reference$aspect <- aspect
  }
  if (!rlang::is_null(panel_heights)) {
    width_reference$panel_heights <- panel_heights
  }
  if (!rlang::is_null(panel_widths)) {
    width_reference$panel_widths <- panel_widths
  }

  options(ggblanket.width_reference = width_reference)
}

#' Standardise width to the reference
#'
#' @description
#' Standardise the width against the reference, so that widths look the same across plots.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param n Number of categories (excluding dodge groups) in the plot with width to be standardised.
#' @param dodge_n Number of dodge groups in the plot with width to be standardised.
#' @param aspect Aspect ("x" or "y") in the plot with width to be standardised.
#' @param panel_widths Panel widths in the plot with width to be standardised.
#' @param panel_heights Panel heights in the plot with width to be standardised.
#'
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
#' penguins |>
#'   tidyr::drop_na(sex) |>
#'   mutate(across(sex, \(x) str_to_sentence(x))) |>
#'   gg_bar(
#'     x = sex,
#'     col = species,
#'     width = standardise_width(
#'       n = 2,
#'       dodge_n = 1,
#'       aspect = "x",
#'     )
#'   )
#'
#' penguins |>
#'   tidyr::drop_na(sex) |>
#'   mutate(across(sex, \(x) str_to_sentence(x))) |>
#'   gg_bar(
#'     x = sex,
#'     col = species,
#'     position = position_dodge(),
#'     width = standardise_width(
#'       n = 2,
#'       dodge_n = 3,
#'       aspect = "x",
#'     )
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
#'       n = 2,
#'       dodge_n = 3,
#'       aspect = "y",
#'     )
#'   )
#'
#' d <- tibble::tibble(
#'   continent = c("Europe","Europe","Europe",
#'                 "Europe","Europe","South America","South America"),
#'   country = c("AT", "DE", "DK", "ES", "PK", "TW", "BR"),
#'   value = c(10L, 15L, 20L, 25L, 17L, 13L, 5L)
#' )
#'
#' max_n <- d |>
#'   group_by(continent) |>
#'   count() |>
#'   ungroup() |>
#'   filter(n == max(n)) |>
#'   pull(n)
#'
#' d |>
#'   mutate(country = forcats::fct_rev(country)) |>
#'   gg_col(y = country,
#'          x = value,
#'          facet = continent,
#'          width = standardise_width(
#'            n = max_n,
#'            dodge_n = 1,
#'            aspect = "y",
#'          ),
#'          facet_scales = "free_y",
#'   ) +
#'   scale_y_discrete(continuous.limits = c(1, max_n)) +
#'   coord_cartesian(reverse = "y", clip = "off")
#'
standardise_width <- function(
  ...,
  n = NULL,
  dodge_n = 1,
  aspect = "x",
  panel_widths = ggplot2::theme_get()$panel.widths,
  panel_heights = ggplot2::theme_get()$panel.heights
) {
  if (missing(n)) {
    rlang::abort("n must be specified")
  }

  # Input validation
  if (!aspect %in% c("x", "y")) {
    rlang::abort("aspect must be 'x' or 'y'")
  }
  if (n <= 0) {
    rlang::abort("n must be positive")
  }
  if (dodge_n <= 0) {
    rlang::abort("dodge_n must be positive")
  }

  # Get global standard
  ws <- getOption("ggblanket.width_reference", width_reference)

  # Validation
  if (
    rlang::is_null(panel_widths) |
      rlang::is_null(panel_heights) |
      rlang::is_null(ws$panel_widths) |
      rlang::is_null(ws$panel_heights)
  ) {
    rlang::abort("panel widths and heights must be set or specified")
  }

  # Base category scaling
  base_width <- (n / ws$n) * ws$width

  # Dodge scaling
  width <- if (dodge_n > 1 | ws$dodge_n > 1) {
    base_width * (dodge_n / ws$dodge_n)
  } else {
    base_width
  }

  # Panel dimension adjustment
  if (
    !rlang::is_null(ws$panel_widths) |
      !rlang::is_null(ws$panel_heights) |
      !rlang::is_null(panel_widths) |
      !rlang::is_null(panel_heights)
  ) {
    from_dim <- if (aspect == "x") {
      as.numeric(panel_widths)[1]
    } else {
      as.numeric(panel_heights)[1]
    }
    to_dim <- if (ws$aspect == "x") {
      as.numeric(ws$panel_widths)[1]
    } else {
      as.numeric(ws$panel_heights)[1]
    }
    scaling_factor <- to_dim / from_dim
    width <- width * scaling_factor
  }

  if (any(width >= 1)) {
    rlang::abort("width cannot be greater than or equal to 1")
  }

  return(width)
}
