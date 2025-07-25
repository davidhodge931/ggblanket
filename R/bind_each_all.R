#' Bind each all.
#'
#' @description
#' Binds data to support plotting each category _and_ all combined data.
#'
#' @param data A data frame or tibble.
#' @param ... An unquoted variable.
#' @param name A variable name. Defaults to `each_all`.
#' @param each A string for the each value. Defaults to `"Each"`.
#' @param all A string for the all value. Defaults to `"All"`.
#' @param all_after A number for where the all value should be placed after. Use `0` for first or `Inf` for last. Defaults to `Inf`.
#'
#' @return A data frame or tibble
#' @export
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' set_blanket()
#'
#' palmerpenguins::penguins |>
#'   count(species)
#'
#' palmerpenguins::penguins |>
#'   bind_each_all(species) |>
#'   count(species, each_all)
#'
#' palmerpenguins::penguins |>
#'   bind_each_all(species) |>
#'   gg_jitter(
#'     x = species,
#'     y = body_mass_g,
#'   )
#'
#' palmerpenguins::penguins |>
#'   bind_each_all(species) |>
#'   gg_jitter(
#'     x = species,
#'     y = body_mass_g,
#'     col = each_all,
#'     col_palette = jumble[c(11, 10)],
#'   ) +
#'   theme(legend.position = "none")
#'
#' palmerpenguins::penguins |>
#'   bind_each_all(species) |>
#'   group_by(species, each_all) |>
#'   summarise(across(body_mass_g, \(x) mean(x, na.rm = TRUE))) |>
#'   gg_col(
#'     x = species,
#'     y = body_mass_g,
#'     col = each_all,
#'     col_palette = jumble[c(11, 10)],
#'     width = 0.5,
#'     y_title = "Average body mass g",
#'   ) +
#'   theme(legend.position = "none")
#'
#' palmerpenguins::penguins |>
#'   bind_each_all(species, all = "All\nspecies") |>
#'   gg_jitter(
#'     x = species,
#'     y = body_mass_g,
#'     col = each_all,
#'     col_palette = jumble[c(11, 10)],
#'     facet = each_all,
#'     facet_scales = "free_x",
#'     facet_space = "free_x",
#'   ) +
#'   theme(legend.position = "none") +
#'   theme(strip.text.x = element_blank()) +
#'   labs(x = NULL)
#'
bind_each_all <- function(
  data,
  ...,
  name = "each_all",
  each = "Each",
  all = "All",
  all_after = Inf
) {
  if (...length() != 1) {
    stop("Please provide one variable")
  }
  by <- rlang::enquos(...)[1][[1]]
  if (inherits(rlang::eval_tidy(by, data), what = c("factor"))) {
    levels <- levels(rlang::eval_tidy(by, data))
    data <- data |>
      dplyr::bind_rows(dplyr::mutate(data, !!by := all)) |>
      dplyr::mutate(!!by := factor(!!by, levels = c(levels, all))) |>
      dplyr::mutate(
        !!by := {
          col_values <- !!by
          if (any(is.na(col_values))) {
            forcats::fct_relevel(
              forcats::fct_na_value_to_level(col_values),
              all,
              after = all_after
            )
          } else {
            forcats::fct_relevel(col_values, all, after = all_after)
          }
        }
      )
  } else {
    data <- data |>
      dplyr::mutate(!!by := as.character(!!by)) |>
      dplyr::bind_rows(dplyr::mutate(data, !!by := all)) |>
      dplyr::mutate(
        !!by := {
          col_values <- !!by
          if (any(is.na(col_values))) {
            forcats::fct_relevel(
              forcats::fct_na_value_to_level(col_values),
              all,
              after = all_after
            )
          } else {
            forcats::fct_relevel(as.factor(col_values), all, after = all_after)
          }
        }
      )
  }
  data <- data |>
    dplyr::mutate(
      !!name := dplyr::if_else(!!by == all, all, each, missing = each)
    ) |>
    dplyr::mutate(dplyr::across(!!name, forcats::fct_inorder))
  if (all_after == 0) {
    data <- data |>
      dplyr::mutate(dplyr::across(!!name, forcats::fct_rev))
  }
  return(data)
}
