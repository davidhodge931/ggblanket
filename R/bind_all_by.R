#' Bind all the data to itself
#'
#' @description
#' Bind all the data to itself by a variable for plotting of groups against all data.
#'
#' @param data A data frame or tibble.
#' @param by An unquoted character or factor variable.
#' @param ... Provided to require argument naming, support trailing commas etc.
#' @param all A character value for the all value. Defaults to `"All"`.
#' @param all_after A number for where the all value should be placed after. Either 0 for first or Inf for last.
#' @param groups A character value for the group value. Defaults to `"Groups"`.
#' @param name A variable name. Defaults to `groups_or_all`.
#'
#' @return A data frame or tibble
#' @export
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' library(palmerpenguins)
#'
#' set_blanket()
#'
#' penguins |>
#'   bind_all_by(species) |>
#'   distinct(species, groups_or_all)
#'
#' penguins |>
#'   bind_all_by(species) |>
#'   gg_jitter(
#'     x = species,
#'     y = body_mass_g,
#'   )
#'
#' penguins |>
#'   bind_all_by(species) |>
#'   gg_jitter(
#'     x = species,
#'     y = body_mass_g,
#'     col = groups_or_all,
#'     col_palette = c(blue, grey),
#'   ) +
#'   theme(legend.position = "none")
#'
#' penguins |>
#'   bind_all_by(species) |>
#'   group_by(species, groups_or_all) |>
#'   summarise(across(body_mass_g, \(x) mean(x, na.rm = TRUE))) |>
#'   gg_col(
#'     x = species,
#'     y = body_mass_g,
#'     col = groups_or_all,
#'     col_palette = c(blue, grey),
#'     width = 0.5,
#'     y_label = "Average body mass g",
#'   ) +
#'   theme(legend.position = "none")
#'
#' penguins |>
#'   bind_all_by(species, all = "All\nspecies") |>
#'   gg_jitter(
#'     x = species,
#'     y = body_mass_g,
#'     col = groups_or_all,
#'     col_palette = c(blue, grey),
#'     facet = groups_or_all,
#'     facet_layout = "grid",
#'     facet_scales = "free_x",
#'     facet_space = "free_x",
#'   ) +
#'   theme(legend.position = "none") +
#'   theme(strip.text.x = element_blank()) +
#'   labs(x = NULL)
#'
bind_all_by <- function(data,
                        by,
                        ...,
                        all = "All",
                        all_after = Inf,
                        groups = "Groups",
                        name = "groups_or_all") {

  by <- rlang::enquo(by)

  if (inherits(rlang::eval_tidy({{ by }}, data), what = c("character"))) {
    data <- data |>
      dplyr::bind_rows(dplyr::mutate(data, !!by := all)) |>
      dplyr::mutate(!!by := forcats::fct_relevel(!!by, all, after = all_after))
  }
  else if (inherits(rlang::eval_tidy(by, data), what = c("factor"))) {
    levels <- levels(rlang::eval_tidy(by, data))

    data <- data |>
      dplyr::bind_rows(dplyr::mutate(data, !!by := all)) |>
      dplyr::mutate(!!by := factor(!!by, levels = c(levels, all))) |>
      dplyr::mutate(!!by := forcats::fct_relevel(!!by, all, after = all_after))
  }

  data <- data |>
    dplyr::mutate(!!name := ifelse(!!by == all, all, groups)) |>
    dplyr::mutate(dplyr::across(!!name, forcats::fct_inorder))

  if (all_after == 0) {
    data <- data |>
      dplyr::mutate(dplyr::across(!!name, forcats::fct_rev))
  }

  return(data)
}
