#' Bind all the data to itself.
#'
#' @description
#' Bind all the data to itself by a variable for plotting of groups against all data.
#'
#' @param data A data frame or tibble.
#' @param ... Provided to require argument naming, support trailing commas etc.
#' @param by An unquoted variable.
#' @param all_value A character value for the all value. Defaults to "All".
#' @param all_value_after A number for where the all value should be placed. Either 0 for first or Inf for last.
#' @param group_value A character value for the group value. Defaults to "Groups".
#' @param name A variable name. Defaults to `all_or_groups`.
#'
#' @return A data frame or tibble
#' @export
#'
#' @examples
#' library(dplyr)
#' library(stringr)
#' library(tidyr)
#' library(ggplot2)
#' library(palmerpenguins)
#'
#' penguins |>
#'   bind_all(by = species) |>
#'   distinct(species, all_or_groups)
#'
#' penguins |>
#'   bind_all(by = species,
#'            all_value = "All\nspecies",
#'            group_value = "Species",
#'            ) |>
#'   distinct(species, all_or_groups)
#'
#' set.seed(123)
#'
#' penguins |>
#'   bind_all(by = species, all_value = "All\nspecies", group_value = "Species") |>
#'   gg_jitter(
#'     x = species,
#'     y = body_mass_g,
#'     col = all_or_groups,
#'     col_palette = c(blue, grey),
#'   ) +
#'   theme(legend.position = "none") +
#'   labs(x = NULL)
#'
#' set.seed(123)
#'
#' penguins |>
#'   bind_all(by = species, all_value = "All\nspecies", group_value = "Species") |>
#'   gg_jitter(
#'     x = species,
#'     y = body_mass_g,
#'     col = all_or_groups,
#'     facet = all_or_groups,
#'     col_palette = c(blue, grey),
#'     facet_layout = "grid",
#'     facet_space = "free_x",
#'     facet_scales = "free_x",
#'   ) +
#'   theme(legend.position = "none") +
#'   theme(strip.text = element_blank()) +
#'   labs(x = NULL)
#'
#' set.seed(123)
#'
#' penguins |>
#'   bind_all(by = species, all_value = "All\nspecies", group_value = "Species") |>
#'   drop_na(sex) |>
#'   mutate(sex = str_to_sentence(sex)) |>
#'   gg_violin(
#'     x = species,
#'     y = body_mass_g,
#'     facet = all_or_groups,
#'     col = all_or_groups,
#'     col_palette = c(blue, grey),
#'     facet_layout = "grid",
#'     facet_space = "free_x",
#'     facet_scales = "free_x",
#'   ) +
#'   theme(strip.text = element_blank()) +
#'   theme(legend.position = "none") +
#'   labs(x = NULL)
#'
#' set.seed(123)
#'
#' penguins |>
#'   drop_na(sex) |>
#'   mutate(sex = str_to_sentence(sex)) |>
#'   bind_all(by = species, all_value = "All\nspecies", group_value = "Species") |>
#'   gg_violin(
#'     x = species,
#'     y = body_mass_g,
#'     col = all_or_groups,
#'     facet = sex,
#'     col_palette = c(blue, grey),
#'   ) +
#'   theme(legend.position = "none") +
#'   labs(x = NULL)
#'
bind_all <- function(data,
                     ...,
                     by,
                     all_value = "All",
                     all_value_after = Inf,
                     group_value = "Groups",
                     name = "all_or_groups") {

  by <- rlang::enquo(by)

  if (inherits(rlang::eval_tidy({{ by }}, data), what = c("character"))) {
    data <- data |>
      dplyr::bind_rows(dplyr::mutate(data, !!by := all_value)) |>
      dplyr::mutate(!!by := forcats::fct_relevel(!!by, all_value, after = all_value_after))
  }
  else if (inherits(rlang::eval_tidy(by, data), what = c("factor"))) {
    levels <- levels(rlang::eval_tidy(by, data))

    data <- data |>
      dplyr::bind_rows(dplyr::mutate(data, !!by := all_value)) |>
      dplyr::mutate(!!by := factor(!!by, levels = c(levels, all_value))) |>
      dplyr::mutate(!!by := forcats::fct_relevel(!!by, all_value, after = all_value_after))
  }

  data <- data |>
    dplyr::mutate(!!name := ifelse(!!by == all_value, all_value, group_value)) |>
    dplyr::mutate(dplyr::across(!!name, forcats::fct_inorder))

  if (all_value_after == 0) {
    data <- data |>
      dplyr::mutate(dplyr::across(!!name, forcats::fct_rev))
  }

  return(data)
}

#' library(dplyr)
#' library(ggplot2)
#' library(palmerpenguins)
#'
#' penguins |>
#'   bind_all(by = species) |>
#'   distinct(species, all_or_groups)
#'
#' penguins |>
#'   bind_all(by = species,
#'            all_value = "All\nspecies",
#'            group_value = "Species",
#'            ) |>
#'   distinct(species, all_or_groups)
#'
#' set.seed(123)
#'
#' penguins |>
#'   bind_all(by = species, all_value = "All\nspecies", group_value = "Species") |>
#'   gg_jitter(
#'     x = species,
#'     y = body_mass_g,
#'     col = all_or_groups,
#'     col_palette = c(blue, grey),
#'   ) +
#'   theme(legend.position = "none") +
#'   labs(x = NULL)
#'
#' set.seed(123)
#'
#' penguins |>
#'   bind_all(by = species, all_value = "All\nspecies", group_value = "Species") |>
#'   gg_jitter(
#'     x = species,
#'     y = body_mass_g,
#'     col = all_or_groups,
#'     facet = all_or_groups,
#'     col_palette = c(blue, grey),
#'     facet_layout = "grid",
#'     facet_space = "free_x",
#'     facet_scales = "free_x",
#'   ) +
#'   theme(legend.position = "none") +
#'   theme(strip.text = element_blank()) +
#'   labs(x = NULL)
#'
#' set.seed(123)
#'
#' penguins |>
#'   bind_all(by = species, all_value = "All\nspecies", group_value = "Species") |>
#'   drop_na(sex) |>
#'   mutate(sex = str_to_sentence(sex)) |>
#'   gg_violin(
#'     x = species,
#'     y = body_mass_g,
#'     facet = all_or_groups,
#'     col = all_or_groups,
#'     col_palette = c(blue, grey),
#'     facet_layout = "grid",
#'     facet_space = "free_x",
#'     facet_scales = "free_x",
#'   ) +
#'   theme(strip.text = element_blank()) +
#'   theme(legend.position = "none") +
#'   labs(x = NULL)
#'
#' set.seed(123)
#'
#' penguins |>
#'   drop_na(sex) |>
#'   mutate(sex = str_to_sentence(sex)) |>
#'   bind_all(by = species, all_value = "All\nspecies", group_value = "Species") |>
#'   gg_violin(
#'     x = species,
#'     y = body_mass_g,
#'     col = all_or_groups,
#'     facet = sex,
#'     col_palette = c(blue, grey),
#'   ) +
#'   theme(legend.position = "none") +
#'   labs(x = NULL)

