#' Bind each to all
#'
#' @description
#' Bind data to support comparing each category to all of the data.
#'
#' @param data A data frame or tibble.
#' @param ... An unquoted variable.
#' @param name A variable name. Defaults to `each_or_all`.
#' @param each A string for the each value. Defaults to `"Each"`.
#' @param all A string for the all value. Defaults to `"All"`.
#' @param all_after A number for where the all value should be placed after. Use 0 for first or Inf for last. Defaults to Inf.
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
#'   bind_each_to_all(species) |>
#'   distinct(species, each_or_all)
#'
#' penguins |>
#'   bind_each_to_all(species) |>
#'   gg_jitter(
#'     x = species,
#'     y = body_mass_g,
#'   )
#'
#' penguins |>
#'   bind_each_to_all(species) |>
#'   gg_jitter(
#'     x = species,
#'     y = body_mass_g,
#'     col = each_or_all,
#'     col_palette = c(blue, grey),
#'   ) +
#'   theme(legend.position = "none")
#'
#' penguins |>
#'   bind_each_to_all(species) |>
#'   group_by(species, each_or_all) |>
#'   summarise(across(body_mass_g, \(x) mean(x, na.rm = TRUE))) |>
#'   gg_col(
#'     x = species,
#'     y = body_mass_g,
#'     col = each_or_all,
#'     col_palette = c(blue, grey),
#'     width = 0.5,
#'     y_label = "Average body mass g",
#'   ) +
#'   theme(legend.position = "none")
#'
#' penguins |>
#'   bind_each_to_all(species, all = "All\nspecies") |>
#'   gg_jitter(
#'     x = species,
#'     y = body_mass_g,
#'     col = each_or_all,
#'     col_palette = c(blue, grey),
#'     facet = each_or_all,
#'     facet_layout = "grid",
#'     facet_scales = "free_x",
#'     facet_space = "free_x",
#'   ) +
#'   theme(legend.position = "none") +
#'   theme(strip.text.x = element_blank()) +
#'   labs(x = NULL)
#'
bind_each_to_all <- function(data,
                        ...,
                        name = "each_or_all",
                        each = "Each",
                        all = "All",
                        all_after = Inf) {

  if (...length() != 1) stop("Please provide one variable")

  by <- rlang::enquos(...)[1][[1]]

  if (inherits(rlang::eval_tidy(by, data), what = c("factor"))) {
    levels <- levels(rlang::eval_tidy(by, data))

    data <- data |>
      dplyr::bind_rows(dplyr::mutate(data, !!by := all)) |>
      dplyr::mutate(!!by := factor(!!by, levels = c(levels, all))) |>
      dplyr::mutate(!!by := forcats::fct_relevel(!!by, all, after = all_after))
  }
  else {
      data <- data |>
      dplyr::mutate(!!by := as.character(!!by)) |>
      dplyr::bind_rows(dplyr::mutate(data, !!by := all)) |>
      dplyr::mutate(!!by := forcats::fct_relevel(!!by, all, after = all_after))
  }

  data <- data |>
    dplyr::mutate(!!name := ifelse(!!by == all, all, each)) |>
    dplyr::mutate(dplyr::across(!!name, forcats::fct_inorder))

  if (all_after == 0) {
    data <- data |>
      dplyr::mutate(dplyr::across(!!name, forcats::fct_rev))
  }

  return(data)
}
