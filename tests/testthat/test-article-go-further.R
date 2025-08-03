testthat::skip_if(getRversion() <= package_version("4.1.0"))
testthat::skip_on_os(c("mac", "linux"))

## ----setup------------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(forcats)
library(stringr)
library(ggplot2)
library(scales)
library(ggblanket)
library(patchwork)
library(palmerpenguins)

set_blanket()

test_that("stat changes work", {
  palmerpenguins::penguins |>
    gg_pointrange(
      stat = "summary",
      x = species,
      y = flipper_length_mm,
    )
})

test_that("reordering works", {
  diamonds |>
    count(color) |>
    gg_col(
      x = n,
      y = color,
      width = 0.75,
      x_labels = \(x) x / 1000,
      x_title = "Count (thousands)",
    )

  diamonds |>
    count(color) |>
    mutate(color = fct_rev(fct_reorder(color, n))) |>
    gg_col(
      x = n,
      y = color,
      width = 0.75,
      x_labels = \(x) x / 1000,
      x_title = "Count (thousands)",
    )
})

test_that("dropping unused levels works", {
  diamonds |>
    count(color) |>
    filter(color %in% c("E", "G", "I")) |>
    gg_col(
      x = n,
      y = color,
      width = 0.75,
      x_labels = \(x) x / 1000,
      x_title = "Count (thousands)",
    )

  diamonds |>
    count(color) |>
    filter(color %in% c("E", "G", "I")) |>
    mutate(color = forcats::fct_drop(color)) |>
    gg_col(
      x = n,
      y = color,
      width = 0.75,
      x_labels = \(x) x / 1000,
      x_title = "Count (thousands)",
    )
})

test_that("transforms work", {
  pressure |>
    gg_point(
      x = temperature,
      y = pressure,
      x_breaks_n = 4,
      y_breaks_n = 4,
    )

  pressure |>
    gg_point(
      x = temperature,
      y = pressure,
      x_breaks_n = 4,
      y_breaks_n = 4,
      y_transform = "reverse",
    )

  pressure |>
    gg_point(
      x = temperature,
      y = pressure,
      x_breaks_n = 4,
      y_breaks_n = 4,
      y_transform = "sqrt",
    )

  pressure |>
    gg_point(
      x = temperature,
      y = pressure,
      x_breaks_n = 4,
      y_breaks_n = 4,
      y_transform = c("sqrt", "reverse"),
    )
})

test_that("aspect works", {
  palmerpenguins::penguins |>
    gg_point(
      x = bill_depth_mm,
      y = bill_length_mm,
    )

  palmerpenguins::penguins |>
    gg_point(
      x = bill_depth_mm,
      y = bill_length_mm,
      aspect = "y",
    )
})

test_that("symmetric scales work", {
  palmerpenguins::penguins |>
    gg_pointrange(
      x = sex,
      y = bill_length_mm,
      stat = "summary",
      position = position_dodge(),
      x_labels = \(x) str_sub(x, 1, 1),
    )

  palmerpenguins::penguins |>
    gg_pointrange(
      x = sex,
      y = bill_length_mm,
      stat = "summary",
      position = position_dodge(),
      x_labels = \(x) str_sub(x, 1, 1),
      y_symmetric = FALSE,
    )

  palmerpenguins::penguins |>
    gg_col(
      x = sex,
      y = bill_length_mm,
      stat = "summary",
      position = position_dodge(),
      width = 0.5,
      x_labels = \(x) str_sub(x, 1, 1),
      y_symmetric = FALSE,
    )
})

test_that("position changes work", {
  economics |>
    gg_line(
      x = date,
      y = unemploy,
      col = date,
      y_position = "right",
      x_position = "top",
      caption = "",
      title = "Unemployment",
      subtitle = "1967\u20132015",
    )
})

test_that("zooming works", {
  penguins |>
    gg_smooth(
      x = body_mass_g,
      y = bill_depth_mm,
      x_limits_include = c(0),
      y_limits_include = c(10, 25),
      se = TRUE,
    )

  penguins |>
    filter(bill_depth_mm < 15) |>
    gg_point(
      x = bill_depth_mm,
      y = body_mass_g,
    )

  penguins |>
    gg_smooth(
      x = body_mass_g,
      y = bill_depth_mm,
      coord = coord_cartesian(ylim = c(14.8, 15)),
      y_breaks = scales::breaks_width(0.05),
      se = TRUE,
    )
})

test_that("after_stat works", {
  palmerpenguins::penguins |>
    gg_histogram(
      x = flipper_length_mm,
      mapping = aes(y = after_stat(density)),
      facet = species,
    )

  faithfuld |>
    gg_contour(
      x = waiting,
      y = eruptions,
      z = density,
      mapping = aes(colour = after_stat(level)),
      bins = 8,
    )
})

test_that("col_rescale works", {
  rescale_vctr <- sort(c(range(mpg$cty), 15))

  mpg |>
    gg_point(
      x = displ,
      y = hwy,
      col = cty,
      col_rescale = scales::rescale(rescale_vctr),
      col_breaks = scales::breaks_width(5),
      col_palette = c(jumble[c(3, 1)], "white", jumble[c(2, 4)]),
    )
})

test_that("legend positioning works", {
  palmerpenguins::penguins |>
    gg_histogram(
      x = flipper_length_mm,
      col = species,
    ) +
    theme(legend.position = "inside") +
    theme(legend.position.inside = c(1, 0.975)) +
    theme(legend.justification = c(1, 1))
})

test_that("panel sizes work", {

  penguins |>
    gg_point(
      x = bill_length_mm,
      y = body_mass_g,
      col = species,
      size = 1,
    )

  penguins |>
    drop_na(sex) |>
    mutate(across(sex, \(x) str_to_sentence(x))) |>
    gg_point(
      x = bill_length_mm,
      y = body_mass_g,
      col = species,
      facet = sex,
      size = 1,
    )
})

test_that("standardise_width works", {
  set_blanket(
    theme = theme_lighter(
      panel_heights = rep(unit(50, "mm"), times = 100),
      panel_widths = rep(unit(75, "mm"), times = 100),
    ),
  )

  penguins |>
    drop_na(sex) |>
    mutate(across(sex, \(x) str_to_sentence(x))) |>
    gg_bar(
      x = species,
      col = sex,
      width = 0.25,
    )

  penguins |>
    drop_na(sex) |>
    mutate(across(sex, \(x) str_to_sentence(x))) |>
    gg_bar(
      x = sex,
      col = species,
      width = standardise_width(from_n = 2, to_width = 0.25, to_n = 3)
    )

  penguins |>
    drop_na(sex) |>
    mutate(across(sex, \(x) str_to_sentence(x))) |>
    gg_bar(
      x = sex,
      col = species,
      position = position_dodge(),
      width = standardise_width(from_n = 2, from_dodge_n = 3, to_width = 0.25, to_n = 3)
    )

  penguins |>
    drop_na(sex) |>
    mutate(across(sex, \(x) str_to_sentence(x))) |>
    gg_bar(
      y = sex,
      col = species,
      position = position_dodge(),
      width = standardise_width(
        from_n = 2,
        from_dodge_n = 3,
        from_aspect = "y",
        to_width = 0.25,
        to_n = 3
      )
    )
})

test_that("annotate_axis_line works", {
  set_blanket(
    theme = theme_lighter(
      panel_heights = rep(unit(50, "mm"), times = 100),
      panel_widths = rep(unit(75, "mm"), times = 100),
    ),
  )

  penguins |>
    tidyr::drop_na(sex) |>
    mutate(across(sex, \(x) str_to_sentence(x))) |>
    add_row(flipper_length_mm = 195, body_mass_g = 2500, sex = "Female") |>
    gg_blanket(
      x = flipper_length_mm,
      y = body_mass_g,
      col = sex,
    ) +
    annotate_axis_line(x_position = "bottom") +
    annotate_axis_ticks(x_breaks = 210, x_position = "bottom")
    geom_point()
})

test_that("annotate_axis_ticks works", {
  set_blanket(
    theme = theme_lighter(
      panel_heights = rep(unit(50, "mm"), times = 100),
      panel_widths = rep(unit(75, "mm"), times = 100),
    ),
  )

  penguins |>
    tidyr::drop_na(sex) |>
    mutate(across(sex, \(x) str_to_sentence(x))) |>
    gg_histogram(
      x = flipper_length_mm,
      col = sex,
    ) +
    annotate_axis_ticks(x_breaks = c(seq(170, 230, 10), 195))
})

set_blanket()
