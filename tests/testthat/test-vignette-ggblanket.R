testthat::skip_if(getRversion() <= package_version("4.1.0"))
testthat::skip_on_os(c("mac", "linux"))

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
library(ggblanket)
library(palmerpenguins)
library(patchwork)

set_blanket()

test_that("setup works", {
  set_blanket()
})

test_that("basic point plot works", {
  palmerpenguins::penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
    )
})

test_that("col aesthetic works", {
  palmerpenguins::penguins |>
    drop_na(sex) |>
    gg_violin(
      x = flipper_length_mm,
      y = island,
      col = sex,
    )
})

test_that("facet works", {
  palmerpenguins::penguins |>
    gg_histogram(
      x = flipper_length_mm,
      facet = species,
    )
})

test_that("facet2 works", {
  palmerpenguins::penguins |>
    gg_histogram(
      x = flipper_length_mm,
      facet = species,
      facet2 = sex,
    )
})

test_that("other aesthetics work", {
  palmerpenguins::penguins |>
    drop_na(sex) |>
    gg_jitter(
      x = species,
      y = flipper_length_mm,
      col = island,
      shape = island,
    )
})

test_that("prefixed arguments work", {
  palmerpenguins::penguins |>
    gg_jitter(
      x = flipper_length_mm,
      y = body_mass_g,
      col = bill_length_mm,
      x_breaks_n = 4,
      x_title = "Flipper length",
      x_labels = \(x) paste0(x, " mm"),
      y_limits_include = 1000,
      y_labels = label_number(big.mark = " "),
      y_transform = "sqrt",
      col_title = "Bill\nlength (mm)",
      col_breaks = \(x) quantile(x, seq(0, 1, 0.25)),
    )
})

test_that("smart titles work", {
  palmerpenguins::penguins |>
    gg_freqpoly(
      x = flipper_length_mm,
      col = species,
    )
})

test_that("geom arguments work", {
  palmerpenguins::penguins |>
    gg_smooth(
      x = flipper_length_mm,
      y = body_mass_g,
      level = 0.999,
    )
})

test_that("theme_darker works", {
  set_blanket(theme = theme_darker())

  palmerpenguins::penguins |>
    gg_histogram(
      x = flipper_length_mm,
      col = species,
      title = "Penguin flipper length by species",
      subtitle = "Palmer Archipelago, Antarctica",
      caption = "Source: Gorman, 2020",
    )

  set_blanket()
})

test_that("aspect side-effects work", {
  palmerpenguins::penguins |>
    drop_na() |>
    gg_jitter(
      x = sex,
      y = bill_depth_mm,
      col = sex,
    )

  palmerpenguins::penguins |>
    drop_na() |>
    gg_jitter(
      x = bill_depth_mm,
      y = sex,
      col = sex,
    )
})

test_that("multiple layers work", {
  palmerpenguins::penguins |>
    gg_violin(
      x = species,
      y = bill_depth_mm,
      outliers = FALSE,
    ) +
    geom_jitter(shape = 19)
})

test_that("complex multiple layers work", {
  palmerpenguins::penguins |>
    drop_na(sex) |>
    group_by(species, sex) |>
    summarise(
      lower = quantile(bill_depth_mm, probs = 0.05),
      upper = quantile(bill_depth_mm, probs = 0.95),
      bill_depth_mm = mean(bill_depth_mm, na.rm = TRUE),
    ) |>
    labelled::copy_labels_from(palmerpenguins::penguins) |>
    gg_blanket(
      y = species,
      x = bill_depth_mm,
      xmin = lower,
      xmax = upper,
      col = sex,
      x_limits_include = 0,
    ) +
    geom_col(
      width = 0.75,
      position = position_dodge(),
    ) +
    geom_errorbar(
      width = 0.1,
      position = position_dodge(width = 0.75),
    )
})

test_that("customised set_blanket works", {
  set_blanket(
    theme = theme_lighter(
      base_size = 9,
      legend_position = "top",
      axis_line_linewidth = 0.25,
      axis_line_colour = "#78909C",
      panel_grid_colour = "#C8D7DF",
      panel_background_fill = "#E8EFF2",
      panel_grid_linewidth = 0.25,
    ),
    col = "tan",
    col_palette_d = c("#003f5c", "#bc5090", "#ffa600", "#357BA2"),
  )

  palmerpenguins::penguins |>
    gg_point(
      x = bill_depth_mm,
      y = flipper_length_mm,
    )

  palmerpenguins::penguins |>
    gg_bar(
      y = sex,
      col = species,
      position = "dodge",
    )

  set_blanket()
})

test_that("bordered style works", {
  set_blanket(
    theme = theme_lighter(),
    bordered_colour = \(x) scales::col_darker(x, amount = 40),
    bordered_linewidth = 0.5,
  )

  palmerpenguins::penguins |>
    gg_violin(
      x = species,
      y = body_mass_g,
      col = sex,
    )

  set_blanket()
})

test_that("gg_blanket with custom geom works", {
  expand.grid(x = 1:10, y = 1:10) |>
    tibble() |>
    mutate(angle = runif(100, 0, 2 * pi)) |>
    mutate(speed = runif(100, 0, sqrt(0.1 * x))) |>
    gg_blanket(
      geom = "spoke",
      x = x,
      y = y,
      col = speed,
      mapping = aes(angle = angle, radius = speed),
    ) +
    geom_point()
})

set_blanket()
