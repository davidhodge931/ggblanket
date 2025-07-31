testthat::skip_if(getRversion() <= package_version("4.1.0"))
testthat::skip_on_os(c("mac", "linux"))

## ----setup------------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(forcats)
library(stringr)
library(ggplot2)
library(scales)
library(palmerpenguins)

set_blanket()

test_that("both axes", {
  p <- palmerpenguins::penguins |>
    tidyr::drop_na(sex) |>
    dplyr::mutate(dplyr::across(sex, \(x) stringr::str_to_sentence(x))) |>
    gg_blanket(
      x = flipper_length_mm,
      y = body_mass_g,
      col = sex,
    ) +
    annotate_panel_grid(
      x_breaks = c(180, 190, 200, 210, 220, 230),
      y_breaks = c(3000, 3500, 4000, 4500, 5000, 5500, 6000),
    ) +
    ggplot2::geom_point() +
    ggplot2::labs(title = "Both axes")

  vdiffr::expect_doppelganger("both_axes", p)
})

test_that("x only", {
  p <- palmerpenguins::penguins |>
    tidyr::drop_na(sex) |>
    dplyr::mutate(dplyr::across(sex, \(x) stringr::str_to_sentence(x))) |>
    gg_blanket(
      x = flipper_length_mm,
      y = body_mass_g,
      col = sex,
    ) +
    annotate_panel_grid(
      x_breaks = seq(180, 230, 10),
      colour = "red",
      linewidth = 0.3,
      linetype = "dashed"
    ) +
    ggplot2::geom_point() +
    ggplot2::labs(title = "X only (red, dashed)")

  vdiffr::expect_doppelganger("x_only", p)
})

test_that("y only", {
  p <- palmerpenguins::penguins |>
    tidyr::drop_na(sex) |>
    dplyr::mutate(dplyr::across(sex, \(x) stringr::str_to_sentence(x))) |>
    gg_blanket(
      x = flipper_length_mm,
      y = body_mass_g,
      col = sex,
    ) +
    annotate_panel_grid(
      y_breaks = seq(3000, 6000, 500),
      colour = "blue",
      linewidth = 0.8,
      linetype = "dotted"
    ) +
    ggplot2::geom_point() +
    ggplot2::labs(title = "Y only (blue, dotted)")

  vdiffr::expect_doppelganger("y_only", p)
})

test_that("histogram", {
  p <- palmerpenguins::penguins |>
    tidyr::drop_na(sex) |>
    dplyr::mutate(dplyr::across(sex, \(x) stringr::str_to_sentence(x))) |>
    gg_histogram(
      x = flipper_length_mm,
      col = sex,
    ) +
    annotate_panel_grid(
      x_breaks = c(185, 195, 205, 215, 225),
      y_breaks = c(5, 10, 15, 20, 25),
      colour = "gray70",
      linewidth = 0.4
    ) +
    ggplot2::labs(title = "Histogram")

  vdiffr::expect_doppelganger("histogram", p)
})

test_that("theme blank", {
  p <- palmerpenguins::penguins |>
    tidyr::drop_na(sex) |>
    dplyr::mutate(dplyr::across(sex, \(x) stringr::str_to_sentence(x))) |>
    gg_blanket(
      x = flipper_length_mm,
      y = body_mass_g,
      col = sex,
    ) +
    annotate_panel_grid(
      x_breaks = seq(180, 230, 15),
      y_breaks = seq(3000, 6000, 750),
      theme_elements = "blank"
    ) +
    ggplot2::geom_point() +
    ggplot2::labs(title = "Theme blank")

  vdiffr::expect_doppelganger("theme_blank", p)
})

test_that("no breaks", {
  p <- palmerpenguins::penguins |>
    tidyr::drop_na(sex) |>
    dplyr::mutate(dplyr::across(sex, \(x) stringr::str_to_sentence(x))) |>
    gg_blanket(
      x = flipper_length_mm,
      y = body_mass_g,
      col = sex,
    ) +
    annotate_panel_grid() +
    ggplot2::geom_point() +
    ggplot2::labs(title = "No breaks")

  vdiffr::expect_doppelganger("no_breaks", p)
})

test_that("faceted", {
  p <- palmerpenguins::penguins |>
    tidyr::drop_na(sex, species) |>
    dplyr::mutate(dplyr::across(sex, \(x) stringr::str_to_sentence(x))) |>
    gg_blanket(
      x = flipper_length_mm,
      y = body_mass_g,
      col = sex,
      facet = species,
    ) +
    annotate_panel_grid(
      x_breaks = c(190, 210, 230),
      y_breaks = c(3500, 4500, 5500),
      colour = "purple",
      linewidth = 0.5,
      linetype = "longdash"
    ) +
    ggplot2::geom_point() +
    ggplot2::labs(title = "Faceted")

  vdiffr::expect_doppelganger("faceted", p)
})

test_that("smooth", {
  p <- palmerpenguins::penguins |>
    tidyr::drop_na(sex) |>
    dplyr::mutate(dplyr::across(sex, \(x) stringr::str_to_sentence(x))) |>
    gg_blanket(
      x = flipper_length_mm,
      y = body_mass_g,
      col = sex,
    ) +
    annotate_panel_grid(
      x_breaks = seq(175, 235, 20),
      y_breaks = seq(2500, 6500, 1000),
      colour = "darkgreen",
      linewidth = 0.6
    ) +
    ggplot2::geom_smooth() +
    ggplot2::labs(title = "Smooth")

  vdiffr::expect_doppelganger("smooth", p)
})

test_that("keep theme", {
  p <- palmerpenguins::penguins |>
    tidyr::drop_na(sex) |>
    dplyr::mutate(dplyr::across(sex, \(x) stringr::str_to_sentence(x))) |>
    gg_blanket(
      x = flipper_length_mm,
      y = body_mass_g,
      col = sex,
    ) +
    annotate_panel_grid(
      x_breaks = c(185, 205, 225),
      y_breaks = c(3500, 4500, 5500),
      theme_elements = "keep",
      colour = "orange",
      linewidth = 1
    ) +
    ggplot2::geom_point() +
    ggplot2::labs(title = "Keep theme")

  vdiffr::expect_doppelganger("keep_theme", p)
})

test_that("boxplot", {
  p <- palmerpenguins::penguins |>
    tidyr::drop_na(sex) |>
    dplyr::mutate(dplyr::across(sex, \(x) stringr::str_to_sentence(x))) |>
    gg_blanket(
      x = species,
      y = body_mass_g,
      col = sex,
    ) +
    annotate_panel_grid(
      y_breaks = seq(3000, 6000, 500),
      colour = "gray50",
      linewidth = 0.3
    ) +
    ggplot2::geom_boxplot(bordered = TRUE) +
    ggplot2::labs(title = "Boxplot")

  vdiffr::expect_doppelganger("boxplot", p)
})
