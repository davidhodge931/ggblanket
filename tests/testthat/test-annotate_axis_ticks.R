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

# Setup
set_blanket(
  theme = theme_lighter(
    panel_heights = rep(unit(50, "mm"), 100),
    panel_widths = rep(unit(75, "mm"), 100),
  ),
)

test_that("both axes", {
  p <- palmerpenguins::penguins |>
    tidyr::drop_na(sex) |>
    dplyr::mutate(dplyr::across(sex, \(x) stringr::str_to_sentence(x))) |>
    gg_blanket(
      x = flipper_length_mm,
      y = body_mass_g,
      col = sex,
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    annotate_axis_ticks(
      x_breaks = c(185, 195, 205, 215, 225),
      y_breaks = c(3500, 4500, 5500),
    ) +
    ggplot2::geom_point() +
    ggplot2::labs(title = "Both axes")

  vdiffr::expect_doppelganger("axis_both_axes", p)
})

test_that("x only bottom", {
  p <- palmerpenguins::penguins |>
    tidyr::drop_na(sex) |>
    dplyr::mutate(dplyr::across(sex, \(x) stringr::str_to_sentence(x))) |>
    gg_blanket(
      x = flipper_length_mm,
      y = body_mass_g,
      col = sex,
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    annotate_axis_ticks(
      x_breaks = c(180, 200, 220),
      x_position = "bottom",
      colour = "red",
      linewidth = 1,
      length = grid::unit(20, "pt")
    ) +
    ggplot2::geom_point() +
    ggplot2::labs(title = "X bottom (red, thick, long)")

  vdiffr::expect_doppelganger("axis_x_bottom", p)
})

test_that("x only top", {
  p <- palmerpenguins::penguins |>
    tidyr::drop_na(sex) |>
    dplyr::mutate(dplyr::across(sex, \(x) stringr::str_to_sentence(x))) |>
    gg_blanket(
      x = flipper_length_mm,
      y = body_mass_g,
      col = sex,
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    annotate_axis_ticks(
      x_breaks = c(190, 210, 230),
      x_position = "top",
      colour = "blue",
      linewidth = 0.3
    ) +
    ggplot2::geom_point() +
    ggplot2::labs(title = "X top (blue, thin)")

  vdiffr::expect_doppelganger("axis_x_top", p)
})

test_that("y only left", {
  p <- palmerpenguins::penguins |>
    tidyr::drop_na(sex) |>
    dplyr::mutate(dplyr::across(sex, \(x) stringr::str_to_sentence(x))) |>
    gg_blanket(
      x = flipper_length_mm,
      y = body_mass_g,
      col = sex,
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    annotate_axis_ticks(
      y_breaks = c(3000, 4000, 5000, 6000),
      y_position = "left",
      colour = "darkgreen"
    ) +
    ggplot2::geom_point() +
    ggplot2::labs(title = "Y left (green)")

  vdiffr::expect_doppelganger("axis_y_left", p)
})

test_that("y only right", {
  p <- palmerpenguins::penguins |>
    tidyr::drop_na(sex) |>
    dplyr::mutate(dplyr::across(sex, \(x) stringr::str_to_sentence(x))) |>
    gg_blanket(
      x = flipper_length_mm,
      y = body_mass_g,
      col = sex,
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    annotate_axis_ticks(
      y_breaks = c(3500, 4500, 5500),
      y_position = "right",
      colour = "purple",
      length = grid::unit(15, "pt")
    ) +
    ggplot2::geom_point() +
    ggplot2::labs(title = "Y right (purple, long)")

  vdiffr::expect_doppelganger("axis_y_right", p)
})

test_that("histogram", {
  p <- palmerpenguins::penguins |>
    tidyr::drop_na(sex) |>
    dplyr::mutate(dplyr::across(sex, \(x) stringr::str_to_sentence(x))) |>
    gg_histogram(
      x = flipper_length_mm,
      col = sex,
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    annotate_axis_ticks(
      x_breaks = c(185, 195, 205, 215, 225),
      y_breaks = c(10, 20, 30),
      colour = "gray50"
    ) +
    ggplot2::labs(title = "Histogram")

  vdiffr::expect_doppelganger("axis_histogram", p)
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
    ggplot2::coord_cartesian(clip = "off") +
    annotate_axis_ticks(
      x_breaks = c(180, 200, 220),
      y_breaks = c(3500, 4500, 5500),
      theme_elements = "blank"
    ) +
    ggplot2::geom_point() +
    ggplot2::labs(title = "Theme blank")

  vdiffr::expect_doppelganger("axis_theme_blank", p)
})

test_that("theme keep", {
  p <- palmerpenguins::penguins |>
    tidyr::drop_na(sex) |>
    dplyr::mutate(dplyr::across(sex, \(x) stringr::str_to_sentence(x))) |>
    gg_blanket(
      x = flipper_length_mm,
      y = body_mass_g,
      col = sex,
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    annotate_axis_ticks(
      x_breaks = c(190, 210),
      y_breaks = c(4000, 5000),
      theme_elements = "keep",
      colour = "orange",
      linewidth = 1.5
    ) +
    ggplot2::geom_point() +
    ggplot2::labs(title = "Theme keep (orange, thick)")

  vdiffr::expect_doppelganger("axis_theme_keep", p)
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
    ggplot2::coord_cartesian(clip = "off") +
    annotate_axis_ticks(
      x_breaks = c(190, 210, 230),
      y_breaks = c(3500, 4500, 5500),
      colour = "darkred"
    ) +
    ggplot2::geom_point() +
    ggplot2::labs(title = "Faceted")

  vdiffr::expect_doppelganger("axis_faceted", p)
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
    ggplot2::coord_cartesian(clip = "off") +
    annotate_axis_ticks(
      y_breaks = c(3000, 4000, 5000, 6000),
      colour = "gray30",
      length = grid::unit(12, "pt")
    ) +
    ggplot2::geom_boxplot(bordered = TRUE) +
    ggplot2::labs(title = "Boxplot")

  vdiffr::expect_doppelganger("axis_boxplot", p)
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
    ggplot2::coord_cartesian(clip = "off") +
    annotate_axis_ticks() +
    ggplot2::geom_point() +
    ggplot2::labs(title = "No breaks")

  vdiffr::expect_doppelganger("axis_no_breaks", p)
})
