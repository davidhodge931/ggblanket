# Visual regression tests for ggblanket package
library(testthat)
library(vdiffr)
library(palmerpenguins)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)

# Setup test data
penguins2 <- palmerpenguins::penguins |>
  labelled::set_variable_labels(
    bill_length_mm = "Bill length (mm)",
    bill_depth_mm = "Bill depth (mm)",
    flipper_length_mm = "Flipper length (mm)",
    body_mass_g = "Body mass (g)",
  ) |>
  mutate(sex = str_to_sentence(sex)) |>
  tidyr::drop_na(sex)

set_blanket()

# Basic point plots
test_that("point plots render correctly", {

  expect_doppelganger("point_basic", {
    palmerpenguins::penguins |>
      gg_point(
        x = flipper_length_mm,
        y = body_mass_g,
        col = species,
      )
  })

  expect_doppelganger("point_shape", {
    palmerpenguins::penguins |>
      gg_point(
        x = flipper_length_mm,
        y = body_mass_g,
        col = species,
        shape = species,
        x_title = NULL,
      )
  })

  expect_doppelganger("point_custom_title", {
    palmerpenguins::penguins |>
      gg_point(
        x = flipper_length_mm,
        y = body_mass_g,
        col = species,
        fill = species,
        x_title = snakecase::to_screaming_snake_case,
      )
  })

  expect_doppelganger("point_string_title", {
    palmerpenguins::penguins |>
      gg_point(
        x = flipper_length_mm,
        y = body_mass_g,
        col = species,
        fill = species,
        x_title = "Blah",
      )
  })

  expect_doppelganger("point_expr_title", {
    palmerpenguins::penguins |>
      gg_point(
        x = flipper_length_mm,
        y = body_mass_g,
        col = species,
        fill = species,
        x_title = expression(beta[0]),
      )
  })
})

# Histogram tests
test_that("histograms render correctly", {

  expect_doppelganger("hist_density_labeled", {
    penguins2 |>
      gg_histogram(
        mapping = aes(x = flipper_length_mm, y = after_stat(density)),
        col = species,
      )
  })
})

# Boxplot tests
test_that("boxplots render correctly", {

  expect_doppelganger("boxplot_labeled", {
    penguins2 |>
      tidyr::drop_na(sex) |>
      gg_boxplot(
        x = species,
        y = body_mass_g,
        col = island,
        position = position_dodge2(preserve = "single"),
      )
  })
})

# Computed variable tests (these should show proper titles, not "Col")
test_that("computed variable titles render correctly", {

  expect_doppelganger("hex_count", {
    penguins2 |>
      gg_hex(
        x = flipper_length_mm,
        y = body_mass_g,
      )
  })

  expect_doppelganger("density2d_level", {
    penguins2 |>
      gg_density2d_filled(
        x = flipper_length_mm,
        y = body_mass_g,
      )
  })
})

# Label priority tests
test_that("label priorities work correctly", {

  # Test labeled data uses labels
  expect_doppelganger("labeled_data", {
    penguins2 |>
      gg_point(
        x = flipper_length_mm,
        y = body_mass_g,
        col = species,
      )
  })

  # Test function override
  expect_doppelganger("function_override", {
    penguins2 |>
      gg_point(
        x = flipper_length_mm,
        y = body_mass_g,
        col = species,
        x_title = toupper,
        y_title = tolower,
      )
  })

  # Test NULL removes titles
  expect_doppelganger("null_titles", {
    penguins2 |>
      gg_point(
        x = flipper_length_mm,
        y = body_mass_g,
        col = species,
        x_title = NULL,
        y_title = NULL,
        col_title = NULL,
      )
  })
})
