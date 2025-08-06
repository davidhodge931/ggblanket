library(testthat)
library(ggplot2)
library(vdiffr)

test_that("gg_point basic plot", {
  p <- palmerpenguins::penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
    )

  expect_doppelganger("gg_point_basic", p)
})

test_that("gg_point with aspect x", {
  p <- palmerpenguins::penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
      aspect = "x"
    )

  expect_doppelganger("gg_point_aspect_x", p)
})

test_that("gg_point with aspect y", {
  p <- palmerpenguins::penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
      aspect = "y"
    )

  expect_doppelganger("gg_point_aspect_y", p)
})

test_that("gg_point with aspect x and symmetric x", {
  p <- palmerpenguins::penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
      aspect = "x",
      x_limits_to_breaks = TRUE,
    )

  expect_doppelganger("gg_point_aspect_x_symmetric_x", p)
})

test_that("gg_point with aspect y and symmetric y", {
  p <- palmerpenguins::penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
      aspect = "y",
      y_limits_to_breaks = TRUE,
    )

  expect_doppelganger("gg_point_aspect_y_symmetric_y", p)
})

test_that("gg_point with aspect x and symmetric none", {
  p <- palmerpenguins::penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
      aspect = "x",
      x_limits_to_breaks = FALSE,
      y_limits_to_breaks = FALSE,
    )

  expect_doppelganger("gg_point_aspect_x_symmetric_none", p)
})

test_that("gg_point with aspect y and symmetric both", {
  p <- palmerpenguins::penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
      aspect = "y",
      x_limits_to_breaks = TRUE,
      y_limits_to_breaks = TRUE,
    )

  expect_doppelganger("gg_point_aspect_y_symmetric_both", p)
})

test_that("gg_point with aspect y and symmetric none", {
  p <- palmerpenguins::penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
      aspect = "y",
      x_limits_to_breaks = FALSE,
      y_limits_to_breaks = FALSE,
    )

  expect_doppelganger("gg_point_aspect_y_symmetric_none", p)
})

test_that("gg_point with aspect x and symmetric y", {
  p <- palmerpenguins::penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
      aspect = "x",
      y_limits_to_breaks = TRUE,
    )

  expect_doppelganger("gg_point_aspect_x_symmetric_y", p)
})

test_that("gg_point with aspect y and symmetric x", {
  p <- palmerpenguins::penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
      aspect = "y",
      x_limits_to_breaks = TRUE,
    )

  expect_doppelganger("gg_point_aspect_y_symmetric_x", p)
})

test_that("gg_point with aspect x and symmetric both", {
  p <- palmerpenguins::penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
      aspect = "x",
      x_limits_to_breaks = TRUE,
      y_limits_to_breaks = TRUE,
    )

  expect_doppelganger("gg_point_aspect_x_symmetric_both", p)
})
