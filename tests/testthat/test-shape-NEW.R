# Visual tests using vdiffr
library(testthat)
library(vdiffr)
library(ggplot2)
library(dplyr)

# Horizontal orientation tests
test_that("h_shape_only", {
  expect_doppelganger("h_shape_only",
                      palmerpenguins::penguins |>
                        gg_point(
                          x = flipper_length_mm,
                          y = species,
                          shape = island
                        )
  )
})

test_that("h_shape_equals_y", {
  expect_doppelganger("h_shape_equals_y",
                      palmerpenguins::penguins |>
                        gg_point(
                          x = flipper_length_mm,
                          y = species,
                          shape = species
                        )
  )
})

test_that("h_shape_col_diff", {
  expect_doppelganger("h_shape_col_diff",
                      palmerpenguins::penguins |>
                        gg_point(
                          x = flipper_length_mm,
                          y = species,
                          shape = island,
                          col = sex
                        )
  )
})

test_that("h_shape_equals_col", {
  expect_doppelganger("h_shape_equals_col",
                      palmerpenguins::penguins |>
                        gg_point(
                          x = flipper_length_mm,
                          y = species,
                          col = island,
                          shape = island
                        )
  )
})

test_that("h_all_match", {
  expect_doppelganger("h_all_match",
                      palmerpenguins::penguins |>
                        gg_point(
                          x = flipper_length_mm,
                          y = island,
                          col = island,
                          shape = island
                        )
  )
})

# Vertical orientation tests
test_that("v_shape_only", {
  expect_doppelganger("v_shape_only",
                      palmerpenguins::penguins |>
                        gg_point(
                          y = flipper_length_mm,
                          x = species,
                          shape = island
                        )
  )
})

test_that("v_shape_equals_x", {
  expect_doppelganger("v_shape_equals_x",
                      palmerpenguins::penguins |>
                        gg_point(
                          y = flipper_length_mm,
                          x = species,
                          shape = species
                        )
  )
})

test_that("v_shape_col_diff", {
  expect_doppelganger("v_shape_col_diff",
                      palmerpenguins::penguins |>
                        gg_point(
                          y = flipper_length_mm,
                          x = species,
                          shape = island,
                          col = sex
                        )
  )
})

test_that("v_shape_equals_col", {
  expect_doppelganger("v_shape_equals_col",
                      palmerpenguins::penguins |>
                        gg_point(
                          y = flipper_length_mm,
                          x = species,
                          col = island,
                          shape = island
                        )
  )
})

test_that("v_shape_equals_col_2", {
  expect_doppelganger("v_shape_equals_col_2",
                      palmerpenguins::penguins |>
                        gg_point(
                          y = flipper_length_mm,
                          x = species,
                          col = island,
                          shape = island
                        )
  )
})
