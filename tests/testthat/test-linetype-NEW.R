# Visual tests for gg_path linetype using vdiffr
library(testthat)
library(vdiffr)
library(ggplot2)
library(dplyr)

# Horizontal orientation tests
test_that("h_linetype_only", {
  expect_doppelganger("h_linetype_only",
                      palmerpenguins::penguins |>
                        gg_path(
                          x = flipper_length_mm,
                          y = species,
                          linetype = island
                        )
  )
})

test_that("h_linetype_equals_y", {
  expect_doppelganger("h_linetype_equals_y",
                      palmerpenguins::penguins |>
                        gg_path(
                          x = flipper_length_mm,
                          y = species,
                          linetype = species
                        )
  )
})

test_that("h_linetype_col_diff", {
  expect_doppelganger("h_linetype_col_diff",
                      palmerpenguins::penguins |>
                        gg_path(
                          x = flipper_length_mm,
                          y = species,
                          linetype = island,
                          col = sex
                        )
  )
})

test_that("h_linetype_equals_col", {
  expect_doppelganger("h_linetype_equals_col",
                      palmerpenguins::penguins |>
                        gg_path(
                          x = flipper_length_mm,
                          y = species,
                          col = island,
                          linetype = island
                        )
  )
})

test_that("h_all_match", {
  expect_doppelganger("h_all_match",
                      palmerpenguins::penguins |>
                        gg_path(
                          x = flipper_length_mm,
                          y = island,
                          col = island,
                          linetype = island
                        )
  )
})

# Vertical orientation tests
test_that("v_linetype_only", {
  expect_doppelganger("v_linetype_only",
                      palmerpenguins::penguins |>
                        gg_path(
                          y = flipper_length_mm,
                          x = species,
                          linetype = island
                        )
  )
})

test_that("v_linetype_equals_x", {
  expect_doppelganger("v_linetype_equals_x",
                      palmerpenguins::penguins |>
                        gg_path(
                          y = flipper_length_mm,
                          x = species,
                          linetype = species
                        )
  )
})

test_that("v_linetype_col_diff", {
  expect_doppelganger("v_linetype_col_diff",
                      palmerpenguins::penguins |>
                        gg_path(
                          y = flipper_length_mm,
                          x = species,
                          linetype = island,
                          col = sex
                        )
  )
})

test_that("v_linetype_equals_col", {
  expect_doppelganger("v_linetype_equals_col",
                      palmerpenguins::penguins |>
                        gg_path(
                          y = flipper_length_mm,
                          x = species,
                          col = island,
                          linetype = island
                        )
  )
})

test_that("v_linetype_equals_col_2", {
  expect_doppelganger("v_linetype_equals_col_2",
                      palmerpenguins::penguins |>
                        gg_path(
                          y = flipper_length_mm,
                          x = species,
                          col = island,
                          linetype = island
                        )
  )
})
