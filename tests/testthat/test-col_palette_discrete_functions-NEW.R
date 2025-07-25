# test-palettes-discrete-functions.R
library(testthat)
library(vdiffr)
library(ggplot2)
library(dplyr)

test_that("disc_col_trans", {
  # Test with col_palette_d using color transformation
  set_blanket(col_palette_d = scales::col_darker(scales::pal_hue()))

  expect_doppelganger(
    "disc_dark_hue_jit",
    palmerpenguins::penguins |>
      gg_point(
        x = flipper_length_mm,
        y = body_mass_g,
        col = species
      )
  )

  expect_doppelganger(
    "disc_dark_hue_vio",
    palmerpenguins::penguins |>
      gg_violin(
        x = species,
        y = body_mass_g,
        col = island
      )
  )

  set_blanket()
})

test_that("disc_sep_col_fill", {
  # Test with separate colour_palette_d and fill_palette_d
  set_blanket(
    col_palette_d = scales::col_darker(scales::pal_hue(h.start = 15)),
  )

  expect_doppelganger(
    "disc_dark_light_jit",
    palmerpenguins::penguins |>
      gg_point(
        x = flipper_length_mm,
        y = body_mass_g,
        col = species
      )
  )

  expect_doppelganger(
    "disc_dark_light_vio",
    palmerpenguins::penguins |>
      gg_violin(
        x = species,
        y = body_mass_g,
        col = island
      )
  )

  expect_doppelganger(
    "disc_dark_light_vio_rev",
    palmerpenguins::penguins |>
      gg_violin(
        x = species,
        y = body_mass_g,
        col = island,
        col_legend_rev = TRUE
      )
  )

  set_blanket()
})

test_that("disc_flip_orient", {
  set_blanket(
    colour_palette_d = scales::col_darker(scales::pal_hue(h.start = 15)),
    fill_palette_d = scales::col_lighter(scales::pal_hue(h.start = 15))
  )

  expect_doppelganger(
    "disc_flip_vio",
    palmerpenguins::penguins |>
      gg_violin(
        x = body_mass_g,
        y = species,
        col = island
      )
  )

  expect_doppelganger(
    "disc_flip_vio_rev",
    palmerpenguins::penguins |>
      gg_violin(
        x = body_mass_g,
        y = species,
        col = island,
        col_legend_rev = TRUE
      )
  )

  set_blanket()
})

test_that("disc_override", {
  set_blanket(
    colour_palette_d = scales::col_darker(scales::pal_hue(h.start = 15)),
    fill_palette_d = scales::col_lighter(scales::pal_hue(h.start = 15))
  )

  # Base plot with blanket settings
  expect_doppelganger(
    "disc_base_vio",
    palmerpenguins::penguins |>
      gg_violin(
        x = body_mass_g,
        y = species,
        col = island
      )
  )

  # Override colour_palette only
  expect_doppelganger(
    "disc_over_col",
    palmerpenguins::penguins |>
      gg_violin(
        x = body_mass_g,
        y = species,
        col = island,
        colour_palette = scales::pal_viridis()
      )
  )

  # Override fill_palette only
  expect_doppelganger(
    "disc_over_fill",
    palmerpenguins::penguins |>
      gg_violin(
        x = body_mass_g,
        y = species,
        col = island,
        fill_palette = scales::pal_viridis()
      )
  )

  # Override both with transformations
  expect_doppelganger(
    "disc_over_both_trans",
    palmerpenguins::penguins |>
      gg_violin(
        x = body_mass_g,
        y = species,
        col = island,
        colour_palette = col_multiply(scales::pal_viridis()),
        fill_palette = scales::pal_viridis()
      )
  )

  set_blanket()
})

test_that("disc_no_blanket", {
  # Ensure blanket is reset
  set_blanket()

  # Override colour_palette at plot level
  expect_doppelganger(
    "disc_no_bl_col",
    palmerpenguins::penguins |>
      gg_violin(
        x = body_mass_g,
        y = species,
        col = island,
        colour_palette = scales::pal_viridis()
      )
  )

  # Override fill_palette at plot level
  expect_doppelganger(
    "disc_no_bl_fill",
    palmerpenguins::penguins |>
      gg_violin(
        x = body_mass_g,
        y = species,
        col = island,
        fill_palette = scales::pal_viridis()
      )
  )

  # Override both with transformations
  expect_doppelganger(
    "disc_no_bl_both_trans",
    palmerpenguins::penguins |>
      gg_violin(
        x = body_mass_g,
        y = species,
        col = island,
        colour_palette = col_multiply(scales::pal_viridis()),
        fill_palette = scales::pal_viridis()
      )
  )

  set_blanket()
})
