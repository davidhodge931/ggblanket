# test-palettes.R
library(testthat)
library(vdiffr)
library(ggplot2)
library(dplyr)

test_that("disc_pal_fn_vec", {
  # Test with palette function
  set_blanket(col_palette_d = scales::pal_hue(h.start = 15))
  expect_doppelganger(
    "disc_pal_fn",
    palmerpenguins::penguins |>
      gg_point(
        x = flipper_length_mm,
        y = body_mass_g,
        col = species
      )
  )

  # Test with palette vector (pre-evaluated)
  set_blanket(col_palette_d = scales::pal_hue(h.start = 15)(3))
  expect_doppelganger(
    "disc_pal_vec",
    palmerpenguins::penguins |>
      gg_point(
        x = flipper_length_mm,
        y = body_mass_g,
        col = species
      )
  )

  # Test with palette function and legend reverse
  set_blanket(col_palette_d = scales::pal_hue(h.start = 15))
  expect_doppelganger(
    "disc_pal_fn_rev",
    palmerpenguins::penguins |>
      gg_point(
        x = flipper_length_mm,
        y = body_mass_g,
        col = species,
        col_legend_rev = TRUE
      )
  )

  set_blanket()
})

test_that("cont_pal_fn_vec", {
  # Test with palette function
  set_blanket(col_palette_c = scales::pal_viridis(option = "A"))
  expect_doppelganger(
    "cont_pal_fn",
    palmerpenguins::penguins |>
      gg_point(
        x = flipper_length_mm,
        y = body_mass_g,
        col = bill_length_mm
      )
  )

  # Test with palette vector (pre-evaluated)
  set_blanket(col_palette_c = scales::pal_viridis(option = "A")(256))
  expect_doppelganger(
    "cont_pal_vec",
    palmerpenguins::penguins |>
      gg_point(
        x = flipper_length_mm,
        y = body_mass_g,
        col = bill_length_mm
      )
  )

  # Test with palette function and legend reverse
  set_blanket(col_palette_c = scales::pal_viridis(option = "A"))
  expect_doppelganger(
    "cont_pal_fn_rev",
    palmerpenguins::penguins |>
      gg_point(
        x = flipper_length_mm,
        y = body_mass_g,
        col = bill_length_mm,
        col_legend_rev = TRUE
      )
  )

  # Test with palette vector and legend reverse
  set_blanket(col_palette_c = scales::pal_viridis(option = "A")(256))
  expect_doppelganger(
    "cont_pal_vec_rev",
    palmerpenguins::penguins |>
      gg_point(
        x = flipper_length_mm,
        y = body_mass_g,
        col = bill_length_mm,
        col_legend_rev = TRUE
      )
  )

  set_blanket()
})
