# test-gg_blanket_literal_colors.R
# Tests for literal color functionality in gg_blanket

library(testthat)
library(ggplot2)
library(palmerpenguins)
library(vdiffr)

test_name <- "gg_blanket accepts color names"

test_that(test_name, {
  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = "blue"
    )

  vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "gg_blanket accepts hex color codes"
test_that(test_name, {
  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = "#FF6B6B"
    )

  vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "gg_blanket accepts color functions"
test_that(test_name, {
  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = colorspace::lighten("steelblue", 0.3)
    )

  vdiffr::expect_doppelganger(test_name, p)
})
