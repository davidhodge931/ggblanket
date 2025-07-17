# test-palettes.R

test_that("discrete palettes work with functions and vectors", {
  # Test with palette function
  set_blanket(col_palette_d = scales::pal_hue(h.start = 15))
  p1 <- palmerpenguins::penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species
    )
  vdiffr::expect_doppelganger("discrete palette function", p1)

  # Test with palette vector (pre-evaluated)
  set_blanket(col_palette_d = scales::pal_hue(h.start = 15)(3))
  p2 <- palmerpenguins::penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species
    )
  vdiffr::expect_doppelganger("discrete palette vector", p2)

  # Test with palette function and legend reverse
  set_blanket(col_palette_d = scales::pal_hue(h.start = 15))
  p3 <- palmerpenguins::penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
      col_legend_rev = TRUE
    )
  vdiffr::expect_doppelganger("discrete palette function reversed", p3)

  set_blanket()
})

test_that("continuous palettes work with functions and vectors", {
  # Test with palette function
  set_blanket(col_palette_c = scales::pal_viridis(option = "A"))
  p1 <- palmerpenguins::penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = bill_length_mm
    )
  vdiffr::expect_doppelganger("continuous palette function", p1)

  # Test with palette vector (pre-evaluated)
  set_blanket(col_palette_c = scales::pal_viridis(option = "A")(256))
  p2 <- palmerpenguins::penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = bill_length_mm
    )
  vdiffr::expect_doppelganger("continuous palette vector", p2)

  # Test with palette function and legend reverse
  set_blanket(col_palette_c = scales::pal_viridis(option = "A"))
  p3 <- palmerpenguins::penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = bill_length_mm,
      col_legend_rev = TRUE
    )
  vdiffr::expect_doppelganger("continuous palette function reversed", p3)

  # Test with palette vector and legend reverse
  set_blanket(col_palette_c = scales::pal_viridis(option = "A")(256))
  p4 <- palmerpenguins::penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = bill_length_mm,
      col_legend_rev = TRUE
    )
  vdiffr::expect_doppelganger("continuous palette vector reversed", p4)

  set_blanket()
})
