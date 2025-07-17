# test-palettes-discrete-vectors.R

test_that("discrete palettes work with color transformation vectors", {
  # Test with col_palette_d using color transformation
  set_blanket(col_palette_d = scales::col_darker(scales::pal_hue()(3)))

  p1 <- palmerpenguins::penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species
    )
  vdiffr::expect_doppelganger("discrete darker hue jitter", p1)

  p2 <- palmerpenguins::penguins |>
    gg_violin(
      x = species,
      y = body_mass_g,
      col = island
    )
  vdiffr::expect_doppelganger("discrete darker hue violin", p2)

  set_blanket()
})

test_that("discrete palettes work with separate colour and fill transformations", {
  # Test with separate colour_palette_d and fill_palette_d
  set_blanket(
    colour_palette_d = scales::col_darker(scales::pal_hue(h.start = 15)(3)),
    fill_palette_d = scales::col_lighter(scales::pal_hue(h.start = 15)(3))
  )

  p1 <- palmerpenguins::penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species
    )
  vdiffr::expect_doppelganger("discrete darker colour lighter fill jitter", p1)

  p2 <- palmerpenguins::penguins |>
    gg_violin(
      x = species,
      y = body_mass_g,
      col = island
    )
  vdiffr::expect_doppelganger("discrete darker colour lighter fill violin", p2)

  p3 <- palmerpenguins::penguins |>
    gg_violin(
      x = species,
      y = body_mass_g,
      col = island,
      col_legend_rev = TRUE
    )
  vdiffr::expect_doppelganger("discrete darker colour lighter fill violin reversed", p3)

  set_blanket()
})

test_that("discrete palettes work with flipped orientation", {
  set_blanket(
    colour_palette_d = scales::col_darker(scales::pal_hue(h.start = 15)(3)),
    fill_palette_d = scales::col_lighter(scales::pal_hue(h.start = 15)(3))
  )

  p1 <- palmerpenguins::penguins |>
    gg_violin(
      x = body_mass_g,
      y = species,
      col = island
    )
  vdiffr::expect_doppelganger("discrete flipped violin", p1)

  p2 <- palmerpenguins::penguins |>
    gg_violin(
      x = body_mass_g,
      y = species,
      col = island,
      col_legend_rev = TRUE
    )
  vdiffr::expect_doppelganger("discrete flipped violin reversed", p2)

  set_blanket()
})

test_that("discrete palettes can be overridden at plot level", {
  set_blanket(
    colour_palette_d = scales::col_darker(scales::pal_hue(h.start = 15)(3)),
    fill_palette_d = scales::col_lighter(scales::pal_hue(h.start = 15)(3))
  )

  # Base plot with blanket settings
  p1 <- palmerpenguins::penguins |>
    gg_violin(
      x = body_mass_g,
      y = species,
      col = island
    )
  vdiffr::expect_doppelganger("discrete base violin", p1)

  # Override colour_palette only
  p2 <- palmerpenguins::penguins |>
    gg_violin(
      x = body_mass_g,
      y = species,
      col = island,
      colour_palette = scales::pal_viridis()(3)
    )
  vdiffr::expect_doppelganger("discrete override colour palette", p2)

  # Override fill_palette only
  p3 <- palmerpenguins::penguins |>
    gg_violin(
      x = body_mass_g,
      y = species,
      col = island,
      fill_palette = scales::pal_viridis()(3)
    )
  vdiffr::expect_doppelganger("discrete override fill palette", p3)

  # Override both with transformations
  p4 <- palmerpenguins::penguins |>
    gg_violin(
      x = body_mass_g,
      y = species,
      col = island,
      colour_palette = col_multiply(scales::pal_viridis()(3)),
      fill_palette = scales::pal_viridis()(3)
    )
  vdiffr::expect_doppelganger("discrete override both palettes with transform", p4)

  set_blanket()
})

test_that("discrete palettes work without blanket settings", {
  # Ensure blanket is reset
  set_blanket()

  # Override colour_palette at plot level
  p1 <- palmerpenguins::penguins |>
    gg_violin(
      x = body_mass_g,
      y = species,
      col = island,
      colour_palette = scales::pal_viridis()(3)
    )
  vdiffr::expect_doppelganger("discrete no blanket colour override", p1)

  # Override fill_palette at plot level
  p2 <- palmerpenguins::penguins |>
    gg_violin(
      x = body_mass_g,
      y = species,
      col = island,
      fill_palette = scales::pal_viridis()(3)
    )
  vdiffr::expect_doppelganger("discrete no blanket fill override", p2)

  # Override both with transformations
  p3 <- palmerpenguins::penguins |>
    gg_violin(
      x = body_mass_g,
      y = species,
      col = island,
      colour_palette = col_multiply(scales::pal_viridis()(3)),
      fill_palette = scales::pal_viridis()(3)
    )
  vdiffr::expect_doppelganger("discrete no blanket both override with transform", p3)

  set_blanket()
})
