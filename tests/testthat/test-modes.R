testthat::skip_if(getRversion() <= package_version("4.1.0"))
testthat::skip_on_os(c("mac", "linux"))

library(palmerpenguins)
library(ggplot2)

set_blanket()

test_name <- "light_mode_r"

test_that(test_name, {

  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
      facet = sex,
      facet2 = island,
      mapping = aes(alpha = species, shape = species),
      mode = light_mode_r()
    ) +
    scale_alpha_manual(values = c(1, 1, 0.33))

  vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "light_mode_t"

test_that(test_name, {

  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
      facet = sex,
      facet2 = island,
      mapping = aes(alpha = species, shape = species),
      mode = light_mode_t()
    ) +
    scale_alpha_manual(values = c(1, 1, 0.33))

  vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "light_mode_b"

test_that(test_name, {

  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
      facet = sex,
      facet2 = island,
      mapping = aes(alpha = species, shape = species),
      mode = light_mode_b()
    ) +
    scale_alpha_manual(values = c(1, 1, 0.33))

  vdiffr::expect_doppelganger(test_name, p)
})

###
test_name <- "grey_mode_r"

test_that(test_name, {

  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
      facet = sex,
      facet2 = island,
      mapping = aes(alpha = species, shape = species),
      mode = grey_mode_r()
    ) +
    scale_alpha_manual(values = c(1, 1, 0.33))

  vdiffr::expect_doppelganger(test_name, p)
})

###
test_name <- "dark_mode_r"

test_that(test_name, {

  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
      facet = sex,
      facet2 = island,
      mapping = aes(alpha = species, shape = species),
      mode = dark_mode_r()
    ) +
    scale_alpha_manual(values = c(1, 1, 0.33))

  vdiffr::expect_doppelganger(test_name, p)
})

set_blanket()

