testthat::skip_if(getRversion() <= package_version("4.1.0"))
testthat::skip_on_os(c("mac", "linux"))

library(palmerpenguins)
library(ggplot2)

test_name <- "light_mode_r"

test_that(test_name, {

  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
      alpha = species,
      facet = sex,
      facet2 = island,
      mapping = aes(shape = species),
      col_pal = c(teal, orange, plum),
      alpha_pal = c(1, 1, 0.33),
      mode = light_mode_r()
    )

  vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "light_mode_t"

test_that(test_name, {

  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
      alpha = species,
      facet = sex,
      facet2 = island,
      mapping = aes(shape = species),
      col_pal = c(teal, orange, plum),
      alpha_pal = c(1, 1, 0.33),
      mode = light_mode_t()
    )

  vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "light_mode_b"

test_that(test_name, {

  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
      alpha = species,
      facet = sex,
      facet2 = island,
      mapping = aes(shape = species),
      col_pal = c(teal, orange, plum),
      alpha_pal = c(1, 1, 0.33),
      mode = light_mode_b()
    )

  vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "light_mode_n"

test_that(test_name, {

  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
      alpha = species,
      facet = sex,
      facet2 = island,
      mapping = aes(shape = species),
      col_pal = c(teal, orange, plum),
      alpha_pal = c(1, 1, 0.33),
      mode = light_mode_n()
    )

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
      alpha = species,
      facet = sex,
      facet2 = island,
      mapping = aes(shape = species),
      col_pal = c(teal, orange, plum),
      alpha_pal = c(1, 1, 0.33),
      mode = grey_mode_r()
    )

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
      alpha = species,
      facet = sex,
      facet2 = island,
      mapping = aes(shape = species),
      col_pal = c(teal, orange, plum),
      alpha_pal = c(1, 1, 0.33),
      mode = dark_mode_r()
    )

  vdiffr::expect_doppelganger(test_name, p)
})

