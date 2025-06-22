testthat::skip_if(getRversion() <= package_version("4.1.0"))
testthat::skip_on_os(c("mac", "linux"))

library(palmerpenguins)
library(ggplot2)

set_blanket()

test_name <- "theme_lightmode"

test_that(test_name, {
  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
      facet = sex,
      facet2 = island,
      mapping = aes(alpha = species, shape = species),
      theme = theme_lightmode()
    ) +
    scale_alpha_manual(values = c(1, 1, 0.33))

  vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "theme_lightmode-top"

test_that(test_name, {
  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
      facet = sex,
      facet2 = island,
      mapping = aes(alpha = species, shape = species),
      theme = theme_lightmode(
        legend_position = "top"
      )
    ) +
    scale_alpha_manual(values = c(1, 1, 0.33))

  vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "theme_lightmode-bottom"

test_that(test_name, {
  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
      facet = sex,
      facet2 = island,
      mapping = aes(alpha = species, shape = species),
      theme = theme_lightmode(
        legend_postion = "top",
      )
    ) +
    scale_alpha_manual(values = c(1, 1, 0.33))

  vdiffr::expect_doppelganger(test_name, p)
})

###
test_name <- "theme_darkmode"

test_that(test_name, {
  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
      facet = sex,
      facet2 = island,
      mapping = aes(alpha = species, shape = species),
      theme = theme_darkmode()
    ) +
    scale_alpha_manual(values = c(1, 1, 0.33))

  vdiffr::expect_doppelganger(test_name, p)
})

set_blanket()
