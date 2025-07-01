testthat::skip_if(getRversion() <= package_version("4.1.0"))
testthat::skip_on_os(c("mac", "linux"))

library(palmerpenguins)
library(ggplot2)

set_blanket()

test_name <- "theme_lighter"

test_that(test_name, {
  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
      facet = sex,
      facet2 = island,
      mapping = aes(alpha = species, shape = species),
      theme = theme_lighter()
    ) +
    scale_alpha_manual(values = c(1, 1, 0.33))

  vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "theme_lighter-top"

test_that(test_name, {
  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
      facet = sex,
      facet2 = island,
      mapping = aes(alpha = species, shape = species),
      theme = theme_lighter(
        legend_position = "top"
      )
    ) +
    scale_alpha_manual(values = c(1, 1, 0.33))

  vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "theme_lighter-bottom"

test_that(test_name, {
  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
      facet = sex,
      facet2 = island,
      mapping = aes(alpha = species, shape = species),
      theme = theme_lighter(
        legend_postion = "top",
      )
    ) +
    scale_alpha_manual(values = c(1, 1, 0.33))

  vdiffr::expect_doppelganger(test_name, p)
})

###
test_name <- "theme_darker"

test_that(test_name, {
  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
      facet = sex,
      facet2 = island,
      mapping = aes(alpha = species, shape = species),
      theme = theme_darker()
    ) +
    scale_alpha_manual(values = c(1, 1, 0.33))

  vdiffr::expect_doppelganger(test_name, p)
})

set_blanket()
