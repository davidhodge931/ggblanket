testthat::skip_if(getRversion() <= package_version("4.1.0"))
testthat::skip_on_os(c("mac", "linux"))

library(palmerpenguins)
library(ggplot2)

###
test_name <- "1"

test_that(test_name, {

  set_blanket(
    mode = NULL,
    theme = theme_grey(),
  )

  p <- penguins |>
    mutate(across(sex, \(x) stringr::str_to_sentence(x))) |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = sex,
    ) +
    geom_vline(xintercept = 200)

  vdiffr::expect_doppelganger(test_name, p)
})

###
test_name <- "2"

test_that(test_name, {

  set_blanket(
    mode = light_mode_r(),
    geom_colour = pink,
    annotate_colour = teal,
  )

  p <- penguins |>
    mutate(across(sex, \(x) stringr::str_to_sentence(x))) |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = sex,
    ) +
    geom_vline(xintercept = 200)

  vdiffr::expect_doppelganger(test_name, p)
})




