testthat::skip_if(getRversion() <= package_version("4.1.0"))
testthat::skip_on_os(c("mac", "linux"))

library(ggplot2)
library(dplyr)
set_blanket()

## ---------------------------------------------------------------------------------------------------
test_name <- "1"

test_that(test_name, {
  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      x_breaks = scales::breaks_width(-15),
      x_expand_limits = 250,
      x_labels = scales::label_currency(),
      x_transform = scales::transform_reverse(),
      x_limits = c(200, 170),
      x_oob = scales::oob_censor,
      x_expand = c(0.5, 0.5),
      x_position = "top",
      x_title = "Blah",
      caption = ""
  )

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "2"

test_that(test_name, {
  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      y_breaks = scales::breaks_width(-1000),
      y_expand_limits = 7000,
      y_labels = scales::label_currency(),
      y_transform = scales::transform_reverse(),
      y_limits = c(6000, 2000),
      y_oob = scales::oob_censor,
      y_expand = c(0.1, 0.1),
      y_position = "right",
      y_title = "Blah",
      caption = ""
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "3"

test_that(test_name, {
  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      x_breaks = scales::breaks_width(-15),
      x_expand_limits = 250,
      x_labels = scales::label_currency(),
      x_transform = scales::transform_reverse(),
      x_limits = c(200, 170),
      x_oob = scales::oob_censor,
      x_expand = c(0.5, 0.5),
      x_position = "top",
      x_title = "Blah",
      y_breaks = scales::breaks_width(-1000),
      y_expand_limits = 7000,
      y_labels = scales::label_currency(),
      y_transform = scales::transform_reverse(),
      y_limits = c(6000, 2000),
      y_oob = scales::oob_censor,
      y_expand = c(0.1, 0.1),
      y_position = "right",
      y_title = "Blah",
      caption = ""
    )

  vdiffr::expect_doppelganger(test_name, p)
})

set_blanket()
