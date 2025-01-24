testthat::skip_if(getRversion() <= package_version("4.1.0"))
testthat::skip_on_os(c("mac", "linux"))

library(palmerpenguins)
library(ggplot2)
library(dplyr)

###
test_name <- "1"

test_that(test_name, {

  set_blanket(
    theme = theme_grey(),
    theme_axis_line_rm = FALSE,
    theme_axis_ticks_rm = FALSE,
    theme_panel_grid_rm = FALSE
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
    theme = light_mode_r(),
    colour = red,
  )

  weave_font_defaults(colour = teal)

  weave_reference_defaults(colour = teal)

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

set_blanket(
  theme = dark_mode_r(base_size = 15),
  colour = red,
  col_palette_d = c(navy, red, "green"),
  col_palette_c = c(navy, purple, red, orange)
)

weave_font_defaults(colour = "red", size = 15 / 2.83505)

weave_reference_defaults(colour = "red", linewidth = 5)

###
test_name <- "3"

test_that(test_name, {

  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      x_breaks = scales::breaks_pretty(3),
    ) +
    geom_vline(xintercept = 200) +
    annotate("text", x = I(0.25), y = I(0.75), label = "Here")

  vdiffr::expect_doppelganger(test_name, p)
})

###
test_name <- "4"

test_that(test_name, {

  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
      x_breaks = scales::breaks_pretty(3),
    ) +
    geom_vline(xintercept = 200) +
    annotate("text",
             x = I(0.25),
             y = I(0.75),
             label = "Here")


  vdiffr::expect_doppelganger(test_name, p)
})

###
test_name <- "5"

test_that(test_name, {

  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = bill_depth_mm,
      x_breaks = scales::breaks_pretty(3),
    ) +
    geom_vline(xintercept = 200) +
    annotate("text", x = I(0.25), y = I(0.75), label = "Here")

  vdiffr::expect_doppelganger(test_name, p)
})

###
test_name <- "6"

test_that(test_name, {

  p <- penguins |>
    mutate(across(sex, \(x) stringr::str_to_sentence(x))) |>
    gg_smooth(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
      se = TRUE,
    ) +
    geom_vline(xintercept = 200) +
    annotate("text", x = I(0.25), y = I(0.75), label = "Here")

  vdiffr::expect_doppelganger(test_name, p)
})

set_blanket()
