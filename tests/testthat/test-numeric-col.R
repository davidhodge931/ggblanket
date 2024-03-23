testthat::skip_if(getRversion() <= package_version("4.1.0"))
testthat::skip_on_os(c("mac", "linux"))

library(palmerpenguins)
library(ggplot2)

test_name <- "light_mode_r"

test_that(test_name, {

  p <- penguins |>
    gg_jitter(
      x = flipper_length_mm,
      y = island,
      col = bill_depth_mm,
      facet = sex,
      mapping = aes(alpha = species, shape = species),
      col_pal = c(teal, orange, plum),
      mode = light_mode_r()
    ) +
    scale_alpha_manual(values = c(1, 1, 0.33))

  vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "light_mode_t"

test_that(test_name, {

  p <- penguins |>
    gg_jitter(
      x = flipper_length_mm,
      y = island,
      col = bill_depth_mm,
      facet = sex,
      mapping = aes(alpha = species, shape = species),
      col_pal = c(teal, orange, plum),
      mode = light_mode_t()
    ) +
    scale_alpha_manual(values = c(1, 1, 0.33))

  vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "light_mode_b"

test_that(test_name, {

  p <- penguins |>
    gg_jitter(
      x = flipper_length_mm,
      y = island,
      col = bill_depth_mm,
      facet = sex,
      mapping = aes(alpha = species, shape = species),
      col_pal = c(teal, orange, plum),
      mode = light_mode_b()
    ) +
    scale_alpha_manual(values = c(1, 1, 0.33))

  vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "light_mode_n"

test_that(test_name, {

  p <- penguins |>
    gg_jitter(
      x = flipper_length_mm,
      y = island,
      col = bill_depth_mm,
      facet = sex,
      mapping = aes(alpha = species, shape = species),
      col_pal = c(teal, orange, plum),
      mode = light_mode_n()
    ) +
    scale_alpha_manual(values = c(1, 1, 0.33))

  vdiffr::expect_doppelganger(test_name, p)
})
