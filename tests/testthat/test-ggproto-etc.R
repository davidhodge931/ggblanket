testthat::skip_if(getRversion() <= package_version("4.1.0"))
testthat::skip_on_os(c("mac", "linux"))

library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(palmerpenguins)
set_blanket()

## ---------------------------------------------------------------------------------------------------
test_name <- "default"

test_that(test_name, {
  p <- penguins |>
    drop_na(sex) |>
    mutate(across(sex, \(x) str_to_sentence(x))) |>
    gg_blanket(
      geom = "violin",
      stat = "ydensity",
      position = "dodge",
      x = sex,
      y = body_mass_g,
      col = species,
    )

  vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "GeomViolin"

test_that(test_name, {
  p <- penguins |>
    drop_na(sex) |>
    mutate(across(sex, \(x) str_to_sentence(x))) |>
    gg_blanket(
      geom = GeomViolin,
      stat = "ydensity",
      position = "dodge",
      x = sex,
      y = body_mass_g,
      col = species,
    )

    vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "StatYdensity"

test_that(test_name, {
  p <- penguins |>
    drop_na(sex) |>
    mutate(across(sex, \(x) str_to_sentence(x))) |>
    gg_blanket(
      geom = "violin",
      stat = StatYdensity,
      position = "dodge",
      x = sex,
      y = body_mass_g,
      col = species,
    )

    vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "PositionDodge"

test_that(test_name, {
  p <- penguins |>
    drop_na(sex) |>
    mutate(across(sex, \(x) str_to_sentence(x))) |>
    gg_blanket(
      geom = "violin",
      stat = "ydensity",
      position = PositionDodge,
      x = sex,
      y = body_mass_g,
      col = species,
    )

    vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "all 3"

test_that(test_name, {
  p <- penguins |>
    drop_na(sex) |>
    mutate(across(sex, \(x) str_to_sentence(x))) |>
    gg_blanket(
      geom = GeomViolin,
      stat = StatYdensity,
      position = PositionDodge,
      x = sex,
      y = body_mass_g,
      col = species,
    )

    vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "transform_log10"

test_that(test_name, {
  p <- penguins |>
    drop_na(sex) |>
    mutate(across(sex, \(x) str_to_sentence(x))) |>
    gg_blanket(
      geom = GeomViolin,
      stat = StatYdensity,
      position = PositionDodge,
      y_transform = scales::transform_log10(),
      x = sex,
      y = body_mass_g,
      col = species,
    )

    vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "transform_reverse"

test_that(test_name, {
  p <- penguins |>
    drop_na(sex) |>
    mutate(across(sex, \(x) str_to_sentence(x))) |>
    gg_blanket(
      geom = GeomViolin,
      stat = StatYdensity,
      position = PositionDodge,
      y_transform = scales::transform_reverse(),
      x = sex,
      y = body_mass_g,
      col = species,
    )

    vdiffr::expect_doppelganger(test_name, p)
})

# set_blanket()
