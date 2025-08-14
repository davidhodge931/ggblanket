library(testthat)
library(vdiffr)
library(ggplot2)
library(dplyr)

set_blanket()

## ---------------------------------------------------------------------------------------------------
test_name <- "default"

test_that(test_name, {
  set.seed(123)
  p <- palmerpenguins::penguins |>
    tidyr::drop_na(sex) |>
    mutate(across(sex, \(x) stringr::str_to_sentence(x))) |>
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
  set.seed(123)
  p <- palmerpenguins::penguins |>
    tidyr::drop_na(sex) |>
    mutate(across(sex, \(x) stringr::str_to_sentence(x))) |>
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
  set.seed(123)
  p <- palmerpenguins::penguins |>
    tidyr::drop_na(sex) |>
    mutate(across(sex, \(x) stringr::str_to_sentence(x))) |>
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
  set.seed(123)
  p <- palmerpenguins::penguins |>
    tidyr::drop_na(sex) |>
    mutate(across(sex, \(x) stringr::str_to_sentence(x))) |>
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
  set.seed(123)
  p <- palmerpenguins::penguins |>
    tidyr::drop_na(sex) |>
    mutate(across(sex, \(x) stringr::str_to_sentence(x))) |>
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

set_blanket()
