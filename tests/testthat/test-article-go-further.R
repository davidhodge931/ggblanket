testthat::skip_if(getRversion() <= package_version("4.1.0"))
testthat::skip_on_os(c("mac", "linux"))

## ----setup------------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(palmerpenguins)
library(patchwork)
set_blanket()

## ---------------------------------------------------------------------------------------------------
test_name <- "1"

test_that(test_name, {
  p1 <- diamonds |>
    count(color) |>
    gg_col(
      x = n,
      y = color,
      width = 0.75,
      x_labels = \(x) x / 1000,
      x_title = "Count (thousands)",
      subtitle = "\nDefault order"
    )

  p2 <- diamonds |>
    count(color) |>
    mutate(across(color, \(x) {
      x |>
        forcats::fct_reorder(n) |>
        forcats::fct_rev()
    })) |>
    gg_col(
      x = n,
      y = color,
      width = 0.75,
      x_labels = \(x) x / 1000,
      x_title = "Count (thousands)",
      subtitle = "\nRe-orderered"
    )

  p <- p1 + p2

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "2"

test_that(test_name, {
  p1 <- diamonds |>
    count(color) |>
    filter(color %in% c("E", "G", "I")) |>
    gg_col(
      x = n,
      y = color,
      width = 0.75,
      x_labels = \(x) x / 1000,
      x_title = "Count (thousands)",
      subtitle = "\nUnused levels kept",
    )

  p2 <- diamonds |>
    count(color) |>
    filter(color %in% c("E", "G", "I")) |>
    mutate(color = forcats::fct_drop(color)) |>
    gg_col(
      x = n,
      y = color,
      width = 0.75,
      x_labels = \(x) x / 1000,
      x_title = "Count (thousands)",
      subtitle = "\nUnused levels dropped",
    )

  p <- p1 + p2

  vdiffr::expect_doppelganger(test_name, p)
})


## ----fig.asp=0.4------------------------------------------------------------------------------------
test_name <- "3"

test_that(test_name, {
  p <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE) |>
    gg_sf(col = AREA)

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "4"

test_that(test_name, {
  p <- penguins |>
    gg_linerange(
      stat = "summary",
      x = species,
      y = flipper_length_mm,
    ) +
    geom_point(stat = "summary")

  vdiffr::expect_doppelganger(test_name, p)
})


## ---------------------------------------------------------------------------------------------------
test_name <- "5"

test_that(test_name, {
  p <- penguins |>
    mutate(across(sex, \(x) str_to_sentence(x))) |>
    gg_boxplot(
      x = species,
      y = flipper_length_mm,
      col = sex,
      colour = "black", #or fill = #D3D3D3",
      position = position_dodge2(preserve = "single"),
      alpha = 0.9,
    )

  vdiffr::expect_doppelganger(test_name, p)
})


## ---------------------------------------------------------------------------------------------------
test_name <- "6"

test_that(test_name, {
  p <- penguins |>
    gg_boxplot(
      x = species,
      y = flipper_length_mm,
      colour = "black", #or fill = #D3D3D3",
      width = 0.5,
      alpha = 0.9,
    )

  vdiffr::expect_doppelganger(test_name, p)
})


## ---------------------------------------------------------------------------------------------------
test_name <- "7"

test_that(test_name, {
  p <- penguins |>
    count(species, sex) |>
    gg_col(
      x = sex,
      y = n,
      col = species,
      position = position_dodge2(preserve = "single"),
      width = 0.75,
      x_labels = \(x) str_to_sentence(x),
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "8"

test_that(test_name, {
  p <- penguins |>
    count(species, sex) |>
    gg_col(
      x = n,
      y = sex,
      col = species,
      position = position_dodge2(preserve = "single"),
      width = 0.75,
      y_labels = \(x) str_to_sentence(x),
    )

  vdiffr::expect_doppelganger(test_name, p)
})


## ---------------------------------------------------------------------------------------------------
test_name <- "9"

test_that(test_name, {
  p <- penguins |>
    mutate(across(sex, \(x) str_to_sentence(x))) |>
    gg_histogram(
      x = flipper_length_mm,
      facet = species,
      facet2 = sex,
    )

  vdiffr::expect_doppelganger(test_name, p)
})


## ---------------------------------------------------------------------------------------------------
test_name <- "10"

test_that(test_name, {
  p <- data.frame(
    age = c(0:9, 0:9),
    sex = c(rep("Male", 10), rep("Female", 10)),
    population = c(
      200,
      250,
      300,
      350,
      440,
      450,
      500,
      550,
      600,
      650,
      190,
      240,
      290,
      330,
      420,
      430,
      480,
      530,
      580,
      630
    )
  ) |>
    mutate(population = ifelse(sex == "Female", -population, population)) %>%
    gg_col(
      y = age,
      x = population,
      col = sex,
      width = 1,
      orientation = "y",
      x_labels = \(x) abs(x),
      y_symmetric = TRUE,
    ) +
    geom_vline(
      xintercept = 0,
      colour = "#121b24",
      linewidth = 10 / 33
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "11"

test_that(test_name, {
  p <- data.frame(
    age = c(0:9, 0:9),
    sex = c(rep("Male", 10), rep("Female", 10)),
    population = c(
      200,
      250,
      300,
      350,
      440,
      450,
      500,
      550,
      600,
      650,
      190,
      240,
      290,
      330,
      420,
      430,
      480,
      530,
      580,
      630
    )
  ) |>
    mutate(population = ifelse(sex == "Female", -population, population)) %>%
    gg_col(
      y = age,
      x = population,
      col = sex,
      perspective = "x",
      orientation = "y",
      x_labels = \(x) abs(x),
      y_symmetric = FALSE,
      y_expand = c(0, 0),
    )
  # geom_vline(
  #   xintercept = 0,
  # )

  vdiffr::expect_doppelganger(test_name, p)
})

## ----fig.asp=0.55-----------------------------------------------------------------------------------
test_name <- "12"

test_that(test_name, {
  p <- penguins |>
    gg_histogram(
      x = flipper_length_mm,
      mapping = aes(y = after_stat(density)),
      facet = species,
    )

  vdiffr::expect_doppelganger(test_name, p)
})


## ----fig.asp=0.55-----------------------------------------------------------------------------------
test_name <- "13"

test_that(test_name, {
  p <- faithfuld |>
    gg_contour(
      x = waiting,
      y = eruptions,
      z = density,
      mapping = aes(colour = after_stat(level)),
      bins = 8,
    )

  vdiffr::expect_doppelganger(test_name, p)
})

set_blanket()
