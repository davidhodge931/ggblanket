testthat::skip_if(getRversion() <= package_version("4.1.0"))

library(ggblanket)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(palmerpenguins)
library(patchwork)


## ---------------------------------------------------------------------------------------------------
test_name <- "1"

test_that(test_name, {
  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
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
      col = species,
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ----fig.asp=0.45-----------------------------------------------------------------------------------
test_name <- "3"

test_that(test_name, {
  p <- penguins |>
    drop_na(sex) |>
    mutate(across(sex, \(x) str_to_sentence(x))) |>
    gg_bar(
      y = species,
      col = sex,
      width = 0.75,
      position = "dodge",
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "4"

test_that(test_name, {
  p <- penguins |>
    drop_na(sex) |>
    mutate(across(sex, \(x) str_to_sentence(x))) |>
    gg_histogram(
      x = flipper_length_mm,
      facet = species,
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ----fig.asp=0.75-----------------------------------------------------------------------------------
test_name <- "5"

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
test_name <- "6"

test_that(test_name, {
  set.seed(123)

  p <- penguins |>
    gg_jitter(
      x = species,
      y = body_mass_g,
      col_pal = "#7FCDBB",
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ----fig.asp=0.33-----------------------------------------------------------------------------------
# test_name <- "7"
#
# test_that(test_name, {
#   viridisLite::rocket(n = 9)
#
#   p <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE) |>
#     gg_sf(
#       col = AREA,
#       col_pal = viridisLite::rocket(n = 9)
#     )
#
#   vdiffr::expect_doppelganger(test_name, p)
# })

## ---------------------------------------------------------------------------------------------------
test_name <- "8"

test_that(test_name, {
  set.seed(123)

  p <- penguins |>
    drop_na(sex) |>
    gg_jitter(
      x = species,
      y = body_mass_g,
      col = flipper_length_mm,
      facet = sex,
      x_labels = \(x) str_sub(x, 1, 1),
      y_expand_limits = 0,
      y_breaks = scales::breaks_width(1500),
      y_labels = scales::label_number(big.mark = " "),
      y_expand = expansion(mult = c(0, 0.05)),
      y_transform = "sqrt",
      y_title = "Body mass (g)",
      col_continuous_type = "steps",
      facet_labels = \(x) str_to_sentence(x),
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ----fig.asp=0.6------------------------------------------------------------------------------------
test_name <- "9"

test_that(test_name, {
  p <- diamonds |>
    gg_hex(
      x = carat,
      y = price,
      coord = coord_cartesian(clip = "on"),
      y_limits = c(0, 20000),
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ----echo=FALSE-------------------------------------------------------------------------------------
test_name <- "10"

test_that(test_name, {
  d <- data.frame(
    trt = factor(c(1, 1, 2, 2)),
    resp = c(1, 5, 3, 4),
    group = factor(c(1, 2, 1, 2)),
    upper = c(1.1, 5.3, 3.3, 4.2),
    lower = c(0.8, 4.6, 2.4, 3.6)
  )

  p1 <- d |>
    gg_errorbar(
      x = trt,
      ymin = lower,
      ymax = upper,
      col = group,
      width = 0.1,
      x_title = "Treatment",
      y_title = "Response",
      mode = light_mode_n(),
      subtitle = "\nmode = light_mode_n(),"
    )

  p2 <- d |>
    gg_errorbar(
      x = trt,
      ymin = lower,
      ymax = upper,
      col = group,
      width = 0.1,
      x_title = "Treatment",
      y_title = "Response",
      subtitle = "\n+ light_mode_n()"
    ) +
    light_mode_n()

  p <- p1 + p2

  vdiffr::expect_doppelganger(test_name, p)
})



## ----fig.asp=0.7------------------------------------------------------------------------------------
test_name <- "11"

test_that(test_name, {
  p <- penguins |>
    gg_histogram(
      x = flipper_length_mm,
      col = species,
      title = "Penguin flipper length by species",
      subtitle = "Palmer Archipelago, Antarctica",
      caption = "Source: Gorman, 2020",
      mode = light_mode_t(),
    ) +
    theme(legend.title = element_blank())

  vdiffr::expect_doppelganger(test_name, p)
})



## ----fig.asp=0.75-----------------------------------------------------------------------------------
test_name <- "12"

test_that(test_name, {
  p <- penguins |>
    gg_histogram(
      x = flipper_length_mm,
      col = species,
      title = "Penguin flipper length by species",
      subtitle = "Palmer Archipelago, Antarctica",
      caption = "Source: Gorman, 2020",
      mode = grey_mode_b(),
    )

  vdiffr::expect_doppelganger(test_name, p)
})



## ----fig.asp=0.65-----------------------------------------------------------------------------------
test_name <- "13"

test_that(test_name, {
  p <- penguins |>
    gg_histogram(
      x = flipper_length_mm,
      col = species,
      col_pal = c(teal, orange, plum),
      title = "Penguin flipper length by species",
      subtitle = "Palmer Archipelago, Antarctica",
      caption = "Source: Gorman, 2020",
      mode = dark_mode_r(),
    )

  vdiffr::expect_doppelganger(test_name, p)
})



## ----echo=FALSE-------------------------------------------------------------------------------------
test_name <- "14"

test_that(test_name, {
  d <- data.frame(
    trt = factor(c(1, 1, 2, 2)),
    resp = c(1, 5, 3, 4),
    group = factor(c(1, 2, 1, 2)),
    upper = c(1.1, 5.3, 3.3, 4.2),
    lower = c(0.8, 4.6, 2.4, 3.6)
  )

  p1 <- d |>
    gg_errorbar(
      x = trt,
      ymin = lower,
      ymax = upper,
      col = group,
      width = 0.1,
      x_title = "Treatment",
      y_title = "Response",
      mode = light_mode_n(),
      subtitle = "\nDefault y scale"
    )

  p2 <- d |>
    gg_errorbar(
      x = trt,
      ymin = lower,
      ymax = upper,
      col = group,
      width = 0.1,
      x_title = "Treatment",
      y_title = "Response",
      y_limits = c(NA, NA),
      mode = light_mode_n(),
      subtitle = "\ny_limits = c(NA, NA),"
    )

  p3 <- d |>
    gg_col(
      x = trt,
      y = upper,
      col = group,
      position = "dodge",
      width = 0.5,
      x_title = "Treatment upper",
      y_title = "Response",
      y_limits = c(0, NA),
      mode = light_mode_n(),
      subtitle = "\ny_limits = c(0, NA),"
    )

  p <- p1 + p2 + p3

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "15"

test_that(test_name, {
  p <- penguins |>
    mutate(across(sex, \(x) str_to_sentence(x))) |>
    drop_na(sex) |>
    gg_smooth(
      x = flipper_length_mm,
      y = body_mass_g,
      col = sex,
      se = TRUE, # via ... from geom_smooth
      level = 0.999, # via ... from geom_smooth
    )

  vdiffr::expect_doppelganger(test_name, p)
})



## ----fig.asp=0.4------------------------------------------------------------------------------------
test_name <- "16"

test_that(test_name, {
  p <- penguins |>
    gg_boxplot(
      x = flipper_length_mm,
      y = species,
      position = position_dodge2(preserve = "single"),
      alpha_pal = 0, #or col_pal = scales::alpha(blue, 0),
    )

  vdiffr::expect_doppelganger(test_name, p)
})



## ---------------------------------------------------------------------------------------------------
test_name <- "17"

test_that(test_name, {
  p <- penguins |>
    gg_boxplot(
      x = species,
      y = flipper_length_mm,
      col = sex,
      position = position_dodge2(preserve = "single"),
      alpha_pal = 0,
    )

  vdiffr::expect_doppelganger(test_name, p)
})



## ----fig.asp = 0.4----------------------------------------------------------------------------------
test_name <- "18"

test_that(test_name, {
  p <- penguins |>
    group_by(species) |>
    summarise(body_mass_g = mean(body_mass_g, na.rm = TRUE)) |>
    mutate(lower = body_mass_g * 0.95) |>
    mutate(upper = body_mass_g * 1.2) %>%
    gg_col(
      x = body_mass_g,
      xmin = lower,
      xmax = upper,
      y = species,
      col = species,
      width = 0.75,
      x_expand_limits = c(0, max(.$upper)),
      x_labels = \(x) x / 1000,
      x_title = "Body mass kg",
    ) +
    geom_errorbar(
      colour = "black",
      width = 0.1,
    )

  vdiffr::expect_doppelganger(test_name, p)
})



## ----fig.asp=0.4------------------------------------------------------------------------------------
test_name <- "19"

test_that(test_name, {
  p <- penguins |>
    group_by(species) |>
    summarise(body_mass_g = mean(body_mass_g, na.rm = TRUE)) |>
    mutate(lower = body_mass_g * 0.95) |>
    mutate(upper = body_mass_g * 1.2) |>
    gg_blanket(
      x = body_mass_g,
      y = species,
      col = species,
      xmin = lower,
      xmax = upper,
      width = 0.75,
      x_expand_limits = 0,
      x_labels = \(x) x / 1000,
      x_title = "Body mass kg",
    ) +
    geom_col(
      colour = "#d3d3d3",
      fill = "#d3d3d3",
      alpha = 0.9,
      width = 0.75,
    ) +
    geom_errorbar(
      width = 0.1,
    )

  vdiffr::expect_doppelganger(test_name, p)
})



## ---------------------------------------------------------------------------------------------------
test_name <- "20"

test_that(test_name, {
  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
      alpha = species,
      alpha_pal = c(1, 1, 0.33),
      mapping = aes(shape = species),
    )

  vdiffr::expect_doppelganger(test_name, p)
})



## ---------------------------------------------------------------------------------------------------
test_name <- "21"

test_that(test_name, {
  p <- penguins |>
    gg_blanket(
      geom = "bar",
      stat = "bin",
      position = "stack",
      x = flipper_length_mm,
      col = species,
    )

  vdiffr::expect_doppelganger(test_name, p)
})



## ---------------------------------------------------------------------------------------------------
test_name <- "22"

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


