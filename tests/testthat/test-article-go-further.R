testthat::skip_if(getRversion() <= package_version("4.1.0"))
testthat::skip_on_os(c("mac", "linux"))

## ----setup------------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(palmerpenguins)
library(patchwork)
blankify()

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
    mutate(across(color, \(x) x |>
                    forcats::fct_reorder(n) |>
                    forcats::fct_rev())) |>
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
    gg_pointrange(
      stat = "summary",
      x = species,
      y = flipper_length_mm,
      size = 0.1,
    )

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
    mutate(hack = "") |>
    gg_boxplot(
      x = species,
      y = flipper_length_mm,
      col = hack,
      colour = "black", #or fill = #D3D3D3",
      width = 0.5,
      alpha = 0.9,
      mode = light_mode_n(),
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
    ) +
    geom_text(
      mapping = aes(label = n, !!!aes_contrast(lightness)),
      position = position_dodge2(width = 0.75, preserve = "single"),
      vjust = 1.33,
      show.legend = FALSE,
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
    ) +
    geom_text(
      mapping = aes(label = n, !!!aes_contrast(lightness)),
      position = position_dodge2(width = 0.75, preserve = "single"),
      hjust = 1.33,
      show.legend = FALSE,
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
      y_breaks = scales::breaks_pretty(7),
      y_labels = \(x) replace_seq(x),
    )

  vdiffr::expect_doppelganger(test_name, p)
})



## ---------------------------------------------------------------------------------------------------
test_name <- "10"

test_that(test_name, {
  p <- data.frame(
    age = c(0:9, 0:9),
    sex = c(rep("Male", 10), rep("Female", 10)),
    population = c(200, 250, 300, 350, 440, 450, 500, 550, 600, 650,
                   190, 240, 290, 330, 420, 430, 480, 530, 580, 630)) |>
    mutate(population = ifelse(sex == "Female", -population, population)) %>%
    gg_col(
      y = age,
      x = population,
      col = sex,
      width = 1,
      orientation = "y",
      x_labels = \(x) abs(x),
      x_include = max(abs(.$population)) * c(-1, 1),
      y_limits = c(NA, NA),
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
    population = c(200, 250, 300, 350, 440, 450, 500, 550, 600, 650,
                   190, 240, 290, 330, 420, 430, 480, 530, 580, 630)) |>
    mutate(population = ifelse(sex == "Female", -population, population)) %>%
    gg_col(
      y = age,
      x = population,
      col = sex,
      width = 1,
      orientation = "y",
      x_labels = \(x) abs(x),
      x_include = max(abs(.$population)) * c(-1, 1),
      y_limits = c(NA, NA),
    ) +
    geom_vline(
      xintercept = 0,
      colour = "#121b24",
      linewidth = 10 / 33
    ) +
    light_mode_r()
  # ggeasy::easy_remove_y_gridlines() +
  # ggeasy::easy_remove_y_axis()

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



## ---------------------------------------------------------------------------------------------------
test_name <- "14"

test_that(test_name, {
  p1 <- economics |>
    gg_smooth(
      x = date,
      y = unemploy,
      subtitle = "\nNo x_limits set",
      se = TRUE) +
    geom_vline(xintercept = c(lubridate::ymd("1985-01-01", "1995-01-01")),
               col = blue,
               linetype = 3) +
    geom_point(col = blue, alpha = 0.3)

  p2 <- economics |>
    filter(between(date, lubridate::ymd("1985-01-01"), lubridate::ymd("1995-01-01"))) |>
    gg_smooth(
      x = date,
      y = unemploy,
      se = TRUE,
      x_labels = \(x) stringr::str_sub(x, 3, 4),
      subtitle = "\nx data filtered") +
    geom_point(col = blue, alpha = 0.3)

  p <- p1 + p2

  vdiffr::expect_doppelganger(test_name, p)
})



## ---------------------------------------------------------------------------------------------------
test_name <- "15"

test_that(test_name, {
  p <- economics |>
    gg_smooth(
      x = date,
      y = unemploy,
      se = TRUE,
      x_limits = c(lubridate::ymd("1985-01-01", "1995-01-01")),
      x_labels = \(x) stringr::str_sub(x, 3, 4),
      subtitle = "\nx_limits set",
    ) +
    geom_point(col = blue, alpha = 0.3)

  vdiffr::expect_doppelganger(test_name, p)
})



## ---------------------------------------------------------------------------------------------------
test_name <- "16"

test_that(test_name, {
  p4 <- economics |>
    gg_smooth(
      x = date,
      y = unemploy,
      se = TRUE,
      x_limits = c(lubridate::ymd("1985-01-01", "1995-01-01")),
      x_labels = \(x) stringr::str_sub(x, 3, 4),
      coord = coord_cartesian(clip = "on"),
      subtitle = "\nx_limits set & cartesian space clipped") +
    geom_point(col = blue, alpha = 0.3)

  p5 <- economics |>
    gg_smooth(
      x = date,
      y = unemploy,
      se = TRUE,
      x_limits = c(lubridate::ymd("1985-01-01", "1995-01-01")),
      x_labels = \(x) stringr::str_sub(x, 3, 4),
      x_oob = scales::oob_censor,
      subtitle = "\nx_limits set & x_oob censored") +
    geom_point(col = blue, alpha = 0.3)


  p <- p4 + p5

  vdiffr::expect_doppelganger(test_name, p)
})



## ---------------------------------------------------------------------------------------------------
test_name <- "17"

test_that(test_name, {
  p <- mpg |>
    mutate(centred = cty - mean(cty)) |>
    select(displ, hwy, centred) %>%
    gg_point(
      x = displ,
      y = hwy,
      col = centred,
      col_pal = c(blue, greys[1], plum),
      col_breaks = scales::breaks_width(5),
      col_rescale = scales::rescale(c(min(.$centred), 0, max(.$centred)))
    )

  vdiffr::expect_doppelganger(test_name, p)
})



## ---------------------------------------------------------------------------------------------------
test_name <- "18"

test_that(test_name, {
  p <- mpg |>
    mutate(centred = cty - mean(cty)) |>
    select(displ, hwy, centred) %>%
    gg_point(
      x = displ,
      y = hwy,
      col = centred,
      col_pal = c(blue, greys[1], plum),
      col_limits = max(abs(.$centred)) * c(-1, 1),
      col_breaks = scales::breaks_width(5)
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "19"

test_that(test_name, {
  p1 <- pressure |>
    gg_point(
      x = temperature,
      y = pressure,
      x_labels = replace_seq,
      y_labels = replace_seq,
      subtitle = "\nDefault",
    )

  p2 <- pressure |>
    gg_point(
      x = temperature,
      y = pressure,
      x_labels = replace_seq,
      y_transform = "reverse",
      y_labels = replace_seq,
      subtitle = "\nReverse",
    )

  p3 <- pressure |>
    gg_point(
      x = temperature,
      y = pressure,
      x_labels = replace_seq,
      y_transform = "log10",
      subtitle = "\nLog10",
    )

  p4 <- pressure |>
    gg_point(
      x = temperature,
      y = pressure,
      x_labels = replace_seq,
      y_transform = c("log10", "reverse"),
      subtitle = "\nLog10 & Reverse",
    )

  p <- (p1 + p2) / (p3 + p4)

  vdiffr::expect_doppelganger(test_name, p)
})


