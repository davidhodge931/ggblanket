testthat::skip_if(getRversion() <= package_version("4.1.0"))
testthat::skip_on_os(c("mac", "linux"))

library(ggplot2)
library(dplyr)
library(hms)

set_blanket()

test_that("discrete variables", {
  p <- penguins |>
    gg_jitter(
      x = species,
      y = island,
      col = sex,
    )
  vdiffr::expect_doppelganger("discrete - gg_jitter", p)
})

test_that("ordinal col variable", {
  p <- faithfuld |>
    gg_contour_filled(
      x = waiting,
      y = eruptions,
      z = density,
      bins = 8,
    )
  vdiffr::expect_doppelganger("ordinal - gg_contour_filled", p)
})

test_that("date variables", {
  set.seed(123)
  t <- as.POSIXct("2021-01-01")
  e1 <- as.POSIXct("2021-12-31 23:59:59")
  n1 <- as.numeric(lubridate::as.duration(e1 - t))
  s1 <- sample(seq.int(n1), 3000L)

  df <- tibble::tibble(dttm = t + s1) |>
    dplyr::mutate(mon = lubridate::floor_date(dttm, "month")) |>
    dplyr::summarise(total = dplyr::n(), .by = "mon") |>
    dplyr::mutate(mon = lubridate::as_date(mon))

  p <- df |> gg_point(x = mon, y = mon, col = mon)
  vdiffr::expect_doppelganger("date - gg_point", p)
})

test_that("datetime variables", {
  set.seed(123)
  t <- as.POSIXct("2021-01-01")
  e1 <- as.POSIXct("2021-12-31 23:59:59")
  n1 <- as.numeric(lubridate::as.duration(e1 - t))
  s1 <- sample(seq.int(n1), 3000L)

  df <- tibble::tibble(dttm = t + s1) |>
    dplyr::mutate(mon = lubridate::floor_date(dttm, "month")) |>
    dplyr::summarise(total = dplyr::n(), .by = "mon")

  p <- df |> gg_point(x = mon, y = mon, col = mon)
  vdiffr::expect_doppelganger("datetime - gg_point", p)
})

test_that("hms variables", {
  d <- data.frame(hours = 1:20)
  d$hours <- hms::hms(hours = d$hours)

  df <- tibble::tibble(x = d$hours, y = as.numeric(d$hours))

  p <- df |> gg_point(x = x, y = x, col = x)
  vdiffr::expect_doppelganger("time - hms gg_point", p)
})
