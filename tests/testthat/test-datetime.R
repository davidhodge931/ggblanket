testthat::skip_if(getRversion() <= package_version("4.1.0"))
testthat::skip_on_os(c("mac", "linux"))

library(ggplot2)
library(dplyr)

set_blanket()

set.seed(123)
t <- as.POSIXct("2021-01-01")
e1 <- as.POSIXct("2021-12-31 23:59:59")
n1 <- as.numeric(lubridate::as.duration(e1 - t))
s1 <- sample(seq.int(n1), 3000L)

## ---------------------------------------------------------------------------------------------------
test_name <- "datetime"

test_that(test_name, {
  p <- tibble::tibble(dttm = t + s1) |>
    dplyr::mutate(mon = lubridate::floor_date(dttm, "month")) |>
    dplyr::summarise(total = dplyr::n(), .by = "mon") |>
    gg_point(
      x = mon,
      y = total,
    )


  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "time"

test_that(test_name, {
  p <- tibble::tibble(dttm = t + s1) |>
    dplyr::mutate(mon = lubridate::floor_date(dttm, "month")) |>
    dplyr::summarise(total = dplyr::n(), .by = "mon") |>
    dplyr::mutate(mon = hms::as_hms(mon)) |>
    gg_point(
      x = mon,
      y = total,
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "date"

test_that(test_name, {
  p <- tibble::tibble(dttm = t + s1) |>
    dplyr::mutate(mon = lubridate::floor_date(dttm, "month")) |>
    dplyr::summarise(total = dplyr::n(), .by = "mon") |>
    dplyr::mutate(mon = lubridate::as_date(mon)) |>
    gg_point(
      x = mon,
      y = total,
    )

  vdiffr::expect_doppelganger(test_name, p)
})
