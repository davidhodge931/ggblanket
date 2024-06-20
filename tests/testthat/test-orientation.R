testthat::skip_if(getRversion() <= package_version("4.1.0"))
testthat::skip_on_os(c("mac", "linux"))

library(palmerpenguins)
library(ggplot2)

set_blanket()

test_name <- "1"

test_that(test_name, {

  p <- penguins |>
    gg_histogram(
      y = flipper_length_mm,
      y_orientation = FALSE,
    )

  vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "2"

test_that(test_name, {

  p <- penguins |>
    gg_histogram(
      y = flipper_length_mm,
      x_orientation = TRUE,
    )

  vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "3"

test_that(test_name, {

  p <- penguins |>
    gg_histogram(
      y = flipper_length_mm,
      x_symmetric = FALSE,
      y_symmetric = TRUE,
    )

  vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "4"

test_that(test_name, {

  p <- penguins |>
    gg_histogram(
      y = flipper_length_mm,
      x_symmetric = TRUE,
      y_symmetric = FALSE,
    )

  vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "5"

test_that(test_name, {

  p <- penguins |>
    gg_histogram(
      y = flipper_length_mm,
      x_symmetric = TRUE,
      y_symmetric = TRUE,
    )

  vdiffr::expect_doppelganger(test_name, p)
})
