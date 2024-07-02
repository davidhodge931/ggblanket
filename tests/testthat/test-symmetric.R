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
      x_symmetric = FALSE,
      y_symmetric = TRUE,
      mode_orientation = NULL,
    )

  vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "2"

test_that(test_name, {

  p <- penguins |>
    gg_histogram(
      y = flipper_length_mm,
      x_symmetric = TRUE,
      y_symmetric = FALSE,
    )

  vdiffr::expect_doppelganger(test_name, p)
})
