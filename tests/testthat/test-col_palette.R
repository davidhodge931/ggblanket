testthat::skip_if(getRversion() <= package_version("4.1.0"))
testthat::skip_on_os(c("mac", "linux"))

library(ggplot2)
library(dplyr)
library(palmerpenguins)

set_blanket()

## ---------------------------------------------------------------------------------------------------
test_name <- "1"

test_that(test_name, {
  p <- faithfuld |>
    gg_contour_filled(
      x = waiting,
      y = eruptions,
      z = density,
      bins = 8,
    )

  vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "2"

test_that(test_name, {
  set.seed(123)

  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = species,
      col = bill_length_mm,
    )

  vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "3"

test_that(test_name, {
  set.seed(123)

  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = species,
      col = species,
    )

  vdiffr::expect_doppelganger(test_name, p)
})


test_name <- "4"

test_that(test_name, {
  p <- faithfuld |>
    gg_contour_filled(
      x = waiting,
      y = eruptions,
      z = density,
      bins = 8,
      col_palette = scales::pal_viridis(option = "A"),
    )

  vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "5"

test_that(test_name, {
  set.seed(123)

  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = species,
      col = bill_length_mm,
      col_palette = scales::pal_viridis(option = "A"),
    )

  vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "6"

test_that(test_name, {
  set.seed(123)

  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = species,
      col = species,
      col_palette = scales::pal_viridis(option = "A")
    )

  vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "7"

test_that(test_name, {
  set.seed(123)

  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = species,
      col = bill_length_mm,
      col_palette = viridisLite::magma(n = 20)
    )

  vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "8"

test_that(test_name, {
  set.seed(123)

  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = species,
      col = species,
      col_palette = c("red", "blue", "green")
    )

  vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "9"

test_that(test_name, {
  set.seed(123)

  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = species,
      col = species,
      col_palette = c("Gentoo" = "red", "Adelie" = "blue", "Chinstrap" = "green")
    )

  vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "10"

test_that(test_name, {
  set.seed(123)

  p <- faithfuld |>
    gg_contour_filled(
      x = waiting,
      y = eruptions,
      z = density,
      bins = 8,
      col_palette = viridisLite::magma(n = 8)
    )

  vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "11"

test_that(test_name, {
  set.seed(123)

  p <- data.frame(
    trt = factor(c(1, 1, 2, 2)),
    resp = c(1, 5, 3, 4),
    group = factor(c(1, 2, 1, 2)),
    upper = c(1.1, 5.3, 3.3, 4.2),
    lower = c(0.8, 4.6, 2.4, 3.6)
  ) |>
    gg_linerange(
      x = trt,
      ymin = lower,
      ymax = upper,
      mapping = aes(colour = group),
      position = position_dodge(width = 0.2),
      x_label = "Treatment",
      y_label = "Response",
    )

  vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "12"

test_that(test_name, {
  set.seed(123)

  p <- mtcars %>%
    dplyr::mutate(
      cyl = factor(cyl, levels = c("4", "6", "8", "10"))
    ) |>
    gg_point(
      x = mpg,
      y = wt,
      col = cyl,
    )

  vdiffr::expect_doppelganger(test_name, p)
})




set_blanket()
