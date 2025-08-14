library(testthat)
library(vdiffr)
library(ggplot2)
library(dplyr)
library(scales)

set_blanket()

## ---------------------------------------------------------------------------------------------------
test_name <- "labelled"

test_that(test_name, {
  p <- tibble::tibble(
    q1 = c("M", "F"),
    q2 = c(TRUE, FALSE),
    q3 = c(1, 2),
    q4 = factor(c(1, 2)),
  ) |>
    labelled::set_variable_labels(
      q1 = "Sex",
      q2 = "Happy",
      q3 = "Age",
      q4 = "Zero or one"
    ) |> #str()
    gg_point(
      x = q2,
      y = q1,
      col = q3,
      facet = q4,
    )

  vdiffr::expect_doppelganger(test_name, p)
})

set_blanket()
