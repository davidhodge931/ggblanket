# test-histogram-consistency.R

test_that("gg_histogram produces same output as ggplot2 histogram", {
  # Create ggplot2 version
  p1_plot <- palmerpenguins::penguins |>
    ggplot2::ggplot() +
    ggplot2::geom_histogram(
      ggplot2::aes(x = flipper_length_mm, y = ggplot2::after_stat(ncount)),
      bins = 50
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0))

  # Create gg_histogram version
  p2_plot <- palmerpenguins::penguins |>
    gg_histogram(
      x = flipper_length_mm,
      mapping = ggplot2::aes(y = ggplot2::after_stat(ncount)),
      bins = 50
    )

  # Extract built data
  p1_data <- ggplot2::ggplot_build(p1_plot)$data[[1]]
  p2_data <- ggplot2::ggplot_build(p2_plot)$data[[1]]

  # Compare data structure
  expect_equal(names(p1_data), names(p2_data))
  expect_equal(nrow(p1_data), nrow(p2_data))

  # Compare key computed values
  expect_equal(p1_data$x, p2_data$x)
  expect_equal(p1_data$y, p2_data$y)
  expect_equal(p1_data$count, p2_data$count)
  expect_equal(p1_data$ncount, p2_data$ncount)
  expect_equal(p1_data$density, p2_data$density)
  expect_equal(p1_data$ndensity, p2_data$ndensity)

  # Compare bins
  expect_equal(p1_data$xmin, p2_data$xmin)
  expect_equal(p1_data$xmax, p2_data$xmax)

  # Compare aesthetics
  expect_equal(p1_data$colour, p2_data$colour)
  expect_equal(p1_data$fill, p2_data$fill)
  expect_equal(p1_data$alpha, p2_data$alpha)

  # Reset blanket settings
  set_blanket()
})

