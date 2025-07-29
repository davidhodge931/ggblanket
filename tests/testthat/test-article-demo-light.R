testthat::skip_if(getRversion() <= package_version("4.1.0"))
testthat::skip_on_os(c("mac", "linux"))

library(ggplot2)
library(dplyr)

set_blanket()

test_that("gg_area works", {
  economics |>
    gg_area(
      x = date,
      y = unemploy,
      y_title = "Unemployment",
    )
})

test_that("gg_bar works", {
  palmerpenguins::penguins |>
    gg_bar(
      position = position_dodge(preserve = "single"),
      y = species,
      col = sex,
      width = 0.75,
    )
})

test_that("gg_bin2d works", {
  diamonds |>
    gg_bin2d(
      x = carat,
      y = price,
      coord = coord_cartesian(ylim = c(0, NA)),
      y_expand = expansion(c(0, 0.01))
    )
})

test_that("gg_boxplot works", {
  palmerpenguins::penguins |>
    gg_boxplot(
      x = flipper_length_mm,
      y = sex,
      col = species,
    )
})

test_that("gg_col works", {
  palmerpenguins::penguins |>
    group_by(sex, species) |>
    summarise(across(flipper_length_mm, \(x) mean(x, na.rm = TRUE))) |>
    labelled::copy_labels_from(palmerpenguins::penguins) |>
    gg_col(
      position = position_dodge(preserve = "single"),
      x = flipper_length_mm,
      y = species,
      col = sex,
      width = 0.75,
    )
})

test_that("gg_contour works", {
  faithfuld |>
    gg_contour(
      x = waiting,
      y = eruptions,
      z = density,
      bins = 20,
    )
})

test_that("gg_contour_filled works", {
  faithfuld |>
    gg_contour_filled(
      x = waiting,
      y = eruptions,
      z = density,
      bins = 20,
    )
})

test_that("gg_crossbar works", {

  data.frame(
    trt = factor(c(1, 1, 2, 2)),
    resp = c(1, 5, 3, 4),
    group = factor(c(1, 2, 1, 2)),
    upper = c(1.1, 5.3, 3.3, 4.2),
    lower = c(0.8, 4.6, 2.4, 3.6)) |>
    labelled::set_variable_labels(
      trt = "Treatment",
      resp = "Response"
    ) |>
    gg_crossbar(
      x = trt,
      y = resp,
      ymin = lower,
      ymax = upper,
      col = group,
      width = 0.5,
      y_limits_include = 0,
    )
})

test_that("gg_density works", {
  palmerpenguins::penguins |>
    gg_density(
      x = flipper_length_mm,
      col = species,
      blend = "multiply",
    )
})

test_that("gg_density2d works", {
  faithful |>
    gg_density2d(
      x = waiting,
      y = eruptions,
      bins = 20,
    )
})

test_that("gg_density2d_filled works", {
  faithful |>
    gg_density2d_filled(
      x = waiting,
      y = eruptions,
      bins = 20,
    )
})

test_that("gg_errorbar works", {
  data.frame(
    trt = factor(c(1, 1, 2, 2)),
    resp = c(1, 5, 3, 4),
    group = factor(c(1, 2, 1, 2)),
    upper = c(1.1, 5.3, 3.3, 4.2),
    lower = c(0.8, 4.6, 2.4, 3.6)) |>
    labelled::set_variable_labels(
      trt = "Treatment",
      resp = "Response"
    ) |>
    gg_errorbar(
      x = trt,
      ymin = lower,
      ymax = upper,
      col = group,
      y_title = "Response",
      width = 0.05,
      y_limits_include = 0,
    )
})

test_that("gg_freqpoly works", {
  palmerpenguins::penguins |>
    gg_freqpoly(
      x = flipper_length_mm,
      col = sex,
    )
})

test_that("gg_function works", {
  mean <- 0
  sd <- 1

  gg_function(
    fun = \(x) dnorm(x, mean = mean, sd = sd),
    x_limits_include = qnorm(p = c(0.005, 0.995), mean = mean, sd = sd),
    y_symmetric = FALSE,
  )
})

test_that("gg_hex works", {
  diamonds |>
    gg_hex(
      x = carat,
      y = price,
      coord = coord_cartesian(ylim = c(0, NA)),
      y_expand = expansion(c(0, 0.01))
    )
})

test_that("gg_histogram works", {
  palmerpenguins::penguins |>
    gg_histogram(
      x = flipper_length_mm,
      col = sex,
    )
})

test_that("gg_jitter works", {
  set.seed(123)

  palmerpenguins::penguins |>
    gg_jitter(
      position = position_jitter(),
      x = species,
      y = body_mass_g,
      col = flipper_length_mm,
      y_limits_include = 0,
    )
})

test_that("gg_line works", {
  economics |>
    gg_line(
      x = date,
      y = unemploy,
      y_limits_include = 0,
      y_title = "Unemployment",
    )
})

test_that("gg_linerange works", {
  data.frame(
    trt = factor(c(1, 1, 2, 2)),
    resp = c(1, 5, 3, 4),
    group = factor(c(1, 2, 1, 2)),
    upper = c(1.1, 5.3, 3.3, 4.2),
    lower = c(0.8, 4.6, 2.4, 3.6)) |>
    labelled::set_variable_labels(
      trt = "Treatment",
      resp = "Response"
    ) |>
    gg_linerange(
      position = position_dodge(width = 0.2),
      x = trt,
      ymin = lower,
      ymax = upper,
      col = group,
      y_title = "Response",
      y_limits_include = 0,
    )
})

test_that("gg_path works", {
  economics |>
    mutate(unemploy_rate = unemploy / pop) |>
    gg_path(
      x = unemploy_rate,
      y = psavert,
      x_title = "Unemployment rate",
      y_limits_include = 0,
      y_title = "Personal savings rate",
    )
})

test_that("gg_point works", {
  palmerpenguins::penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
    )
})

test_that("gg_pointrange works", {
  data.frame(
    trt = factor(c(1, 1, 2, 2)),
    resp = c(1, 5, 3, 4),
    group = factor(c(1, 2, 1, 2)),
    upper = c(1.1, 5.3, 3.3, 4.2),
    lower = c(0.8, 4.6, 2.4, 3.6)) |>
    labelled::set_variable_labels(
      trt = "Treatment",
      resp = "Response"
    ) |>
    gg_pointrange(
      position = position_dodge(width = 0.2),
      x = trt,
      y = resp,
      col = group,
      ymin = lower,
      ymax = upper,
      y_limits_include = 0,
    )
})

test_that("gg_polygon works", {
  ids <- factor(c("1.1", "2.1", "1.2", "2.2", "1.3", "2.3"))

  values <- data.frame(
    id = ids,
    value = c(3, 3.1, 3.1, 3.2, 3.15, 3.5)
  )

  positions <- data.frame(
    id = rep(ids, each = 4),
    x = c(2, 1, 1.1, 2.2, 1, 0, 0.3, 1.1, 2.2, 1.1, 1.2, 2.5, 1.1, 0.3,
          0.5, 1.2, 2.5, 1.2, 1.3, 2.7, 1.2, 0.5, 0.6, 1.3),
    y = c(-0.5, 0, 1, 0.5, 0, 0.5, 1.5, 1, 0.5, 1, 2.1, 1.7, 1, 1.5,
          2.2, 2.1, 1.7, 2.1, 3.2, 2.8, 2.1, 2.2, 3.3, 3.2)
  )

  datapoly <- merge(values, positions, by = c("id"))

  datapoly |>
    gg_polygon(
      x = x,
      y = y,
      col = value,
      group = id,
    )
})

test_that("gg_qq works", {
  palmerpenguins::penguins |>
    gg_qq(
      sample = body_mass_g,
      col = species,
    )
})

test_that("gg_quantile works", {
  palmerpenguins::penguins |>
    gg_quantile(
      x = flipper_length_mm,
      y = body_mass_g,
    )
})

test_that("gg_raster works", {
  faithfuld |>
    gg_raster(
      x = waiting,
      y = eruptions,
      col = density,
    )
})

test_that("gg_rect works", {
  data.frame(
    x = rep(c(2, 5, 7, 9, 12), 2),
    y = rep(c(1, 2), each = 5),
    z = factor(c(rep(1:4, each = 2), 5, NA)),
    w = rep(diff(c(0, 4, 6, 8, 10, 14)), 2)) |>
    mutate(
      xmin = x - w / 2,
      xmax = x + w / 2,
      ymin = y,
      ymax = y + 1
    ) |>
    gg_rect(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      col = z,
      linetype = 1,
    )
})

test_that("gg_ribbon works", {
  data.frame(year = 1875:1972, level = as.vector(LakeHuron)) |>
    mutate(level_min = level - 1, level_max = level + 1) |>
    gg_ribbon(
      x = year,
      ymin = level_min,
      ymax = level_max,
      x_labels = \(x) x,
      y_title = "Level",
    )
})

test_that("gg_rug works", {
  palmerpenguins::penguins |>
    gg_rug(
      x = flipper_length_mm,
      y = body_mass_g,
      col = sex,
    )
})

test_that("gg_segment works", {
  data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0) |>
    gg_segment(
      x = x1,
      xend = x2,
      y = y1,
      yend = y2,
    )
})

test_that("gg_sf works", {
  sf::st_read(system.file("shape/nc.shp", package = "sf")) |>
    gg_sf(
      col = AREA,
    )
})

test_that("gg_smooth works", {
  palmerpenguins::penguins |>
    gg_smooth(
      x = flipper_length_mm,
      y = body_mass_g,
      col = sex,
      blend = "multiply",
    )
})

test_that("gg_step works", {
  economics |>
    filter(date > lubridate::ymd("2010-01-01")) |>
    gg_step(
      x = date,
      y = unemploy,
      y_limits_include = 0,
      y_title = "Unemployment",
    )
})

test_that("gg_tile works", {
  palmerpenguins::penguins |>
    group_by(species, sex) |>
    summarise(flipper_length_mm = mean(flipper_length_mm, na.rm = TRUE)) |>
    labelled::copy_labels_from(palmerpenguins::penguins) |>
    gg_tile(
      x = sex,
      y = species,
      col = flipper_length_mm,
    )
})

test_that("gg_violin works", {
  palmerpenguins::penguins |>
    gg_violin(
      x = sex,
      y = body_mass_g,
      col = species,
    )
})

test_that("gg_blanket works", {
  palmerpenguins::penguins |>
    gg_blanket(
      geom = "violin",
      stat = "ydensity",
      position = "dodge",
      x = sex,
      y = body_mass_g,
      col = species,
    )
})

set_blanket()
