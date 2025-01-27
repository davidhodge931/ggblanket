testthat::skip_if(getRversion() <= package_version("4.1.0"))
testthat::skip_on_os(c("mac", "linux"))

library(ggplot2)
library(dplyr)
set_blanket()
set_font_defaults()
set_reference_defaults()

## ---------------------------------------------------------------------------------------------------
test_name <- "gg_area"

test_that(test_name, {
  p <- economics |>
    gg_area(
      x = date,
      y = unemploy,
      y_label = "Unemployment",
    )
  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "gg_bar"

test_that(test_name, {
  p <- palmerpenguins::penguins |>
    dplyr::mutate(dplyr::across(sex, \(x) stringr::str_to_sentence(x))) |>
    gg_bar(
      y = species,
      col = sex,
      position = position_dodge(preserve = "single"),
      width = 0.75,
    )
  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "gg_bin_2d"

test_that(test_name, {
  p <- ggplot2::diamonds |>
    gg_bin_2d(
      x = carat,
      y = price,
    )
  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "gg_boxplot"

test_that(test_name, {
  p <- palmerpenguins::penguins |>
    tidyr::drop_na(sex) |>
    dplyr::mutate(dplyr::across(sex, \(x) stringr::str_to_sentence(x))) |>
    gg_boxplot(
      x = flipper_length_mm,
      y = sex,
      col = species,
      theme = light_mode_b(),
    )
  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "gg_col"

test_that(test_name, {
  p <- palmerpenguins::penguins |>
    tidyr::drop_na(sex) |>
    dplyr::mutate(dplyr::across(sex, \(x) stringr::str_to_sentence(x))) |>
    group_by(sex, species) |>
    summarise(dplyr::across(flipper_length_mm, \(x) mean(x, na.rm = TRUE))) |>
    gg_col(
      x = flipper_length_mm,
      y = species,
      col = sex,
      position = position_dodge(preserve = "single"),
      width = 0.75,
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "gg_contour"

test_that(test_name, {
  p <- ggplot2::faithfuld |>
    gg_contour(
      x = waiting,
      y = eruptions,
      z = density,
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "gg_contour_filled"

test_that(test_name, {
  p <- ggplot2::faithfuld |>
    gg_contour_filled(
      x = waiting,
      y = eruptions,
      z = density,
      bins = 8,
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "gg_crossbar"

test_that(test_name, {
  p <- data.frame(
    trt = factor(c(1, 1, 2, 2)),
    resp = c(1, 5, 3, 4),
    group = factor(c(1, 2, 1, 2)),
    upper = c(1.1, 5.3, 3.3, 4.2),
    lower = c(0.8, 4.6, 2.4, 3.6)) |>
    gg_crossbar(
      x = trt,
      y = resp,
      ymin = lower,
      ymax = upper,
      col = group,
      width = 0.5,
      x_label = "Treatment",
      y_label = "Response",
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "gg_density"

test_that(test_name, {
  p <- palmerpenguins::penguins |>
    dplyr::mutate(dplyr::across(sex, \(x) stringr::str_to_sentence(x))) |>
    tidyr::drop_na(sex) |>
    gg_density(
      x = flipper_length_mm,
      col = species,
      theme = light_mode_t(),
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "gg_density_2d"

test_that(test_name, {
  set.seed(123)

  p <- faithful |>
    gg_density_2d(
      x = waiting,
      y = eruptions,
      bins = 8,
      contour = TRUE,
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "gg_density_2d_filled"

test_that(test_name, {
  set.seed(123)

  p <- faithful |>
    gg_density_2d_filled(
      x = waiting,
      y = eruptions,
      bins = 8,
      contour = TRUE,
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "gg_errorbar"

test_that(test_name, {
  p <- data.frame(
    trt = factor(c(1, 1, 2, 2)),
    resp = c(1, 5, 3, 4),
    group = factor(c(1, 2, 1, 2)),
    upper = c(1.1, 5.3, 3.3, 4.2),
    lower = c(0.8, 4.6, 2.4, 3.6)
  ) |>
    gg_errorbar(
      x = trt,
      ymin = lower,
      ymax = upper,
      col = group,
      width = 0.1,
      x_label = "Treatment",
      y_label = "Response",
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "gg_freqpoly"

test_that(test_name, {
  p <- palmerpenguins::penguins |>
    dplyr::mutate(dplyr::across(sex, \(x) stringr::str_to_sentence(x))) |>
    gg_freqpoly(
      x = flipper_length_mm,
      col = sex,
      theme = light_mode_t(),
    ) +
    theme(legend.title = element_blank())

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "gg_function"

test_that(test_name, {
  p <- gg_function(
    fun = \(x) dnorm(x, mean = 0, sd = 5),
    x_expand_limits = qnorm(p = c(0.005, 0.995), mean = 0, sd = 5),
    y_expand_limits = 0,
  )

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "gg_hex"

test_that(test_name, {
  p <- ggplot2::diamonds |>
    gg_hex(
      x = carat,
      y = price,
      coord = coord_cartesian(clip = "on"),
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "gg_histogram"

test_that(test_name, {
  p <- palmerpenguins::penguins |>
    dplyr::mutate(dplyr::across(sex, \(x) stringr::str_to_sentence(x))) |>
    gg_histogram(
      x = flipper_length_mm,
      col = sex,
      facet = species,
      bins = 50,
      theme = light_mode_b(),
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
# test_name <- "gg_jitter"
#
# test_that(test_name, {
#   set.seed(123)
#
#   p <- palmerpenguins::penguins |>
#     gg_jitter(
#       x = species,
#       y = body_mass_g,
#       col = flipper_length_mm,
#       position = position_jitter(height = 0),
#       y_expand_limits = 0,
#       col_steps = TRUE,
#     )
#
#   vdiffr::expect_doppelganger(test_name, p)
# })

## ---------------------------------------------------------------------------------------------------
test_name <- "gg_label"

test_that(test_name, {
  p <- bind_rows(
    mtcars |> slice_min(order_by = mpg),
    mtcars |> slice_max(order_by = mpg)) |>
    tibble::rownames_to_column("model") |>
    gg_label(
      x = model,
      y = mpg,
      label = model,
      size = 3.53,
      y_expand_limits = 0,
      y_label = "Miles per gallon",
      # col_palette = c(orange, "white", teal),
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "gg_line"

test_that(test_name, {
  p <- economics |>
    gg_line(
      x = date,
      y = unemploy,
      y_expand_limits = 0,
      y_label = "Unemployment",
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "gg_linerange"

test_that(test_name, {
  p <- data.frame(
    trt = factor(c(1, 1, 2, 2)),
    resp = c(1, 5, 3, 4),
    group = factor(c(1, 2, 1, 2)),
    upper = c(1.1, 5.3, 3.3, 4.2),
    lower = c(0.8, 4.6, 2.4, 3.6)) |>
    gg_linerange(
      x = trt,
      ymin = lower,
      ymax = upper,
      col = group,
      position = position_dodge(width = 0.2),
      x_label = "Treatment",
      y_label = "Response",
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "gg_path"

test_that(test_name, {
  p <- economics |>
    dplyr::mutate(unemploy_rate = unemploy / pop) |>
    gg_path(
      x = unemploy_rate,
      y = psavert,
      x_label = "Unemployment rate",
      y_expand_limits = 0,
      y_label = "Personal savings rate",
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "gg_point"

test_that(test_name, {
  p <- palmerpenguins::penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "gg_pointrange"

test_that(test_name, {
  p <- data.frame(
    trt = factor(c(1, 1, 2, 2)),
    resp = c(1, 5, 3, 4),
    group = factor(c(1, 2, 1, 2)),
    upper = c(1.1, 5.3, 3.3, 4.2),
    lower = c(0.8, 4.6, 2.4, 3.6)) |>
    gg_pointrange(
      x = trt,
      y = resp,
      col = group,
      ymin = lower,
      ymax = upper,
      position = position_dodge(width = 0.2),
      size = 0.2,
      x_label = "Treatment",
      y_label = "Response",
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "gg_polygon"

test_that(test_name, {
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

  p <- datapoly |>
    gg_polygon(
      x = x,
      y = y,
      col = value,
      group = id,
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "gg_qq"

test_that(test_name, {
  p <- palmerpenguins::penguins |>
    gg_qq(
      sample = body_mass_g,
      facet = species,
      coord = coord_cartesian(clip = "on"),
    ) +
    geom_qq_line(
      colour = blue,
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "gg_quantile"

test_that(test_name, {
  p <- palmerpenguins::penguins |>
    gg_quantile(
      x = flipper_length_mm,
      y = body_mass_g,
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "gg_raster"

test_that(test_name, {
  p <- ggplot2::faithfuld |>
    gg_raster(
      x = waiting,
      y = eruptions,
      col = density,
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "gg_rect"

test_that(test_name, {
  p <- data.frame(
    x = rep(c(2, 5, 7, 9, 12), 2),
    y = rep(c(1, 2), each = 5),
    z = factor(c(rep(1:4, each = 2), 5, NA)),
    w = rep(diff(c(0, 4, 6, 8, 10, 14)), 2)) |>
    dplyr::mutate(
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
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "gg_ribbon"

test_that(test_name, {
  p <- data.frame(year = 1875:1972, level = as.vector(LakeHuron)) |>
    mutate(level_min = level - 1, level_max = level + 1) |>
    gg_ribbon(
      x = year,
      ymin = level_min,
      ymax = level_max,
      colour = NA,
      x_labels = \(x) x,
      y_label = "Level",
    ) +
    geom_line(
      mapping = aes(y = level),
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
# test_name <- "gg_rug"
#
# test_that(test_name, {
#   p <- palmerpenguins::penguins |>
#     dplyr::mutate(dplyr::across(sex, \(x) stringr::str_to_sentence(x))) |>
#     gg_rug(
#       x = flipper_length_mm,
#       y = body_mass_g,
#       col = sex,
#     )
#
#   vdiffr::expect_doppelganger(test_name, p)
# })

## ---------------------------------------------------------------------------------------------------
test_name <- "gg_segment"

test_that(test_name, {
  p <- data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0) |>
    gg_segment(
      x = x1,
      xend = x2,
      y = y1,
      yend = y2,
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
# test_name <- "gg_sf"
#
# test_that(test_name, {
#   p <- sf::st_read(system.file("shape/nc.shp", package = "sf")) |>
#     gg_sf(
#       col = AREA,
#     )
#
#   vdiffr::expect_doppelganger(test_name, p)
# })

## ---------------------------------------------------------------------------------------------------
test_name <- "gg_smooth"

test_that(test_name, {
  p <- palmerpenguins::penguins |>
    dplyr::mutate(dplyr::across(sex, \(x) stringr::str_to_sentence(x))) |>
    tidyr::drop_na(sex) |>
    gg_smooth(
      x = flipper_length_mm,
      y = body_mass_g,
      col = sex,
      se = TRUE,
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "gg_step"

test_that(test_name, {
  p <- economics |>
    filter(date > lubridate::ymd("2010-01-01")) |>
    gg_step(
      x = date,
      y = unemploy,
      y_expand_limits = 0,
      y_label = "Unemployment",
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "gg_text"

test_that(test_name, {
  p <- bind_rows(
    mtcars |> slice_min(order_by = mpg),
    mtcars |> slice_max(order_by = mpg)) |>
    tibble::rownames_to_column("model") |>
    gg_text(
      x = model,
      y = mpg,
      label = model,
      size = 3.53,
      y_expand_limits = 0,
      y_label = "Miles per gallon",
      col_palette = c(orange, "white", teal),
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "gg_tile"

test_that(test_name, {
  p <- palmerpenguins::penguins |>
    dplyr::mutate(dplyr::across(sex, \(x) stringr::str_to_sentence(x))) |>
    group_by(species, sex) |>
    summarise(flipper_length_mm = mean(flipper_length_mm, na.rm = TRUE)) |>
    gg_tile(
      x = sex,
      y = species,
      col = flipper_length_mm,
    )

  vdiffr::expect_doppelganger(test_name, p)
})



## ---------------------------------------------------------------------------------------------------
test_name <- "gg_violin"

test_that(test_name, {
  p <- palmerpenguins::penguins |>
    tidyr::drop_na(sex) |>
    dplyr::mutate(dplyr::across(sex, \(x) stringr::str_to_sentence(x))) |>
    gg_violin(
      x = sex,
      y = body_mass_g,
      col = species,
    )

  vdiffr::expect_doppelganger(test_name, p)
})

## ---------------------------------------------------------------------------------------------------
test_name <- "gg_blanket"

test_that(test_name, {
  p <- palmerpenguins::penguins |>
    tidyr::drop_na(sex) |>
    dplyr::mutate(dplyr::across(sex, \(x) stringr::str_to_sentence(x))) |>
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

set_blanket()
