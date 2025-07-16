library(testthat)
library(vdiffr)
library(ggplot2)
library(palmerpenguins)

# Assuming these functions are loaded from your package
# devtools::load_all()

test_that("breaks arguments work correctly", {
  # Test continuous breaks with scale function
  expect_doppelganger(
    "continuous breaks with scale function",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      gg_point(
        x = bill_depth_mm,
        y = bill_length_mm,
        col = bill_length_mm,
        x_breaks = scales::breaks_pretty(10),
        y_breaks = scales::breaks_pretty(10),
        col_breaks = scales::breaks_pretty(10)
      )
  )

  # Test discrete breaks with character vector
  expect_doppelganger(
    "discrete breaks character vector",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      gg_point(
        x = species,
        y = island,
        col = sex,
        x_breaks = c("Adelie", "Gentoo"),
        y_breaks = c("Dream", "Biscoe"),
        col_breaks = c("female")
      )
  )

  # Test discrete breaks with named vector
  expect_doppelganger(
    "discrete breaks named vector",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      gg_point(
        x = species,
        y = island,
        col = sex,
        x_breaks = c("A" = "Adelie", "G" = "Gentoo"),
        y_breaks = c("D" = "Dream", "B" = "Biscoe"),
        col_breaks = c("F" = "female")
      )
  )
})

test_that("breaks_n arguments work correctly", {
  expect_doppelganger(
    "continuous breaks_n",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      gg_point(
        x = bill_depth_mm,
        y = bill_length_mm,
        col = bill_length_mm,
        x_breaks_n = 10,
        y_breaks_n = 10,
        col_breaks_n = 10
      )
  )
})

test_that("label functions work correctly", {
  # Test labels on discrete scales
  expect_doppelganger(
    "discrete labels function",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      gg_point(
        x = species,
        y = island,
        col = sex,
        x_labels = \(x) paste0("$", x),
        y_labels = \(x) paste0("$", x),
        col_labels = \(x) paste0("$", x)
      )
  )

  # Test labels on continuous scales
  expect_doppelganger(
    "continuous labels function",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      gg_point(
        x = bill_depth_mm,
        y = bill_length_mm,
        col = bill_length_mm,
        x_labels = \(x) paste0("$", x),
        y_labels = \(x) paste0("$", x),
        col_labels = \(x) paste0("$", x)
      )
  )
})

test_that("expand arguments work correctly", {
  # Test expand on discrete scales
  expect_doppelganger(
    "discrete expand zero",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      gg_point(
        x = species,
        y = island,
        col = sex,
        x_expand = c(0, 0),
        y_expand = c(0, 0)
      )
  )

  # Test expand on continuous scales
  expect_doppelganger(
    "continuous expand asymmetric",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      gg_point(
        x = bill_depth_mm,
        y = bill_length_mm,
        col = bill_length_mm,
        x_expand = c(0, 0),
        y_expand = c(0.2, 0.2)
      )
  )
})

test_that("limits_include arguments work correctly", {
  expect_doppelganger(
    "continuous limits include",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      gg_point(
        x = bill_depth_mm,
        y = bill_length_mm,
        col = bill_length_mm,
        x_limits_include = 25,
        y_limits_include = 70,
        col_limits_include = 100
      )
  )
})

test_that("title arguments work correctly", {
  expect_doppelganger(
    "all titles",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      gg_point(
        x = bill_depth_mm,
        y = bill_length_mm,
        col = bill_length_mm,
        x_title = "X Blah",
        y_title = "Y Blah",
        col_title = "Col Blah",
        title = "Blah",
        subtitle = "Blah",
        caption = "Blah"
      )
  )
})

test_that("position arguments work correctly", {
  # Test position on discrete scales
  expect_doppelganger(
    "discrete position top right",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      gg_point(
        x = island,
        y = species,
        x_position = "top",
        y_position = "right",
        caption = ""
      )
  )

  # Test position on continuous scales
  expect_doppelganger(
    "continuous position top right",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      gg_point(
        x = bill_depth_mm,
        y = bill_length_mm,
        col = bill_length_mm,
        x_position = "top",
        y_position = "right",
        caption = ""
      )
  )
})

test_that("secondary axes work correctly", {
  # Test sec axes on continuous scales
  expect_doppelganger(
    "continuous secondary axes",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      gg_point(
        x = bill_depth_mm,
        y = bill_length_mm,
        col = bill_length_mm,
        x_sec_axis = dup_axis(),
        y_sec_axis = dup_axis()
      )
  )

  # Test sec axes on discrete scales
  expect_doppelganger(
    "discrete secondary axes",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      gg_point(
        x = species,
        y = island,
        x_sec_axis = dup_axis(),
        y_sec_axis = dup_axis()
      )
  )
})

test_that("transform arguments work correctly", {
  # Test reverse transform
  expect_doppelganger(
    "reverse transform all scales",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      gg_point(
        x = bill_depth_mm,
        y = bill_length_mm,
        col = bill_length_mm,
        x_transform = "reverse",
        y_transform = "reverse",
        col_transform = "reverse"
      )
  )

  # Test pseudo_log transform
  expect_doppelganger(
    "pseudo log transform",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      gg_point(
        x = bill_depth_mm,
        y = bill_length_mm,
        col = bill_length_mm,
        x_transform = "pseudo_log",
        y_transform = "pseudo_log",
        col_transform = "pseudo_log",
        x_limits_include = 0,
        y_limits_include = 0,
        col_limits_include = 0
      )
  )
})

test_that("symmetric axes work correctly", {
  expect_doppelganger(
    "symmetric x axis only",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      gg_point(
        x = bill_depth_mm,
        y = bill_length_mm,
        col = bill_length_mm,
        x_symmetric = TRUE,
        y_symmetric = FALSE
      )
  )
})

test_that("color legend reverse works correctly", {
  # Test discrete color legend reverse
  expect_doppelganger(
    "discrete color legend reverse",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      gg_point(
        x = species,
        y = island,
        col = sex,
        col_legend_rev = TRUE
      )
  )

  # Test continuous color legend reverse
  expect_doppelganger(
    "continuous color legend reverse",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      gg_point(
        x = bill_depth_mm,
        y = bill_length_mm,
        col = bill_length_mm,
        col_legend_rev = TRUE
      )
  )
})

test_that("horizontal legend order works correctly", {
  expect_doppelganger(
    "horizontal bar legend order",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      gg_bar(
        y = species,
        col = sex
      )
  )
})

test_that("color border works correctly", {
  # Test color border FALSE
  expect_doppelganger(
    "color border false",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      gg_point(
        x = bill_depth_mm,
        y = bill_length_mm,
        col = bill_length_mm,
        col_border = FALSE
      )
  )

  # Test color border with violin plot
  set.seed(123)
  expect_doppelganger(
    "violin with jitter default border",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      gg_violin(
        x = island,
        y = bill_length_mm,
        col = species
      ) +
      geom_jitter(
        position = position_jitterdodge(dodge.width = 0.9)
      )
  )

  set.seed(123)
  expect_doppelganger(
    "violin with jitter no border",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      gg_violin(
        x = island,
        y = bill_length_mm,
        col = species,
        col_border = FALSE
      ) +
      geom_jitter(
        position = position_jitterdodge(dodge.width = 0.9)
      )
  )
})

test_that("color palettes work correctly", {
  # Test continuous palettes
  expect_doppelganger(
    "continuous custom palettes",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      gg_point(
        x = bill_depth_mm,
        y = bill_length_mm,
        col = bill_length_mm,
        colour_palette = col_multiply(scales::pal_viridis(option = "F")),
        fill_palette = scales::pal_viridis(option = "F")
      )
  )

  # Test discrete palettes with violin
  expect_doppelganger(
    "discrete custom palettes violin",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      gg_violin(
        x = island,
        y = bill_length_mm,
        col = species,
        col_border = FALSE,
        colour_palette = col_multiply(scales::pal_hue(h.start = 15)),
        fill_palette = scales::pal_hue(h.start = 15)
      )
  )

  # Test discrete palettes with points
  expect_doppelganger(
    "discrete viridis palette",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      gg_point(
        x = bill_depth_mm,
        y = bill_length_mm,
        col = species,
        colour_palette = col_multiply(scales::pal_viridis()),
        fill_palette = scales::pal_viridis()
      )
  )
})

test_that("legend layout works correctly", {
  # Test legend nrow
  expect_doppelganger(
    "legend nrow 2",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      gg_point(
        x = bill_depth_mm,
        y = bill_length_mm,
        col = species,
        col_legend_nrow = 2
      )
  )

  # Test legend ncol
  expect_doppelganger(
    "legend ncol 2",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      gg_point(
        x = bill_depth_mm,
        y = bill_length_mm,
        col = species,
        col_legend_ncol = 2
      )
  )
})

test_that("legend drop works correctly", {
  # Test keeping all legend elements (default)
  expect_doppelganger(
    "legend keep all elements",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      dplyr::filter(species != "Adelie") |>
      gg_point(
        x = bill_depth_mm,
        y = bill_length_mm,
        col = species
      )
  )

  # Test dropping unused legend elements
  expect_doppelganger(
    "legend drop unused elements",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      dplyr::filter(species != "Adelie") |>
      gg_point(
        x = bill_depth_mm,
        y = bill_length_mm,
        col = species,
        col_drop = TRUE
      )
  )
})

test_that("color scale type steps works correctly", {
  # Test steps with breaks_n
  expect_doppelganger(
    "steps scale with breaks_n",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      gg_point(
        x = bill_depth_mm,
        y = bill_length_mm,
        col = bill_length_mm,
        col_scale_type = "steps",
        col_breaks_n = 10,
        col_labels = \(x) paste0("$", x),
        col_limits_include = 20,
        colour_palette = col_multiply(pal_viridis_by_panel(option = "F")),
        fill_palette = pal_viridis_by_panel(option = "F")
      )
  )

  # Test steps with breaks function
  expect_doppelganger(
    "steps scale with breaks width",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      gg_point(
        x = bill_depth_mm,
        y = bill_length_mm,
        col = bill_length_mm,
        col_scale_type = "steps",
        col_breaks = scales::breaks_width(2.5)
      )
  )
})

test_that("annotate_axis_line works correctly", {
  expect_doppelganger(
    "annotate axis line",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      gg_blanket(
        x = bill_depth_mm,
        y = bill_length_mm,
        col = bill_length_mm,
        y_breaks = scales::breaks_pretty(10)
      ) +
      annotate_axis_line() +
      geom_point()
  )
})

test_that("contour filled works correctly", {
  expect_doppelganger(
    "contour filled basic",
    faithfuld |>
      gg_contour_filled(
        x = waiting,
        y = eruptions,
        z = density
      )
  )
})
