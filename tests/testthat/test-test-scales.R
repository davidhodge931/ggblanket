library(testthat)
library(vdiffr)
library(ggplot2)
library(palmerpenguins)

# Breaks tests
test_that("breaks_cont_fn", {
  expect_doppelganger(
    "breaks_cont_fn",
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
})

test_that("breaks_disc_vec", {
  expect_doppelganger(
    "breaks_disc_vec",
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
})

test_that("breaks_disc_named", {
  expect_doppelganger(
    "breaks_disc_named",
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

test_that("breaks_n", {
  expect_doppelganger(
    "breaks_n",
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

# Labels tests
test_that("labels_disc", {
  expect_doppelganger(
    "labels_disc",
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
})

test_that("labels_cont", {
  expect_doppelganger(
    "labels_cont",
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

# Expand tests
test_that("expand_disc_zero", {
  expect_doppelganger(
    "expand_disc_zero",
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
})

test_that("expand_cont_asym", {
  expect_doppelganger(
    "expand_cont_asym",
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

# Other scale tests
test_that("limits_include", {
  expect_doppelganger(
    "limits_include",
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

test_that("titles_all", {
  expect_doppelganger(
    "titles_all",
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

test_that("pos_disc_tr", {
  expect_doppelganger(
    "pos_disc_tr",
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
})

test_that("pos_cont_tr", {
  expect_doppelganger(
    "pos_cont_tr",
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

test_that("sec_axis_cont", {
  expect_doppelganger(
    "sec_axis_cont",
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
})

test_that("sec_axis_disc", {
  expect_doppelganger(
    "sec_axis_disc",
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

# Transform tests
test_that("trans_reverse", {
  expect_doppelganger(
    "trans_reverse",
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
})

test_that("trans_log", {
  expect_doppelganger(
    "trans_log",
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

# test_that("symmetric_x", {
#   expect_doppelganger(
#     "symmetric_x",
#     palmerpenguins::penguins |>
#       tidyr::drop_na() |>
#       gg_point(
#         x = bill_depth_mm,
#         y = bill_length_mm,
#         col = bill_length_mm,
#         symmetric = "x",
#       )
#   )
# })

# Legend tests
test_that("leg_rev_disc", {
  expect_doppelganger(
    "leg_rev_disc",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      gg_point(
        x = species,
        y = island,
        col = sex,
        col_legend_rev = TRUE
      )
  )
})

test_that("leg_rev_cont", {
  expect_doppelganger(
    "leg_rev_cont",
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

test_that("h_bar_leg", {
  expect_doppelganger(
    "h_bar_leg",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      gg_bar(
        y = species,
        col = sex
      )
  )
})

# Color tests
test_that("border_false", {
  expect_doppelganger(
    "border_false",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      gg_point(
        x = bill_depth_mm,
        y = bill_length_mm,
        col = bill_length_mm,
        col_border = FALSE
      )
  )
})

test_that("violin_jit_border", {
  set.seed(123)
  expect_doppelganger(
    "violin_jit_border",
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
})

test_that("violin_jit_no_bord", {
  set.seed(123)
  expect_doppelganger(
    "violin_jit_no_bord",
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

# Palette tests
test_that("pal_cont_viridis", {
  expect_doppelganger(
    "pal_cont_viridis",
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
})

test_that("pal_disc_violin", {
  expect_doppelganger(
    "pal_disc_violin",
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
})

test_that("pal_disc_viridis", {
  expect_doppelganger(
    "pal_disc_viridis",
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

# Legend layout tests
test_that("leg_nrow_2", {
  expect_doppelganger(
    "leg_nrow_2",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      gg_point(
        x = bill_depth_mm,
        y = bill_length_mm,
        col = species,
        col_legend_nrow = 2
      )
  )
})

test_that("leg_ncol_2", {
  expect_doppelganger(
    "leg_ncol_2",
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

test_that("leg_keep_all", {
  expect_doppelganger(
    "leg_keep_all",
    palmerpenguins::penguins |>
      tidyr::drop_na() |>
      dplyr::filter(species != "Adelie") |>
      gg_point(
        x = bill_depth_mm,
        y = bill_length_mm,
        col = species
      )
  )
})

test_that("leg_drop_unused", {
  expect_doppelganger(
    "leg_drop_unused",
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

# Steps scale tests
test_that("steps_breaks_n", {
  expect_doppelganger(
    "steps_breaks_n",
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
})

test_that("steps_breaks_width", {
  expect_doppelganger(
    "steps_breaks_width",
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

# Other tests
test_that("annotate_axis", {
  expect_doppelganger(
    "annotate_axis",
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

test_that("contour_filled", {
  expect_doppelganger(
    "contour_filled",
    faithfuld |>
      gg_contour_filled(
        x = waiting,
        y = eruptions,
        z = density
      )
  )
})
