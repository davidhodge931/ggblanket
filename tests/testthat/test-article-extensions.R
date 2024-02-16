## ----setup------------------------------------------------------------------------------------------
library(ggblanket)
library(ggplot2)
library(dplyr)
library(stringr)
library(palmerpenguins)


## ----fig.asp=0.9------------------------------------------------------------------------------------
library(patchwork)

test_name <- "patchwork"

test_that(test_name, {
  p1 <- mtcars |>
    gg_point(
      x = mpg,
      y = disp,
      y_labels = \(x) replace_seq(x),
    ) +
    facet_wrap(~"Plot 1")

  p2 <- mtcars |>
    gg_boxplot(
      x = gear,
      y = disp,
      group = gear,
      width = 0.5,
      y_labels = \(x) replace_seq(x),
    ) +
    facet_wrap(~"Plot 2")

  p3 <- mtcars |>
    gg_smooth(
      x = disp,
      y = qsec,
      x_breaks = scales::breaks_pretty(4),
    ) +
    facet_wrap(~"Plot 3")

  p <- (p1 + p2) / p3 +
    plot_annotation(
      title = "A collection of plots patched together",
      subtitle = "Works so nice!",
      theme = light_mode_rt() +
        theme(
          plot.title = element_text(margin = margin(t = -11)),
          plot.subtitle = element_text(margin = margin(t = 5.5)),
        )
    )

  vdiffr::expect_doppelganger(test_name, p)
})



## ---------------------------------------------------------------------------------------------------
test_name <- "ggalluvial"

test_that(test_name, {
  library(ggalluvial)

  titanic_wide <- p <- data.frame(Titanic)

  p <- titanic_wide |>
    gg_blanket(
      geom = "alluvium",
      stat = "alluvium",
      y = Freq,
      col = Survived,
      mapping = aes(axis1 = Class, axis2 = Sex, axis3 = Age),
      flipped = FALSE,
      x_breaks = c(1, 2, 3),
      x_labels = c("Class", "Sex", "Age"),
      y_limits = c(NA, NA),
      col_pal = c(orange, teal),
    ) +
    geom_stratum(
      colour = "black",
      fill = "#fcfdfe",
      show.legend = FALSE,
    ) +
    geom_text(
      mapping = aes(label = after_stat(stratum)),
      stat = "stratum",
      colour = "black",
      show.legend = FALSE,
    ) +
    guides(colour = "none") +
    ggeasy::easy_remove_y_axis()

  vdiffr::expect_doppelganger(test_name, p)
})



## ---------------------------------------------------------------------------------------------------
test_name <- "ggblend"

test_that(test_name, {
  p <- penguins |>
    gg_blanket(
      x = flipper_length_mm,
      col = species,
      stat = "density",
      col_pal = RColorBrewer::brewer.pal(3, "Set1"),
    ) +
    geom_density(
      alpha = 0.5,
    ) |>
    ggblend::blend(blend = "multiply")

  vdiffr::expect_doppelganger(test_name, p)
})



## ---------------------------------------------------------------------------------------------------
test_name <- "ggdensity_1"

test_that(test_name, {
  library(ggdensity)
  set.seed(123)

  p <- data.frame(
    x = rnorm(1000),
    y = rnorm(1000),
    z = c(rep("A", times = 500), rep("B", times = 500))
  ) |>
    gg_blanket(
      geom = "hdr",
      stat = "hdr",
      x = x,
      y = y,
      col = z,
      facet = z,
      colour = NA,
    )

  vdiffr::expect_doppelganger(test_name, p)
})



## ---------------------------------------------------------------------------------------------------
test_name <- "ggdensity_2"

test_that(test_name, {

  library(ggdensity)
  set.seed(123)

  p <- data.frame(
    x = rnorm(1000),
    y = rnorm(1000),
    z = c(rep("A", times = 500), rep("B", times = 500))
  ) |>
    gg_blanket(
      geom = "hdr",
      stat = "hdr",
      x = x,
      y = y,
      col_pal = scales::alpha(blue, 0),
    )

  vdiffr::expect_doppelganger(test_name, p)
})


## ---------------------------------------------------------------------------------------------------
test_name <- "ggdist_1"

test_that(test_name, {
  library(ggdist)
  library(distributional)

  p <- data.frame(
    distribution = c("Gamma(2,1)", "Normal(5,1)", "Mixture"),
    values = c(
      dist_gamma(2, 1),
      dist_normal(5, 1),
      dist_mixture(
        dist_gamma(2, 1),
        dist_normal(5, 1),
        weights = c(0.4, 0.6))
    )) |>
    gg_blanket(
      geom = "slabinterval",
      stat = "slabinterval",
      y = distribution,
      mapping = aes(dist = values),
      y_expand = c(0.05, 0.05),
      alpha_pal = 0.5,
      theme = light_mode_n(),
      # show_slab = FALSE,
      # show_interval = FALSE,
      # side = "both",
    )

  vdiffr::expect_doppelganger(test_name, p)
})

test_name <- "ggdist_2"

test_that(test_name, {
  library(ggdist)

  p <- tibble(x = 1:10) |>
    group_by(across(everything()))  |>
    do(tibble(y = rnorm(100, .$x))) |>
    median_qi(.width = c(0.5, 0.8, 0.95)) |>
    mutate(.width = factor(.width)) |>
    gg_blanket(
      geom = "lineribbon",
      x = x,
      y = y,
      ymin = .lower,
      ymax = .upper,
      col = .width,
      colour = "black",
      col_legend_rev = TRUE,
      col_pal = rev(RColorBrewer::brewer.pal(n = 3, "Blues")),
    )

  vdiffr::expect_doppelganger(test_name, p)
})


## ---------------------------------------------------------------------------------------------------
test_name <- "ggeasy"

test_that(test_name, {
  library(ggeasy)

  p <- penguins |>
    mutate(across(sex, \(x) str_to_sentence(x))) |>
    gg_jitter(
      x = species,
      y = body_mass_g,
      col = sex,
    ) +
    light_mode_t() +
    easy_remove_y_gridlines() +
    easy_remove_x_axis(what = c("line", "ticks")) +
    easy_remove_legend_title()

  vdiffr::expect_doppelganger(test_name, p)
})



## ---------------------------------------------------------------------------------------------------
test_name <- "ggforce_1"

test_that(test_name, {
  library(ggforce)

  p <- penguins |>
    gg_blanket(
      geom = "mark_hull",
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
      coord = coord_cartesian(),
      alpha_pal = 0.5,
    ) +
    geom_point()

  vdiffr::expect_doppelganger(test_name, p)
})



## ---------------------------------------------------------------------------------------------------
test_name <- "ggforce_2"

test_that(test_name, {
  p <- economics |>
    slice_head(n = 35) |>
    gg_path(
      stat = "bspline",
      x = date,
      y = unemploy,
      n = 100,
      linewidth = 1,
    )

  vdiffr::expect_doppelganger(test_name, p)
})



## ---------------------------------------------------------------------------------------------------
test_name <- "ggforce_3"

test_that(test_name, {
  p <- iris |>
    mutate(across(Species, \(x) str_to_sentence(x))) |>
    gg_blanket(
      geom = "shape",
      stat = "voronoi_tile",
      x = Sepal.Length,
      y = Sepal.Width,
      col = Species,
      group = 1,
      colour = "#fcfdfe",
    )

  vdiffr::expect_doppelganger(test_name, p)
})



## ---------------------------------------------------------------------------------------------------
test_name <- "ggh4x"

test_that(test_name, {
  p <- iris |>
    rename_with(\(x) snakecase::to_snake_case(x)) |>
    mutate(across(species, \(x) str_to_sentence(x))) |>
    mutate(size = if_else(species == "Setosa", "Short Leaves", "Long Leaves")) |>
    gg_point(
      x = sepal_width,
      y = sepal_length,
      x_breaks = scales::breaks_pretty(n = 3),
    ) +
    ggh4x::facet_nested(
      cols = vars(size, species),
      nest_line = TRUE,
      solo_line = TRUE,
    ) +
    theme(strip.text.x = element_text(margin = margin(t = 2.5, b = 5)))

  vdiffr::expect_doppelganger(test_name, p)
})



## ---------------------------------------------------------------------------------------------------
test_name <- "gghighlight_1"

test_that(test_name, {
  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
    ) +
    gghighlight::gghighlight(body_mass_g >= 5000)

  vdiffr::expect_doppelganger(test_name, p)
})



## ---------------------------------------------------------------------------------------------------
test_name <- "gghighlight_2"

test_that(test_name, {
  p <- penguins |>
    gg_histogram(
      x = flipper_length_mm,
      col = species,
      x_breaks = scales::breaks_pretty(4),
      col_pal = rep(blue, 3),
    ) + #build the scale for not faceted (i.e. all data)
    facet_wrap(~species) + #then add facet layer
    gghighlight::gghighlight()

  vdiffr::expect_doppelganger(test_name, p)
})



## ---------------------------------------------------------------------------------------------------
test_name <- "ggnewscale"

test_that(test_name, {
  p <- penguins |>
    gg_blanket(
      x = flipper_length_mm,
      y = body_mass_g,
    ) +
    geom_point(
      mapping = aes(colour = bill_length_mm),
      data = p <- penguins |> filter(species == "Gentoo"),
    ) +
    scale_color_viridis_c(option = "G", direction = -1) +
    labs(colour = "Gentoo\nBill length (mm)") +
    ggnewscale::new_scale_colour() +
    geom_point(
      mapping = aes(x = flipper_length_mm, y = body_mass_g, colour = species),
      data = p <- penguins |> filter(species != "Gentoo"),
    ) +
    scale_color_manual(values = c(plum, orange)) +
    labs(colour = "Species")

  vdiffr::expect_doppelganger(test_name, p)
})



## ---------------------------------------------------------------------------------------------------
test_name <- "ggrepel"

test_that(test_name, {
  p <- mtcars |>
    tibble::rownames_to_column("car") |>
    filter(wt > 2.75, wt < 3.45) |>
    gg_point(
      x = wt,
      y = mpg,
      label = car,
    ) +
    ggrepel::geom_text_repel(
      colour = blue,
      size = 3.53,
    )

  vdiffr::expect_doppelganger(test_name, p)
})



## ---------------------------------------------------------------------------------------------------
test_name <- "ggridges"

test_that(test_name, {
  library(ggridges)

  p <- ggridges::Catalan_elections |>
    rename_with(snakecase::to_snake_case) |>
    mutate(year = factor(year)) |>
    gg_blanket(
      geom = "density_ridges",
      stat = "density_ridges",
      coord = coord_cartesian(),
      x = percent,
      y = year,
      col = option,
      colour = "white",
      x_limits = c(0, 100),
      y_expand = expansion(c(0, 0.125)),
      alpha_pal = 0.7,
    )

  vdiffr::expect_doppelganger(test_name, p)
})



## ----fig.asp=0.75-----------------------------------------------------------------------------------
test_name <- "ggtext"

test_that(test_name, {
  p <- penguins |>
    gg_point(
      x = bill_depth_mm,
      y = bill_length_mm,
      col = species,
      facet = island,
      facet2 = sex,
      x_labels = \(x) glue::glue("_{x}_"),
      x_title = "**Bill depth** (mm)",
      y_labels = \(x) replace_seq(glue::glue("_{x}_")),
      y_title = "**Bill length** (mm)",
      facet_labels = \(x) glue::glue("_{x}_"),
      title = "***Pygoscelis*** *penguin* bill **lengths** and depths",
      subtitle = "<span style = 'color: #2596be ;'>**Adelie**</span>,
       <span style = 'color:#fc7c24;'>**Chinstrap**</span>,
       *and* <span style = 'color:#9c1e74;'>**Gentoo**</span>",
      theme = mdthemes::as_md_theme(light_mode_n())
    )

  vdiffr::expect_doppelganger(test_name, p)
})



## ---------------------------------------------------------------------------------------------------
test_name <- "geomtextpath"

test_that(test_name, {
  library(geomtextpath)

  p <- expand.grid(x = 1:nrow(volcano), y = 1:ncol(volcano)) |>
    mutate(z = c(volcano)) |>
    gg_contour_filled(
      x = x,
      y = y,
      z = z,
      binwidth = 20,
      x_expand = c(0, 0),
      x_limits = c(NA, NA),
      y_limits = c(NA, NA)) +
    geom_textcontour(
      aes(label = after_stat(level)),
      size = 2.5,
      straight = TRUE,
    ) +
    ggeasy::easy_remove_x_axis() +
    ggeasy::easy_remove_y_axis()

  vdiffr::expect_doppelganger(test_name, p)
})



## ---------------------------------------------------------------------------------------------------
test_name <- "ggiraph"

test_that(test_name, {
  p <- diamonds |>
    gg_blanket(
      x = color,
      y_expand_limits = 0,
      stat = "count",
      theme = light_mode_rt(base_size = 9, base_family = "arial"),
    ) +
    ggiraph::geom_bar_interactive(
      mapping = aes(tooltip = after_stat(count), data_id = color),
      width = 0.75,
      colour = blue,
      fill = blue,
      alpha = 0.9
    )

  vdiffr::expect_doppelganger(test_name, p)
})

