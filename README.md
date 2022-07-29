
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggblanket <img src="man/figures/logo.png" align="right" width="120" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/ggblanket)](https://CRAN.R-project.org/package=ggblanket)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/ggblanket?color=lightgrey)](https://r-pkg.org/pkg/ggblanket)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/last-month/ggblanket?color=lightgrey)](https://r-pkg.org/pkg/ggblanket)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/last-week/ggblanket?color=lightgrey)](https://r-pkg.org/pkg/ggblanket)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/last-day/ggblanket?color=lightgrey)](https://r-pkg.org/pkg/ggblanket)
<!-- badges: end -->

## Overview

{ggblanket} is a package of {ggplot2} wrapper functions to **simplify
visualisation**.

To do this, the {ggblanket} package:

1.  uses `gg_*` functions that wrap a single `ggplot2::geom_*` function
2.  merges col and fill aesthetics into a single `col` argument
3.  customises colours via `pal` and `alpha` arguments consistently
4.  provides a `facet` argument to facet by a single variable
5.  provides an additional `facet2` argument to facet in a grid
6.  provides prefixed arguments to help find arguments
7.  uses nice numeric/date default scales.
8.  supports quick theme adjustments via `gg_theme` function
9.  arranges horizontal plot labels to be in correct order
10. converts unspecified titles to `snakecase::to_sentence`
11. provides a `gg_blank` function for extra flexibility
12. supports nice `plotly::ggplotly` tooltips
13. provides access to all other `geom_*` arguments via `...`
14. is useful for creating customised {ggblanket} functions with your
    own defaults

If you would like to show your support for {ggblanket}, you can
<a href="https://www.buymeacoffee.com/davidhodge931" target="_blank">buy
me a coffee</a> or give the repository a star.

## Website

Click [here](https://davidhodge931.github.io/ggblanket/) for the
{ggblanket} website.

## Installation

Install either from CRAN with:

``` r
install.packages("ggblanket")
```

Or install the development version with:

``` r
# install.packages("devtools")
devtools::install_github("davidhodge931/ggblanket")
```

## Examples

``` r
library(dplyr)
library(ggplot2)
library(ggblanket)
library(palmerpenguins)
library(snakecase)
library(stringr)
```

1.  {ggblanket} uses `gg_*` functions that wrap a single
    `ggplot2::geom_*` function.

``` r
iris %>%
  gg_point(x = Sepal.Width, y = Sepal.Length, col = Species)
```

![](man/figures/README-unnamed-chunk-3-1.png)<!-- -->

2.  {ggblanket} merges col and fill aesthetics into a single `col`
    argument.

``` r
penguins %>% 
  gg_histogram(x = body_mass_g, col = species) 
```

![](man/figures/README-unnamed-chunk-4-1.png)<!-- -->

3.  {ggblanket} customises colours via `pal` and `alpha` arguments
    consistently.

These arguments are the same regardless of whether a `col` variable is
specified. If more colours are provided than needed by the pal argument,
then the excess colours will just be dropped. Note all colours specified
by the pal argument will inherit to any further `ggplot2::geom_*` layers
added.

``` r
penguins %>% 
  mutate(sex = str_to_sentence(sex)) %>% 
  group_by(species, sex) %>% 
  summarise(body_mass_g = mean(body_mass_g, na.rm = TRUE)) %>% 
  gg_col(species, body_mass_g, sex, 
         position = position_dodge2(preserve = "single"), 
         pal = c("#1B9E77", "brown"))
```

![](man/figures/README-unnamed-chunk-5-1.png)<!-- -->

4.  {ggblanket} provides a `facet` argument to facet by a single
    variable.

``` r
penguins %>% 
  tidyr::drop_na(sex) %>%
  mutate(sex = str_to_sentence(sex)) %>% 
  gg_violin(x = sex, y = body_mass_g, facet = species, 
            y_include = 0, 
            y_breaks = scales::breaks_width(1000),
            pal = pals::brewer.dark2(9))
```

![](man/figures/README-unnamed-chunk-6-1.png)<!-- -->

5.  {ggblanket} provides an additional `facet2` argument to facet in a
    grid.

``` r
penguins %>% 
  tidyr::drop_na(sex) %>% 
  mutate(sex = str_to_sentence(sex)) %>% 
  gg_point(x = bill_length_mm, 
           y = body_mass_g,
           col = sex,
           facet = species,
           facet2 = sex, 
           y_breaks = scales::breaks_width(1500), 
           size = 1)
```

![](man/figures/README-unnamed-chunk-7-1.png)<!-- -->

6.  {ggblanket} provides prefixed arguments to help find arguments.

This is designed to work with the Rstudio autocomplete to help you find
the adjustment you need. Press the tab key after typing `x_`,`y_`,
`col_` or `facet_` to access this. Then use arrow keys, and press tab
again to select.

Available arguments are for `x`, `y`, `col` and `facet`:
``` *_breaks``*_limits ```, `*_include`, `*_expand`, and `*_labels`.

For `x` and `y`, there is also a `*_trans` argument.

There is also a `col_intervals` argument, so that legends can be
arranged in appropriate order by default.

``` r
penguins %>%
  gg_jitter(
    x = species,
    y = body_mass_g,
    col = flipper_length_mm,
    position = ggplot2::position_jitter(width = 0.2, height = 0, seed = 123), 
    col_intervals = ~ santoku::chop_quantiles(.x, probs = seq(0, 1, 0.25)),
    col_legend_place = "r",
    y_include = 0,
    y_breaks = scales::breaks_width(1500), 
    y_labels = scales::label_number()
  )
```

![](man/figures/README-unnamed-chunk-8-1.png)<!-- -->

7.  {ggblanket} uses nice numeric/date default scales.

Where x variable is categorical and y numeric, the numeric y scale
defaults to the limits being the min and max of the *breaks*, with
expand of c(0, 0). Equivalent happens for the horizontal vice versa
situation.

Where both x and y are numeric/date, the y scale defaults to the limits
being the min and max of the *breaks* with expand of c(0, 0) - and x
scales default to the min and max of the *variable* with expand of
`c(0.025, 0.025)`.

``` r
storms %>%
  group_by(year) %>%
  filter(between(year, 1980, 2020)) %>%
  summarise(wind = mean(wind, na.rm = TRUE)) %>%
  gg_col(
    x = year,
    y = wind,
    x_labels = ~.x,
    title = "Storm wind speed",
    subtitle = "USA average storm wind speed, 1980\u20132020",
    y_title = "Wind speed (knots)",
    caption = "Source: NOAA"
  ) 
```

![](man/figures/README-unnamed-chunk-9-1.png)<!-- -->

8.  {ggblanket} supports quick theme adjustments via `gg_theme`
    function.

``` r
storms %>%
  group_by(year) %>%
  filter(between(year, 1980, 2020)) %>%
  summarise(wind = mean(wind, na.rm = TRUE)) %>%
  gg_line(
    x = year,
    y = wind,
    x_labels = ~.x,
    y_include = 0, 
    theme = gg_theme(
      bg_plot_pal = "white",
      bg_panel_pal = "#F1F3F5",
      title_size = 13,
      body_size = 12,
      grid_v = TRUE, 
      grid_h = FALSE)) +
  geom_point()
```

![](man/figures/README-unnamed-chunk-10-1.png)<!-- -->

9.  {ggblanket} arranges horizontal plot labels to be in correct order.

``` r
penguins %>%
  tidyr::drop_na(sex) %>% 
  group_by(species, sex, island) %>%
  summarise(body_mass_kg = mean(body_mass_g) / 1000) %>%
  gg_col(x = body_mass_kg, y = species, col = sex, facet = island,
         col_labels = to_sentence_case, 
         position = "dodge")
```

![](man/figures/README-unnamed-chunk-11-1.png)<!-- -->

10. {ggblanket} converts unspecified titles to snakecase::to_sentence.

``` r
penguins %>%
  group_by(species, sex) %>%
  summarise(across(body_mass_g, ~ round(mean(.x, na.rm = TRUE)), 0)) %>% 
  gg_tile(sex, species, col = body_mass_g, 
          x_labels = to_sentence_case,
          pal = pals::brewer.blues(9),
          width = 0.9, 
          height = 0.9, 
          col_legend_place = "r",
          title = "Average penguin body mass",
          subtitle = "Palmer Archipelago, Antarctica",
          theme = gg_theme(grid_h = FALSE, 
                           bg_plot_pal = "white",
                           axis_pal = "white", 
                           ticks_pal = "white")) +
  geom_text(aes(label = body_mass_g), col = "#232323", size = 3.5) 
```

![](man/figures/README-unnamed-chunk-12-1.png)<!-- -->

11. {ggblanket} provides a `gg_blank` function for extra flexibility.

``` r
penguins %>%
  tidyr::drop_na(sex) %>%
  mutate(sex = str_to_sentence(sex)) %>%
  group_by(species, sex) %>%
  summarise(
    mean = round(mean(bill_length_mm, na.rm = TRUE), 0),
    n = n(),
    se = mean / sqrt(n),
    upper = mean + 1.96 * se,
    lower = mean - 1.96 * se
  ) %>%
  gg_blank(
    x = sex,
    y = mean,
    col = sex,
    facet = species,
    label = mean,
    ymin = lower,
    ymax = upper,
    y_include = 0,
    y_title = "Bill length mm"
  ) +
  geom_col(width = 0.75, alpha = 0.9) +
  geom_errorbar(width = 0.1, colour = pal_na()) 
```

![](man/figures/README-unnamed-chunk-13-1.png)<!-- -->

12. {ggblanket} supports nice `plotly::ggplotly` tooltips.

The `add_tooltip` function allows users to create nice tooltips in
combination with the `text` argument, and the `tooltip = "text"`
argument in `ggplotly`.

``` r
iris %>% 
  mutate(Species = str_to_sentence(Species)) %>% 
  add_tooltip_text(titles = to_sentence_case) %>% 
  gg_point(x = Sepal.Width, 
           y = Sepal.Length, 
           col = Species, 
           text = text, 
           theme = gg_theme("helvetica", 
                            bg_plot_pal = "white")) %>% 
  plotly::ggplotly(tooltip = "text")
```

![](man/figures/ggplotly_screenshot.png)

13. {ggblanket} provides access to all other `geom_*` arguments via
    `...`

``` r
penguins %>%
  tidyr::drop_na(sex) %>%
  gg_smooth(
    x = flipper_length_mm,
    y = body_mass_g,
    col = sex,
    level = 0.99, 
    col_legend_place = "t",
    col_title = "", 
    col_labels = to_sentence_case
  ) 
```

![](man/figures/README-unnamed-chunk-15-1.png)<!-- -->

14. {ggblanket} is useful for creating customised {ggblanket} functions
    with your own defaults.

This is because the `...` argument can allow you to access *all*
arguments within the {ggblanket} `gg_` function.

``` r
gg_point_custom <- function(data, x, y, col, 
                            size = 3, 
                            pal = pals::brewer.dark2(9), 
                            col_title = "", 
                            col_legend_place = "t",
                            ...) {
  data %>% 
    gg_point(x = {{ x }}, y = {{ y }}, col = {{col}}, 
             size = size, 
             pal = pal, 
             col_title = col_title, 
             col_legend_place = col_legend_place, 
             ...)
}

iris %>%
  mutate(Species = str_to_sentence(Species)) %>% 
  gg_point_custom(
    x = Sepal.Width,
    y = Sepal.Length,
    col = Species, 
    x_breaks = scales::breaks_width(1)
  )
```

![](man/figures/README-unnamed-chunk-16-1.png)<!-- -->
