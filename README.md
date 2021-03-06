
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

{ggblanket} is a package of wrapper functions around the amazing
{ggplot2} package.

It aims to **simplify pretty {ggplot2} visualisation**.

To do this, the {ggblanket} package:

1.  uses `gg_*` functions that each wrap a `ggplot2::ggplot` call with a
    single `ggplot2::geom_*` function
2.  merges col and fill aesthetics into a single col aesthetic
3.  provides colour customisation via pal and alpha arguments
4.  treats faceting as an aesthetic
5.  provides good-looking default x and y scales
6.  provides prefixed arguments for easy customisation with Rstudio
    autocomplete
7.  arranges horizontal geom y and col labels etc to be in correct order
8.  converts unspecified titles to snakecase::to_sentence by default
9.  outputs a `ggplot2` object, so extra `ggplot2` layers can be added
    if necessary
10. provides access to all of the relevant geom arg’s through the `...`
    argument
11. provides a `gg_blank` function for extra flexibility
12. is useful for creating custom {ggblanket} functions with your own
    defaults
13. supports ggplotly use

If you would like to show your support for {ggblanket}, you can
<a href="https://www.buymeacoffee.com/davidhodge931" target="_blank">buy
me a coffee</a> or give the repository a star.

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

## Website

Click [here](https://davidhodge931.github.io/ggblanket/) for the
{ggblanket} website.

## Examples

``` r
library(dplyr)
library(ggplot2)
library(ggblanket)
library(palmerpenguins)
```

1.  {ggblanket} uses `gg_*` functions that each wrap a `ggplot2::ggplot`
    call with a single `ggplot2::geom_*` function.

``` r
iris %>%
  gg_point(x = Sepal.Width, y = Sepal.Length, col = Species)
```

![](man/figures/README-unnamed-chunk-3-1.png)<!-- -->

2.  {ggblanket} merges col and fill aesthetics into a single col
    aesthetic.

``` r
penguins %>% 
  gg_histogram(x = body_mass_g, col = species) 
```

![](man/figures/README-unnamed-chunk-4-1.png)<!-- -->

3.  {ggblanket} provides colour customisation via pal and alpha
    arguments.

These arguments are the same regardless of whether a col variable is
specified. If more colours are provided than needed by the pal argument,
then the excess colours will just be dropped. Note all colours specified
by the pal argument will inherit to any further `ggplot2::geom_*` layers
added.

``` r
penguins %>% 
  gg_density(x = body_mass_g, col = species, 
             pal = pals::brewer.dark2(9))
```

![](man/figures/README-unnamed-chunk-5-1.png)<!-- -->

4.  {ggblanket} treats faceting as an aesthetic.

``` r
penguins %>% 
  tidyr::drop_na() %>%
  mutate(sex = stringr::str_to_sentence(sex)) %>% 
  gg_violin(x = sex, y = body_mass_g, facet = species, 
            y_include = 0, 
            y_breaks = scales::breaks_width(1000),
            pal = pals::brewer.dark2(9))
```

![](man/figures/README-unnamed-chunk-6-1.png)<!-- -->

5.  {ggblanket} provides good-looking default x and y scales.

For where:

-   x categorical and y numeric/date: y_limits default to min/max of
    y_breaks with y_expand of c(0, 0)
-   y categorical and x numeric/date: x_limits default to min/max of
    x_breaks with x_expand of c(0, 0)
-   x numeric/date and y numeric/date: y_limits default to min/max of
    y_breaks with y_expand of c(0, 0), and x_limits default to NULL
    (i.e. min/max of x variable) and x_expand of c(0.025, 0.025)

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

![](man/figures/README-unnamed-chunk-7-1.png)<!-- -->

6.  {ggblanket} provides prefixed arguments for easy customisation with
    Rstudio autocomplete.

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

7.  {ggblanket} arranges horizontal geom y and col labels etc to be in
    correct order.

``` r
penguins %>%
  tidyr::drop_na() %>% 
  group_by(species, sex, island) %>%
  summarise(body_mass_kg = mean(body_mass_g) / 1000) %>%
  gg_col(x = body_mass_kg, y = species, col = sex, facet = island,
         col_labels = stringr::str_to_sentence, 
         position = "dodge")
```

![](man/figures/README-unnamed-chunk-9-1.png)<!-- -->

8.  {ggblanket} converts unspecified titles to snakecase::to_sentence by
    default.

``` r
penguins %>%
  group_by(species, sex) %>%
  summarise(across(body_mass_g, ~ round(mean(.x, na.rm = TRUE)), 0)) %>% 
  gg_tile(sex, species, col = body_mass_g, 
          x_labels = snakecase::to_sentence_case,
          pal = pals::brewer.blues(9),
          width = 0.9, 
          height = 0.9, 
          col_legend_place = "r",
          title = "Average penguin body mass",
          subtitle = "Palmer Archipelago, Antarctica",
          theme = gg_theme(pal_axis = "#ffffff", pal_ticks = "#ffffff")) +
  geom_text(aes(label = body_mass_g), col = "#232323", size = 3.5) 
```

![](man/figures/README-unnamed-chunk-10-1.png)<!-- -->

9.  {ggblanket} outputs a `ggplot2` object, so extra `ggplot2` layers
    can be added if necessary.

Aesthetics and the pal are inherited to any subsequent geoms.

``` r
storms %>%
  group_by(year) %>%
  filter(between(year, 1980, 2020)) %>%
  summarise(wind = mean(wind, na.rm = TRUE)) %>%
  gg_line(
    x = year,
    y = wind,
    x_labels = ~.x,
    y_include = 0
  ) +
  geom_point()
```

![](man/figures/README-unnamed-chunk-11-1.png)<!-- -->

10. {ggblanket} provides access to all of the relevant geom arg’s
    through the `...` argument.

``` r
penguins %>%
  tidyr::drop_na() %>%
  gg_smooth(
    x = flipper_length_mm,
    y = body_mass_g,
    col = sex,
    level = 0.99,
    size = 0.5, 
    col_legend_place = "t",
    col_title = "", 
    col_labels = stringr::str_to_sentence
  ) 
```

![](man/figures/README-unnamed-chunk-12-1.png)<!-- -->

11. {ggblanket} provides a `gg_blank` function for extra flexibility.

``` r
penguins %>%
  tidyr::drop_na() %>%
  mutate(sex = stringr::str_to_sentence(sex)) %>%
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

12. {ggblanket} is useful for creating custom {ggblanket} functions with
    your own defaults.

The `...` argument will allow you to access all other arguments within
the {ggblanket} function.

``` r
gg_point_custom <- function(data, x, y, col, 
                            size = 3, 
                            pal = pals::brewer.dark2(9), 
                            col_title = "", 
                            col_legend_place = "t", 
                            theme = gg_theme(pal_body = "white", 
                                             pal_title = "white", 
                                             pal_subtitle = "white", 
                                             pal_background = c("#232323", "black"), 
                                             pal_grid = "black",
                                             y_grid = TRUE),
                            ...) {
  data %>% 
    gg_point(x = {{ x }}, y = {{ y }}, col = {{col}}, 
             size = size, 
             pal = pal, 
             col_title = col_title, 
             col_legend_place = col_legend_place, 
             theme = theme,
             ...)
}

iris %>%
  mutate(Species = stringr::str_to_sentence(Species)) %>% 
  gg_point_custom(
    x = Sepal.Width,
    y = Sepal.Length,
    col = Species, 
    x_breaks = scales::breaks_width(1)
  )
```

![](man/figures/README-unnamed-chunk-14-1.png)<!-- -->

13. {ggblanket} supports ggplotly use.

`ggplotly` won’t work in all situations. But it does work a lot of the
time.

{ggblanket} provides an `add_tooltip` function to assist with creating
nice tooltips in combination with the `text` argument, and the
`tooltip = "text"` argument in `ggplotly`.

``` r
iris %>% 
  mutate(Species = stringr::str_to_sentence(Species)) %>% 
  add_tooltip_text(titles = snakecase::to_sentence_case) %>% 
  gg_point(x = Sepal.Width, 
           y = Sepal.Length, 
           col = Species, 
           text = text, 
           theme = gg_theme("helvetica", y_grid = TRUE)) %>% 
  plotly::ggplotly(tooltip = "text")
```

![](man/figures/ggplotly_screenshot.png)
