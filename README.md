
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/logo.png" align="right" height="139" />

# ggblanket

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/ggblanket)](https://CRAN.R-project.org/package=ggblanket)
<!-- badges: end -->

The objective is primarily to enable less thought for ggplot2, so you
make beautiful visualisation quickly and effortless.

A secondary objective was to build intuition for ggplot2, such that
users can move between the two packages without too much confusion.

As such, and with this balance in mind, the ggblanket package of wrapper
functions was created.

## Installation

You can install the development version of ggblanket from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("davidhodge931/ggblanket")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ggblanket)
library(palmerpenguins)
library(tidyverse)
#> -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
#> v ggplot2 3.3.5     v purrr   0.3.4
#> v tibble  3.1.6     v dplyr   1.0.8
#> v tidyr   1.2.0     v stringr 1.4.0
#> v readr   2.1.2     v forcats 0.5.1
#> -- Conflicts ------------------------------------------ tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
```

``` r
penguins |>
  ggplot() +
  geom_point(aes(x = bill_length_mm,  
                 y = flipper_length_mm, 
                 col = sex)) +
  facet_wrap(facets = ~species)
#> Warning: Removed 2 rows containing missing values (geom_point).
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" /> Key
changes from ggplot2:

-   aesthetics not required to be called within an aes function.
-   facet treated as an aesthetic
-   col and fill aesthetics merged together into a single col aesthetic
-   controlling input of all customised colours via pal argument
-   pushing x and y limits to the max of the x and y breaks by default
-   arranging the order of y values and col legends etc to be correct by
    default
-   providing quick arguments for scale adjustment and legend placement
-   changes to default colours, alphas, and widths used.

Other than that, each wrapper is faithful to ggplot2 to avoid too much
confusion.

``` r
penguins |>
  gg_point(x = bill_length_mm, 
           y = flipper_length_mm, 
           col = sex, 
           facet = species)
```

## Wrapper functions available

-   `gg_area`
-   `gg_bar`  
-   `gg_blank`  
-   `gg_boxplot`  
-   `gg_col`  
-   `gg_crossbar`
-   `gg_density`
-   `gg_errorbar`
-   `gg_freqpoly`
-   `gg_function`
-   `gg_histogram`  
-   `gg_jitter`  
-   `gg_label`
-   `gg_line`
-   `gg_linerange`
-   `gg_path`
-   `gg_point`  
-   `gg_pointrange`  
-   `gg_polygon`
-   `gg_qq`
-   `gg_raster`  
-   `gg_rect`
-   `gg_ribbon`
-   `gg_rug`
-   `gg_segment`  
-   `gg_sf`  
-   `gg_smooth`  
-   `gg_step`
-   `gg_text`
-   `gg_theme`
-   `gg_tile`
-   `gg_violin`

Default changes: - gg_sf defaults coord_sf and gg_theme(void = TRUE) -
gg_qq defaults oob_squish - width default = 0.75 for categorical vars -
size defaults 0.75 - alpha for fill defaults to 0.9 generally - alpha on
boxplot and smooth is 0.25

# library(palmerpenguins)

# 

# penguins \|\>

# gg_point(x = flipper_length_mm,

# y = body_mass_g)

# 

# penguins \|\>

# gg_point(x = flipper_length_mm,

# y = body_mass_g,

# col = sex)

# 

# penguins \|\>

# gg_point(x = flipper_length_mm,

# y = body_mass_g,

# col = species)

# 

# penguins \|\>

# gg_point(x = body_mass_g,

# y = species,

# col = sex)

# 

# penguins \|\>

# gg_point(x = body_mass_g,

# y = species,

# col = flipper_length_mm)

# 

# penguins \|\>

# gg_point(x = flipper_length_mm,

# y = body_mass_g,

# col = sex,

# facet = species)

# 

# penguins \|\>

# gg_point(x = body_mass_g,

# y = species,

# col = sex)
