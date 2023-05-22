
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

## Purpose

ggblanket is a package of wrapper functions around the fantastic ggplot2
package.

The primary objective is to **simplify ggplot2 visualisation**.

Secondary objectives relate to:

- Scope: cover the most useful 80% of what ggplot2 does
- Design: produce well-designed visualisation by default
- Alignment: use conventions generally aligned with ggplot2.

It is intended to be useful for all levels of experience from beginner
to expert.

## Installation

``` r
install.packages("ggblanket")
```

## Examples

``` r
library(ggblanket)
library(palmerpenguins)

penguins |>
  gg_point(
    x = flipper_length_mm,
    y = body_mass_g, 
    col = species)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="75%" /> <br>
<br>

``` r
penguins |>
  tidyr::drop_na(sex) |> 
  gg_histogram(
    x = flipper_length_mm,
    col = sex,
    facet = species,
    pal = c("#2596be", "#fc7c24"),
    col_labels = stringr::str_to_sentence)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="75%" />

## Get started

Click
[here](https://davidhodge931.github.io/ggblanket/articles/ggblanket.html)
to get started learning how ggblanket works.

## Thanks!

Thank you to all authors and contributors to ggplot2, tidyverse, and the
wider R ecosystem. If you like ggblanket, please give the repository a
star and help spread the word.
