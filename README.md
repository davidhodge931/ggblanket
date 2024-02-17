
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
[![R-CMD-check](https://github.com/davidhodge931/ggblanket/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/davidhodge931/ggblanket/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/davidhodge931/ggblanket/branch/main/graph/badge.svg)](https://app.codecov.io/gh/davidhodge931/ggblanket?branch=main)
<!-- badges: end -->

## Overview

ggblanket is a package of ggplot2 wrapper functions.

The primary objective is to **simplify ggplot2 visualisation**.

Secondary objectives relate to:

- Design: produce well-designed visualisation
- Alignment: use conventions aligned with ggplot2
- Scope: cover much of what ggplot2 does.

## Installation

``` r
install.packages("ggblanket")
```

## Example

``` r
library(ggblanket)
library(palmerpenguins)
library(tidyverse)

penguins |>
  gg_histogram(
    x = flipper_length_mm,
    col = species,
  )
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="75%" />

## Get started

Click
[here](https://davidhodge931.github.io/ggblanket/articles/ggblanket.html)
to start learning how ggblanket works.

## Thank you

Thanks to all authors of ggplot2, tidyverse, and the wider R ecosystem.

This R package is dedicated to my Dad (Peter Hodge, 1953-2023).
