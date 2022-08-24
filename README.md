
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

ggblanket is a package of wrapper functions around the incredible
ggplot2 package.

The primary objective is to **simplify visualisation**.

Secondary objectives are:

-   Scope: cover \>90% of what ggplot2 does
-   Design: produce well-designed visualisation by default
-   Alignment: use conventions generally aligned with ggplot2.

It is intended to be useful for all levels of experience from beginners
to experts.

## Website

Click
[here](https://davidhodge931.github.io/ggblanket/articles/ggblanket.html)
to get started learning how ggblanket works.

## Installation

``` r
install.packages("ggblanket")
```

## Examples

``` r
library(dplyr)
library(ggblanket)
library(palmerpenguins)

palmerpenguins::penguins %>%
  tidyr::drop_na() %>%
  mutate(sex = stringr::str_to_sentence(sex)) %>% 
  gg_histogram(x = flipper_length_mm,
               col = sex,
               facet = species,
               pal = c("#1B9E77", "#9E361B"))
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="75%" />

## Thanks

Thank you Hadley Wickham and all other authors of the ggplot2 package.

If you like ggblanket, please give the repository a star, tweet or blog
about it, or tell a friend or colleague.
