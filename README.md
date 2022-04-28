
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/logo.png" align="right" height="139" />

# ggblanket

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/ggblanket)](https://CRAN.R-project.org/package=ggblanket)
<!-- badges: end -->

## Overview

{ggblanket} is a package of wrapper functions around the amazing
{ggplot2} package.

Hence the blanket metaphor in the package name.

The primary objective of the package is to assist users in making
beautiful {ggplot2} visualisation faster with less cognitive load.

A secondary objective is to assist users in building intuition for
{ggplot2}, such that users can move between the two packages as needed.

With these objectives in mind, the {ggblanket} package:

-   uses quick functions that wrap around a single geom
-   merges col and fill aesthetics together into a single col aesthetic
-   customises colours via pal argument
-   treats faceting as an aesthetic
-   pushes x and y limits to the max of the x and y breaks by default
-   arranges horizontal geom y and col labels etc to be correct
-   makes titles and labels convert to sentence case and comma format by
    default
-   provides quick arguments for scale adjustment and legend placement
-   changes default colours, alphas, widths and themes
-   allows users to access all other non-aesthetic geom functionality
-   keeps all stat and position arguments consistent with the applicable
    geom’s

## Installation

You can install the development version of ggblanket from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("davidhodge931/ggblanket")
```

## Examples
