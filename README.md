
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggblanket

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/ggblanket)](https://CRAN.R-project.org/package=ggblanket)
<!-- badges: end -->

The goal of ggblanket is to assist used

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
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.

Wrapper functions:

-   `gg_area`
-   gg_bar  
-   gg_blank  
-   gg_boxplot  
-   gg_col  
-   gg_crossbar
-   gg_density
-   gg_errorbar  
-   gg_freqpoly  
-   gg_function  
-   gg_histogram  
-   gg_jitter
-   gg_label
-   gg_line
-   gg_linerange
-   gg_path  
-   gg_point  
-   gg_pointrange
-   gg_polygon
-   gg_qq
-   gg_raster
-   gg_rect  
-   gg_ribbon
-   gg_rug
-   gg_segment  
-   gg_sf
-   gg_smooth
-   gg_step  
-   gg_text  
-   gg_theme
-   gg_tile  
-   gg_violin
