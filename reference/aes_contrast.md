# A colour aesthetic for contrast

A colour aesthetic to contrast with a fill aesthetic. Can be spliced
into [ggplot2::aes](https://ggplot2.tidyverse.org/reference/aes.html)
with
[rlang::!!!](https://rlang.r-lib.org/reference/splice-operator.html).

## Usage

``` r
aes_contrast(..., dark = "#121B24FF", light = "#FFFFFFFF")
```

## Arguments

- ...:

  Provided to require argument naming, support trailing commas etc.

- dark:

  A dark colour.

- light:

  A light colour.

## Value

A ggplot2 aesthetic

## Examples

``` r
library(ggplot2)
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
library(stringr)
library(palmerpenguins)
#> 
#> Attaching package: ‘palmerpenguins’
#> The following objects are masked from ‘package:datasets’:
#> 
#>     penguins, penguins_raw

set_blanket()
#> Warning: Duplicated aesthetics after name standardisation: fill
#> Warning: Duplicated aesthetics after name standardisation: fill

penguins |>
  count(species, sex) |>
  gg_col(
    x = sex,
    y = n,
    col = species,
    label = n,
    position = position_dodge(preserve = "single"),
    width = 0.75,
    x_labels = \(x) str_to_sentence(x),
  ) +
  geom_text(
    mapping = aes_contrast(),
    position = position_dodge(width = 0.75, preserve = "single"),
    vjust = 1.33,
    show.legend = FALSE,
  )
#> Scale for colour is already present.
#> Adding another scale for colour, which will replace the existing scale.


penguins |>
  count(species, sex) |>
  gg_col(
    x = sex,
    y = n,
    col = species,
    position = position_dodge(preserve = "single"),
    width = 0.75,
    x_labels = \(x) str_to_sentence(x),
    theme = dark_mode_r(),
  ) +
  geom_text(
    mapping = aes(label = n, !!!aes_contrast(dark = darkness[3], light = darkness[1])),
    position = position_dodge(width = 0.75, preserve = "single"),
    vjust = 1.33,
    show.legend = FALSE,
  )
#> Scale for colour is already present.
#> Adding another scale for colour, which will replace the existing scale.
```
