# Guides for legend element colour

Guides to over-ride legend elements with a grey colour

- `guides_shape_grey()` for shape

- `guides_linewidth_grey()` for linewidth

- `guides_size_grey()` for size.

## Usage

``` r
guides_shape_grey(colour = grey, ...)

guides_linewidth_grey(colour = grey, ...)

guides_size_grey(colour = grey, ...)
```

## Arguments

- colour:

  A default hex code to override the colour of the legend elements.
  Note, the "fill" inherits from this argument. Defaults to grey.

- ...:

  Other arguments passed to
  [`ggplot2::guide_legend()`](https://ggplot2.tidyverse.org/reference/guide_legend.html).

## Value

A ggplot guides.

## Examples

``` r
library(dplyr)
library(tidyr)
library(ggplot2)
library(palmerpenguins)

set_blanket()
#> Warning: Duplicated aesthetics after name standardisation: fill
#> Warning: Duplicated aesthetics after name standardisation: fill

penguins |>
  drop_na() |>
  gg_jitter(
    x = species,
    y = flipper_length_mm,
    col = island,
    mapping = aes(shape = sex),
  ) +
  guides_shape_grey()
#> Scale for colour is already present.
#> Adding another scale for colour, which will replace the existing scale.

```
