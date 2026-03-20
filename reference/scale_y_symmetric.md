# Symmetric y continuous scale

Create a symmetric continuous y scale for ggplot2 plots. The scale
ensures that limits set to the range of breaks with zero expand (where
`symmetric = TRUE`). Note this scale should only be used in plots with
geoms with `stat = "identity"`. Symmetric y continuous scale

## Usage

``` r
scale_y_symmetric(
  data = NULL,
  y = NULL,
  ...,
  breaks = NULL,
  breaks_n = 6,
  expand = NULL,
  expand_limits = NULL,
  labels = NULL,
  position = "left",
  sec_axis = ggplot2::waiver(),
  transform = "identity",
  symmetric = TRUE
)
```

## Arguments

- data:

  A data frame or tibble.

- y:

  An unquoted variable.

- ...:

  Provided to force user argument naming etc.

- breaks:

  A `scales::breaks_*` function (e.g. `scales::breaks_*()`), or a vector
  of breaks.

- breaks_n:

  If `breaks = NULL`, the desired number of breaks.

- expand:

  Padding to the limits with the
  [`ggplot2::expansion()`](https://ggplot2.tidyverse.org/reference/expansion.html)
  function, or a vector of length 2 (e.g. `c(0, 0)`).

- expand_limits:

  Any values that the limits should encompass (e.g. `0`).

- labels:

  A function that takes the breaks as inputs (e.g.
  `\(x) stringr::str_to_sentence(x)` or `scales::label_*()`), or a
  vector of labels.

- position:

  The position of the axis (i.e. `"left"`, `"right"`, `"bottom"` or
  `"top"`).

- sec_axis:

  A secondary axis created with
  [`ggplot2::sec_axis()`](https://ggplot2.tidyverse.org/reference/sec_axis.html)
  or
  [`ggplot2::dup_axis()`](https://ggplot2.tidyverse.org/reference/sec_axis.html).

- transform:

  A transformation object (e.g.
  [`scales::transform_log10()`](https://scales.r-lib.org/reference/transform_log.html))
  or character string of this minus the `transform_` prefix (e.g.
  `"log10"`).

- symmetric:

  `TRUE` or `FALSE` of whether a symmetric scale.

## Value

A ggplot2 continuous y scale.

## Examples

``` r
library(ggplot2)
library(dplyr)
library(palmerpenguins)

set_blanket()
#> Warning: Duplicated aesthetics after name standardisation: fill
#> Warning: Duplicated aesthetics after name standardisation: fill

penguins |>
  ggplot() +
  geom_point(aes(x = flipper_length_mm, y = body_mass_g, colour = species)) +
  scale_y_symmetric(penguins, body_mass_g) +
  theme(axis.line.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  coord_cartesian(clip = "off") +
  labs(x = "Flipper length mm", y = "Body mass g", colour = "Species")
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_point()`).

```
