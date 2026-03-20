# ggblanket

## Overview

ggblanket is a package of ggplot2 wrapper functions.

The primary objective is to **simplify ggplot2 visualisation**.

Secondary objectives relate to:

- Design: produce well-designed visualisation
- Alignment: align with ggplot2 and tidyverse
- Scope: cover much of what ggplot2 does.

Computational speed has been traded-off.

## Installation

``` r
install.packages("ggblanket")
```

## Example

``` r
library(ggblanket)
library(palmerpenguins)

set_blanket()

penguins |>
  gg_histogram(
    x = flipper_length_mm,
    col = species,
  )
```

![](reference/figures/README-unnamed-chunk-2-1.png)

## Get started

Click
[here](https://davidhodge931.github.io/ggblanket/articles/ggblanket.html)
to start learning how ggblanket works.

## Thank you

Thanks to the developers of ggplot2, tidyverse and the R ecosystem.

For Dad, always missed (Peter Hodge, 1953-2023).
