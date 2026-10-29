# Set the ggblanket refine function

`set_refine()` sets a global function that refines the theme depending
on the plot's scale types. All other defaults (fill, colour, linewidth,
palettes, etc.) are controlled through the theme, e.g. with
[`ggplot2::set_theme()`](https://ggplot2.tidyverse.org/reference/get_theme.html),
[`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html)
and
[`ggplot2::element_geom()`](https://ggplot2.tidyverse.org/reference/element.html).

## Usage

``` r
set_refine(new = NULL)
```

## Arguments

- new:

  A function with arguments `discrete` and `orientation` that returns a
  theme refinement. `NULL` (default) resets to
  `\(discrete, orientation) refine_axis_grid(discrete, orientation)`.

## Value

Invisibly, the previous value of the option.

## Examples

``` r
set_refine(\(discrete, orientation) refine_axis_grid(discrete, orientation))
```
