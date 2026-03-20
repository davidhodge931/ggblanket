# Mode colour and linewidth defaults

`lightness` and `darkness` are vectors of 3 colours used in the
`*_theme_*` themes for the for the text, axis.line (and axis.ticks),
panel.grid, panel.background and plot.background etc.

`linewidthness` is a vector of 2 integers used in the `*_theme_*` themes
for the linewidth of the axis.line (axis.ticks and legend.ticks) and
panel.grid theme elements.

## Usage

``` r
lightness

darkness
```

## Value

A character vector.

## Examples

``` r
scales::show_col(c(lightness, darkness), ncol = 3)
```
