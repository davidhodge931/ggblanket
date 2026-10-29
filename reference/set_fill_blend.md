# Set the ggblanket fill blend function

When `polygon = TRUE`, `fill_blend` is a function applied to the
`colour` and `colour_palette` to derive the fill. Only one of the colour
blend and fill blend can be active: setting a non-`NULL` fill blend
clears any colour blend (last call wins), and the colour blend then
defaults to identity.

## Usage

``` r
set_fill_blend(new = NULL)
```

## Arguments

- new:

  A function of one argument (a colour vector), e.g.
  `\(x) scales::alpha(x, 0.75)`. `NULL` (default) resets to the default
  of identity.

## Value

Invisibly, the previous value of the option.

## Examples

``` r
set_fill_blend(\(x) scales::alpha(x, 0.75))
```
