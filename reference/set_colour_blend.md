# Set the ggblanket colour blend function

When `polygon = TRUE`, `colour_blend` is a function applied to the
`fill` and `fill_palette` to derive the colour. Only one of the colour
blend and fill blend can be active: setting a non-`NULL` colour blend
clears any fill blend (last call wins), and the fill blend then defaults
to identity.

## Usage

``` r
set_colour_blend(new = NULL)
```

## Arguments

- new:

  A function of one argument (a colour vector), e.g.
  [`blends::multiply()`](https://davidhodge931.github.io/blends/reference/multiply.html).
  `NULL` (default) resets to the default: if no fill blend is set,
  [`blends::multiply()`](https://davidhodge931.github.io/blends/reference/multiply.html)
  for light panels and
  [`blends::screen()`](https://davidhodge931.github.io/blends/reference/screen.html)
  for dark panels; otherwise identity.

## Value

Invisibly, the previous value of the option.

## Examples

``` r
set_colour_blend(\(x) blends::multiply(x))
```
