# Set ggblanket defaults

Use `set_blanket` to:

- Set a global theme via
  [`ggplot2::set_theme()`](https://ggplot2.tidyverse.org/reference/get_theme.html)

- Set a global option for how themes are to be refined based on plot
  scale types via `refine`

- Update the global theme `fill`, `colour`, `linewidth`, `shape`,
  `linetype`, `size`, `stroke`

- Update the global theme `fill_palette`, `colour_palette`,
  `shape_palette` and `linetype_palette`

- Set a global option for `colour_blend`, which is a function to
  transform the `colour` and `colour_palette` with input of the `fill`
  and `fill_palette` respectively

- Set a global option for `fill_blend`, which is a function to transform
  the `fill` and `fill_palette` with input of the `colour` and
  `colour_palette` respectively.

- Set a global option `coord_clip`.

## Usage

``` r
set_blanket(
  ...,
  theme = theme_lights(),
  refine = function(discrete, orientation) refine_axis_grid(discrete, orientation),
  colour_blend = NULL,
  fill_blend = NULL,
  borderwidth = 0.33,
  fill = "steelblue",
  fill_palette = list(jumble::jumble, viridis::turbo(n = 256)),
  colour = fill,
  colour_palette = fill_palette,
  linewidth = 0.66,
  shape = 21,
  shape_palette = scales::pal_manual(c(21, 24, 22, 23, 25)),
  linetype = 1,
  linetype_palette = scales::pal_manual(1:6),
  size = 1.5,
  stroke = 0.33,
  coord_clip = "on"
)
```

## Arguments

- ...:

  Not used. Forces named arguments.

- theme:

  A ggplot2 theme.

- refine:

  A function with arguments discrete and orientation to refine the theme
  based on these. Defaults to that globally set.

- colour_blend:

  When `polygon = TRUE`, a function applied to `fill` and `fill_palette`
  to derive the colour. If `fill_blend` is `NULL`, defaults to
  [`blends::multiply()`](https://davidhodge931.github.io/blends/reference/multiply.html)
  for light panels and
  [`blends::screen()`](https://davidhodge931.github.io/blends/reference/screen.html)
  for dark panels. Otherwise defaults to `\(x) x`.

- fill_blend:

  When `polygon = TRUE`, a function applied to `fill` and `fill_palette`
  to derive the fill. Defaults to `\(x) x`.

- borderwidth:

  When `polygon = TRUE`, the default linewidth. Defaults to `0.33`.

- fill:

  Default fill colour. Defaults to `"#357BA2FF"`.

- fill_palette:

  Palette for fill scales. A single discrete palette or
  `list(discrete, continuous)`. Defaults to
  `list(jumble::jumble, viridis::turbo(n = 256))`.

- colour:

  Default colour. Defaults to `fill`.

- colour_palette:

  Palette for colour scales. Same format as `fill_palette`. Defaults to
  `fill_palette`.

- linewidth:

  Default linewidth. Defaults to `0.66`.

- shape:

  Default point shape. Defaults to `21`.

- shape_palette:

  Palette for shape scales. Defaults to
  `scales::pal_manual(c(21, 24, 22, 23, 25))`.

- linetype:

  Default linetype. Defaults to `1`.

- linetype_palette:

  Palette for linetype scales. Defaults to `scales::pal_manual(1:6)`.

- size:

  Default point size. Defaults to `1.5`.

- stroke:

  Default stroke for point geoms. Defaults to `0.33`.

- coord_clip:

  Whether drawing is clipped to the panel. Either `"on"` or `"off"`.

## Value

Called for side effects.

## See also

[`scales::number_options()`](https://scales.r-lib.org/reference/number_options.html)

## Examples

``` r
set_blanket(
  fill_palette = scales::pal_hue(),
)
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygontype = linetype

palmerpenguins::penguins |>
  gg_density(
    x = flipper_length_mm,
    fill = species,
  )
#> Warning: Removed 2 rows containing non-finite outside the scale range
#> (`stat_density()`).
#> Warning: The `panel.polygon` theme element is not defined in the element hierarchy.
#> Warning: Removed 2 rows containing non-finite outside the scale range
#> (`stat_density()`).
#> Warning: The `panel.polygon` theme element is not defined in the element hierarchy.


set_blanket(
  fill_palette = scales::pal_hue(),
  fill_blend = \(x) scales::alpha(x, 0.75),
)
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygonwidth = 0.33
#> Warning: `...` must be empty.
#> ✖ Problematic argument:
#> • polygontype = linetype

palmerpenguins::penguins |>
  gg_density(
    x = flipper_length_mm,
    fill = species,
  )
#> Warning: Removed 2 rows containing non-finite outside the scale range
#> (`stat_density()`).
#> Warning: The `panel.polygon` theme element is not defined in the element hierarchy.
#> Warning: Removed 2 rows containing non-finite outside the scale range
#> (`stat_density()`).
#> Warning: The `panel.polygon` theme element is not defined in the element hierarchy.

```
