# Errorbar ggplot

Create a errorbar ggplot with a wrapper around
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html) +
[geom_errorbar()](https://ggplot2.tidyverse.org/reference/geom_linerange.html).

## Usage

``` r
gg_errorbar(
  data = NULL,
  ...,
  stat = "identity",
  position = "identity",
  coord = ggplot2::coord_cartesian(clip = "off"),
  theme = NULL,
  theme_orientation = NULL,
  theme_axis_line_rm = NULL,
  theme_axis_ticks_rm = NULL,
  theme_panel_grid_rm = NULL,
  blend = NULL,
  x = NULL,
  xmin = NULL,
  xmax = NULL,
  xend = NULL,
  y = NULL,
  ymin = NULL,
  ymax = NULL,
  yend = NULL,
  z = NULL,
  col = NULL,
  facet = NULL,
  facet2 = NULL,
  group = NULL,
  subgroup = NULL,
  label = NULL,
  text = NULL,
  sample = NULL,
  mapping = NULL,
  x_breaks = NULL,
  x_breaks_n = NULL,
  x_expand = NULL,
  x_limits_include = NULL,
  x_label = NULL,
  x_labels = NULL,
  x_position = "bottom",
  x_sec_axis = ggplot2::waiver(),
  x_symmetric = NULL,
  x_transform = NULL,
  y_breaks = NULL,
  y_breaks_n = NULL,
  y_expand = NULL,
  y_limits_include = NULL,
  y_label = NULL,
  y_labels = NULL,
  y_position = "left",
  y_sec_axis = ggplot2::waiver(),
  y_symmetric = NULL,
  y_transform = NULL,
  col_breaks = NULL,
  col_breaks_n = 5,
  col_drop = FALSE,
  col_limits_include = NULL,
  col_label = NULL,
  col_labels = NULL,
  col_legend_ncol = NULL,
  col_legend_nrow = NULL,
  col_legend_rev = FALSE,
  col_palette = NULL,
  col_palette_na = NULL,
  col_rescale = scales::rescale(),
  col_steps = FALSE,
  col_transform = NULL,
  facet_axes = NULL,
  facet_axis_labels = "margins",
  facet_drop = FALSE,
  facet_labels = NULL,
  facet_layout = NULL,
  facet_ncol = NULL,
  facet_nrow = NULL,
  facet_scales = "fixed",
  facet_space = "fixed",
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  label_case = NULL
)
```

## Arguments

- data:

  A data frame or tibble.

- ...:

  Other arguments passed to within a `params` list in
  [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html).

- stat:

  A statistical transformation to use on the data. A snakecase character
  string of a ggproto Stat subclass object minus the Stat prefix (e.g.
  `"identity"`).

- position:

  A position adjustment. A snakecase character string of a ggproto
  Position subclass object minus the Position prefix (e.g.
  `"identity"`), or a `position_*()` function that outputs a ggproto
  Position subclass object (e.g.
  [`ggplot2::position_identity()`](https://ggplot2.tidyverse.org/reference/position_identity.html)).

- coord:

  A coordinate system. A `coord_*()` function that outputs a constructed
  ggproto Coord subclass object (e.g.
  [`ggplot2::coord_cartesian()`](https://ggplot2.tidyverse.org/reference/coord_cartesian.html)).

- theme:

  A ggplot2 theme (e.g.
  [`light_mode_t()`](https://davidhodge931.github.io/ggblanket/reference/light_mode_r.md)
  or
  [`dark_mode_r()`](https://davidhodge931.github.io/ggblanket/reference/dark_mode_r.md)).
  (Or a list that includes 1. a theme and 2. a
  [`ggplot2::labs()`](https://ggplot2.tidyverse.org/reference/labs.html)
  function. E.g.
  `list(light_mode_r(), labs(colour = NULL, fill = NULL)`).

- theme_orientation:

  The orientation of plot, which affects the theme components that are
  removed. Either `"x"` or `"y"`.

- theme_axis_line_rm:

  `TRUE` or `FALSE` of whether to remove the relevant axis line per the
  `theme_orientation` of the plot.

- theme_axis_ticks_rm:

  `TRUE` or `FALSE` of whether to remove the relevant axis ticks per the
  `theme_orientation` of the plot.

- theme_panel_grid_rm:

  `TRUE` or `FALSE` of whether to remove the relevant panel grid per the
  `theme_orientation` of the plot.

- blend:

  The blending mode per
  [`ggblend::blend()`](https://mjskay.github.io/ggblend/reference/blend.html)
  (e.g. "multiply").

- x, xmin, xmax, xend, y, ymin, ymax, yend, z, col, facet, facet2,
  group, subgroup, label, text, sample:

  An unquoted aesthetic variable.

- mapping:

  A set of additional aesthetic mappings in
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html).
  Intended primarily for non-supported aesthetics (e.g. `shape`,
  `linetype`, `linewidth`, or `size`), but can also be used for delayed
  evaluation etc.

- x_breaks, y_breaks, col_breaks:

  A `scales::breaks_*` function (e.g. `scales::breaks_*()`), or a vector
  of breaks.

- x_breaks_n, y_breaks_n, col_breaks_n:

  A number of desired breaks for when `*_breaks = NULL`.

- x_expand, y_expand:

  Padding to the limits with the
  [`ggplot2::expansion()`](https://ggplot2.tidyverse.org/reference/expansion.html)
  function, or a vector of length 2 (e.g. `c(0, 0)`).

- x_limits_include, y_limits_include, col_limits_include:

  For a continuous variable, any values that the limits should encompass
  (e.g. `0`). For a discrete scale, manipulate the data instead with
  [`forcats::fct_expand`](https://forcats.tidyverse.org/reference/fct_expand.html).

- x_label, y_label, col_label:

  Label for the axis or legend title. Use `+ ggplot2::labs(... = NULL)`
  for no title.

- x_labels, y_labels, col_labels, facet_labels:

  A function that takes the breaks as inputs (e.g.
  `\(x) stringr::str_to_sentence(x)` or `scales::label_*()`), or a
  vector of labels. (Note this must be named for `facet_labels`).

- x_position, y_position:

  The position of the axis (i.e. `"left"`, `"right"`, `"bottom"` or
  `"top"`).If using `y_position = "top"` with a `*_theme_*` theme, add
  `caption = ""` or `caption = "\n"`.

- x_sec_axis, y_sec_axis:

  A secondary axis with
  [`ggplot2::dup_axis()`](https://ggplot2.tidyverse.org/reference/sec_axis.html)
  or
  [`ggplot2::sec_axis()`](https://ggplot2.tidyverse.org/reference/sec_axis.html).

- x_symmetric, y_symmetric:

  `TRUE` or `FALSE` of whether a symmetric scale.

- x_transform, y_transform, col_transform:

  For a continuous scale, a transformation object (e.g.
  [`scales::transform_log10()`](https://scales.r-lib.org/reference/transform_log.html))
  or character string of this minus the `transform_` prefix (e.g.
  `"log10"`).

- col_drop, facet_drop:

  For a discrete variable, FALSE or TRUE of whether to drop unused
  levels.

- col_legend_ncol, col_legend_nrow:

  The number of columns and rows in a legend guide.

- col_legend_rev:

  `TRUE` or `FALSE` of whether to reverse the elements of a legend
  guide. Defaults to `FALSE`.

- col_palette:

  A character vector of hex codes (or names) or a `scales::pal_*()`
  function.

- col_palette_na:

  A hex code (or name) for the colour of `NA` values.

- col_rescale:

  For a continuous variable, a
  [`scales::rescale()`](https://scales.r-lib.org/reference/rescale.html)
  function.

- col_steps:

  For a continuous variable, `TRUE` or `FALSE` of whether to colour in
  steps. Defaults to `FALSE`.

- facet_axes:

  Whether to add interior axes and ticks with `"margins"`, `"all"`,
  `"all_x"`, or `"all_y"`. Sometimes `+ *_theme_*()` may be needed.

- facet_axis_labels:

  Whether to add interior axis labels with `"margins"`, `"all"`,
  `"all_x"`, or `"all_y"`.

- facet_layout:

  Whether the layout is to be `"wrap"` or `"grid"`. If `NULL` and a
  single `facet` (or `facet2`) argument is provided, then defaults to
  `"wrap"`. If `NULL` and both facet and facet2 arguments are provided,
  defaults to `"grid"`.

- facet_ncol, facet_nrow:

  The number of columns and rows of facet panels. Only applies to a
  facet layout of `"wrap"`.

- facet_scales:

  Whether facet scales should be `"fixed"` across facets, `"free"` in
  both directions, or free in just one direction (i.e. `"free_x"` or
  `"free_y"`). Defaults to `"fixed"`.

- facet_space:

  When the facet scales are *not* `"fixed"`, whether facet space should
  be `"fixed"` across facets, `"free"` to be proportional in both
  directions, or free to be proportional in just one direction (i.e.
  `"free_x"` or `"free_y"`). Defaults to `"fixed"`.

- title:

  Title string.

- subtitle:

  Subtitle string.

- caption:

  Caption title string.

- label_case:

  A function to format the label of unlabelled variables. Defaults to
  [`snakecase::to_sentence_case`](https://rdrr.io/pkg/snakecase/man/caseconverter.html).

## Value

A ggplot object.

## Examples

``` r
library(ggplot2)
library(dplyr)
library(palmerpenguins)

set_blanket()
#> Warning: Duplicated aesthetics after name standardisation: fill
#> Warning: Duplicated aesthetics after name standardisation: fill

data.frame(
  trt = factor(c(1, 1, 2, 2)),
  resp = c(1, 5, 3, 4),
  group = factor(c(1, 2, 1, 2)),
  upper = c(1.1, 5.3, 3.3, 4.2),
  lower = c(0.8, 4.6, 2.4, 3.6)
) |>
  gg_errorbar(
    x = trt,
    ymin = lower,
    ymax = upper,
    col = group,
    width = 0.1,
    x_label = "Treatment",
    y_label = "Response",
  )
#> Scale for colour is already present.
#> Adding another scale for colour, which will replace the existing scale.

```
