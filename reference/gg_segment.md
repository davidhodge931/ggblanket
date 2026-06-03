# Segment ggplot

A segment ggplot with
[geom_segment()](https://ggplot2.tidyverse.org/reference/geom_segment.html)
defaults for the geom, stat and position.

## Usage

``` r
gg_segment(
  data,
  ...,
  geom = "segment",
  stat = "identity",
  position = ggplot2::position_identity(),
  before = NULL,
  with = NULL,
  focus = NULL,
  refine = NULL,
  border = FALSE,
  x = NULL,
  xmin = NULL,
  xmax = NULL,
  xend = NULL,
  xintercept = NULL,
  y = NULL,
  ymin = NULL,
  ymax = NULL,
  yend = NULL,
  yintercept = NULL,
  z = NULL,
  fill = NULL,
  colour = NULL,
  alpha = NULL,
  shape = NULL,
  linetype = NULL,
  linewidth = NULL,
  size = NULL,
  stroke = NULL,
  label = NULL,
  weight = NULL,
  group = NULL,
  width = NULL,
  height = NULL,
  slope = NULL,
  intercept = NULL,
  sample = NULL,
  angle = NULL,
  radius = NULL,
  mapping = ggplot2::aes(),
  x_type = NULL,
  x_subtype = NULL,
  x_breaks = NULL,
  x_drop = TRUE,
  x_expand = NULL,
  x_guide = ggplot2::waiver(),
  x_labels = NULL,
  x_limits = NULL,
  x_minor_breaks = ggplot2::waiver(),
  x_name = ggplot2::waiver(),
  x_oob = scales::oob_censor,
  x_palette = seq_len,
  x_position = "bottom",
  x_sec_axis = ggplot2::waiver(),
  x_transform = NULL,
  y_type = NULL,
  y_subtype = NULL,
  y_breaks = NULL,
  y_drop = TRUE,
  y_expand = NULL,
  y_guide = ggplot2::waiver(),
  y_labels = NULL,
  y_limits = NULL,
  y_minor_breaks = ggplot2::waiver(),
  y_name = ggplot2::waiver(),
  y_oob = scales::oob_censor,
  y_palette = seq_len,
  y_position = "left",
  y_sec_axis = ggplot2::waiver(),
  y_transform = NULL,
  fill_type = NULL,
  fill_subtype = NULL,
  fill_breaks = ggplot2::waiver(),
  fill_drop = TRUE,
  fill_guide = NULL,
  fill_labels = NULL,
  fill_limits = NULL,
  fill_name = ggplot2::waiver(),
  fill_oob = scales::oob_censor,
  fill_rescaler = scales::rescale,
  fill_palette = NULL,
  fill_transform = NULL,
  colour_type = NULL,
  colour_subtype = NULL,
  colour_breaks = NULL,
  colour_drop = NULL,
  colour_guide = NULL,
  colour_labels = NULL,
  colour_limits = NULL,
  colour_name = NULL,
  colour_oob = NULL,
  colour_rescaler = NULL,
  colour_palette = NULL,
  colour_transform = NULL,
  alpha_type = NULL,
  alpha_subtype = NULL,
  alpha_breaks = ggplot2::waiver(),
  alpha_drop = TRUE,
  alpha_guide = NULL,
  alpha_labels = NULL,
  alpha_limits = NULL,
  alpha_name = ggplot2::waiver(),
  alpha_oob = scales::oob_censor,
  alpha_palette = NULL,
  alpha_transform = NULL,
  size_type = NULL,
  size_subtype = NULL,
  size_breaks = ggplot2::waiver(),
  size_drop = TRUE,
  size_guide = NULL,
  size_labels = NULL,
  size_limits = NULL,
  size_name = ggplot2::waiver(),
  size_oob = scales::oob_censor,
  size_palette = NULL,
  size_transform = NULL,
  linewidth_type = NULL,
  linewidth_subtype = NULL,
  linewidth_breaks = ggplot2::waiver(),
  linewidth_drop = TRUE,
  linewidth_guide = NULL,
  linewidth_labels = NULL,
  linewidth_limits = NULL,
  linewidth_name = ggplot2::waiver(),
  linewidth_oob = scales::oob_censor,
  linewidth_palette = NULL,
  linewidth_transform = NULL,
  linetype_type = NULL,
  linetype_breaks = ggplot2::waiver(),
  linetype_drop = TRUE,
  linetype_guide = NULL,
  linetype_labels = NULL,
  linetype_limits = NULL,
  linetype_name = ggplot2::waiver(),
  linetype_palette = NULL,
  shape_type = NULL,
  shape_breaks = ggplot2::waiver(),
  shape_drop = TRUE,
  shape_guide = NULL,
  shape_labels = NULL,
  shape_limits = NULL,
  shape_name = ggplot2::waiver(),
  shape_palette = NULL,
  facet_wrap = NULL,
  facet_rows = NULL,
  facet_cols = NULL,
  facet_axes = "margins",
  facet_axis_labels = "all",
  facet_drop = TRUE,
  facet_labeller = "label_value",
  facet_ncol = NULL,
  facet_nrow = NULL,
  facet_scales = "fixed",
  facet_space = "fixed",
  coord_xlim = NULL,
  coord_ylim = NULL,
  coord_clip = NULL,
  coord_reverse = "none",
  coord_ratio = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  ggplot = NULL
)
```

## Arguments

- data:

  A data frame.

- ...:

  Arguments passed to the geom layer, including geom params.

- geom:

  A geom as a string (`"point"`). Note relevant geom library must be
  loaded.

- stat:

  A stat as a string (`"identity"`). Note relevant stat library must be
  loaded.

- position:

  A position as a function
  ([`ggplot2::position_identity()`](https://ggplot2.tidyverse.org/reference/position_identity.html)).

- before:

  A ggplot2 layer to add before the geom layer. Unaffected by border
  transformations.

- with:

  A function to apply to the geom layer.

- focus:

  The orientation focus of the plot. Either `"x"` (default) or `"y"` for
  horizontal plots. Auto-detected from scale types.

- refine:

  A function to refine the theme based on focus and scale types.
  Defaults to
  [`ggrefine::modern`](https://davidhodge931.github.io/ggrefine/reference/modern.html).

- border:

  Whether to apply border colour and linewidth. `TRUE` forces border on,
  `FALSE` forces off.

- x:

  Variable mapped to x.

- xmin:

  Variable mapped to xmin.

- xmax:

  Variable mapped to xmax.

- xend:

  Variable mapped to xend.

- xintercept:

  Variable mapped to xintercept.

- y:

  Variable mapped to y.

- ymin:

  Variable mapped to ymin.

- ymax:

  Variable mapped to ymax.

- yend:

  Variable mapped to yend.

- yintercept:

  Variable mapped to yintercept.

- z:

  Variable mapped to z.

- fill:

  Variable mapped to fill, or a set value. When mapped, colour inherits
  the same mapping unless colour is specified separately.

- colour:

  Variable mapped to colour, or a set value. When not specified and fill
  is mapped, colour inherits from fill.

- alpha:

  Variable mapped to alpha, or a set value.

- shape:

  Variable mapped to shape, or a set value.

- linetype:

  Variable mapped to linetype, or a set value.

- linewidth:

  Variable mapped to linewidth, or a set value.

- size:

  Variable mapped to size, or a set value.

- stroke:

  Variable mapped to stroke, or a set value.

- label:

  Variable mapped to label, or a set value.

- weight:

  Variable mapped to weight, or a set value.

- group:

  Variable mapped to group, or a set value.

- width:

  Variable mapped to width, or a set value.

- height:

  Variable mapped to height, or a set value.

- slope:

  Variable mapped to slope, or a set value.

- intercept:

  Variable mapped to intercept, or a set value.

- sample:

  Variable mapped to sample, or a set value.

- angle:

  Variable mapped to angle, or a set value.

- radius:

  Variable mapped to radius, or a set value.

- mapping:

  Additional aesthetic mappings from
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html),
  merged with individual aesthetic arguments.

- x_type:

  Scale type for x. One of `"continuous"`, `"discrete"`, or `"binned"`.
  Auto-detected if `NULL`.

- x_subtype:

  Scale subtype for x. One of `"date"`, `"datetime"`, `"time"`, or `NA`.
  Auto-detected if `NULL`.

- x_breaks:

  Breaks for the x scale. Defaults to
  [`scales::breaks_pretty()`](https://scales.r-lib.org/reference/breaks_pretty.html)
  for date/datetime subtypes, otherwise
  [`scales::breaks_extended()`](https://scales.r-lib.org/reference/breaks_extended.html).

- x_drop:

  Whether to drop unused levels for a discrete x scale. Defaults to
  `TRUE`.

- x_expand:

  Expansion for the x scale. Defaults to zero expansion where the limit
  is zero, otherwise 5% expansion.

- x_guide:

  Guide for the x scale. Defaults to
  [`ggplot2::waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html).

- x_labels:

  Labels for the x scale. Defaults to
  [`scales::label_date_short()`](https://scales.r-lib.org/reference/label_date.html)
  for date/datetime, otherwise
  [`scales::label_number()`](https://scales.r-lib.org/reference/label_number.html).

- x_limits:

  Limits for the x scale. Accepts a vector or a function.

- x_minor_breaks:

  Minor breaks for the x scale.

- x_name:

  Name/title for the x scale. Defaults to
  [`ggplot2::waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html).

- x_oob:

  Out-of-bounds handler for the x scale. Defaults to
  [scales::oob_censor](https://scales.r-lib.org/reference/oob.html).

- x_palette:

  Palette for a discrete x scale. Defaults to `seq_len`.

- x_position:

  Position of the x axis. Either `"bottom"` (default) or `"top"`.

- x_sec_axis:

  Secondary axis for x. Defaults to
  [`ggplot2::waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html).

- x_transform:

  Transform for the x scale. Auto-detected from subtype if `NULL`.

- y_type:

  Scale type for y. One of `"continuous"`, `"discrete"`, or `"binned"`.
  Auto-detected if `NULL`.

- y_subtype:

  Scale subtype for y. One of `"date"`, `"datetime"`, `"time"`, or `NA`.
  Auto-detected if `NULL`.

- y_breaks:

  Breaks for the y scale. Defaults to
  [`scales::breaks_pretty()`](https://scales.r-lib.org/reference/breaks_pretty.html)
  for date/datetime subtypes, otherwise
  [`scales::breaks_extended()`](https://scales.r-lib.org/reference/breaks_extended.html).

- y_drop:

  Whether to drop unused levels for a discrete y scale. Defaults to
  `TRUE`.

- y_expand:

  Expansion for the y scale. Defaults to zero expansion where the limit
  is zero, otherwise 5% expansion.

- y_guide:

  Guide for the y scale. Defaults to
  [`ggplot2::waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html).

- y_labels:

  Labels for the y scale. Defaults to
  [`scales::label_date_short()`](https://scales.r-lib.org/reference/label_date.html)
  for date/datetime, otherwise
  [`scales::label_number()`](https://scales.r-lib.org/reference/label_number.html).

- y_limits:

  Limits for the y scale. Accepts a vector or a function.

- y_minor_breaks:

  Minor breaks for the y scale.

- y_name:

  Name/title for the y scale. Defaults to
  [`ggplot2::waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html).

- y_oob:

  Out-of-bounds handler for the y scale. Defaults to
  [scales::oob_censor](https://scales.r-lib.org/reference/oob.html).

- y_palette:

  Palette for a discrete y scale. Defaults to `seq_len`.

- y_position:

  Position of the y axis. Either `"left"` (default) or `"right"`.

- y_sec_axis:

  Secondary axis for y. Defaults to
  [`ggplot2::waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html).

- y_transform:

  Transform for the y scale. Auto-detected from subtype if `NULL`.

- fill_type:

  Scale type for fill. One of `"continuous"`, `"discrete"`, or
  `"binned"`. Auto-detected if `NULL`.

- fill_subtype:

  Scale subtype for fill. Auto-detected if `NULL`.

- fill_breaks:

  Breaks for the fill scale.

- fill_drop:

  Whether to drop unused levels for a discrete fill scale. Defaults to
  `TRUE`.

- fill_guide:

  Guide for the fill scale.

- fill_labels:

  Labels for the fill scale.

- fill_limits:

  Limits for the fill scale.

- fill_name:

  Name/title for the fill scale.

- fill_oob:

  Out-of-bounds handler for the fill scale. Defaults to
  [scales::oob_censor](https://scales.r-lib.org/reference/oob.html).

- fill_rescaler:

  Rescaler for the fill scale. Defaults to
  [scales::rescale](https://scales.r-lib.org/reference/rescale.html).

- fill_palette:

  Palette for the fill scale.

- fill_transform:

  Transform for the fill scale. Auto-detected from subtype if `NULL`.

- colour_type:

  Scale type for colour. Inherits from `fill_type` if `NULL`.

- colour_subtype:

  Scale subtype for colour. Inherits from `fill_subtype` if `NULL`.

- colour_breaks:

  Breaks for the colour scale. Inherits from `fill_breaks` if `NULL`.

- colour_drop:

  Whether to drop unused levels for a discrete colour scale. Inherits
  from `fill_drop` if `NULL`.

- colour_guide:

  Guide for the colour scale. Inherits from `fill_guide` if `NULL`.

- colour_labels:

  Labels for the colour scale. Inherits from `fill_labels` if `NULL`.

- colour_limits:

  Limits for the colour scale. Inherits from `fill_limits` if `NULL`.

- colour_name:

  Name/title for the colour scale. Inherits from `fill_name` if `NULL`.

- colour_oob:

  Out-of-bounds handler for the colour scale. Inherits from `fill_oob`
  if `NULL`.

- colour_rescaler:

  Rescaler for the colour scale. Inherits from `fill_rescaler` if
  `NULL`.

- colour_palette:

  Palette for the colour scale.

- colour_transform:

  Transform for the colour scale. Inherits from `fill_transform` if
  `NULL`.

- alpha_type:

  Scale type for alpha. One of `"continuous"`, `"discrete"`, or
  `"binned"`. Auto-detected if `NULL`.

- alpha_subtype:

  Scale subtype for alpha. Auto-detected if `NULL`.

- alpha_breaks:

  Breaks for the alpha scale.

- alpha_drop:

  Whether to drop unused levels for a discrete alpha scale. Defaults to
  `TRUE`.

- alpha_guide:

  Guide for the alpha scale. Defaults to `NULL`.

- alpha_labels:

  Labels for the alpha scale.

- alpha_limits:

  Limits for the alpha scale.

- alpha_name:

  Name/title for the alpha scale.

- alpha_oob:

  Out-of-bounds handler for the alpha scale. Defaults to
  [scales::oob_censor](https://scales.r-lib.org/reference/oob.html).

- alpha_palette:

  Palette for the alpha scale.

- alpha_transform:

  Transform for the alpha scale.

- size_type:

  Scale type for size. One of `"continuous"`, `"discrete"`, or
  `"binned"`. Auto-detected if `NULL`.

- size_subtype:

  Scale subtype for size. Auto-detected if `NULL`.

- size_breaks:

  Breaks for the size scale.

- size_drop:

  Whether to drop unused levels for a discrete size scale. Defaults to
  `TRUE`.

- size_guide:

  Guide for the size scale. Defaults to `NULL`.

- size_labels:

  Labels for the size scale.

- size_limits:

  Limits for the size scale.

- size_name:

  Name/title for the size scale.

- size_oob:

  Out-of-bounds handler for the size scale. Defaults to
  [scales::oob_censor](https://scales.r-lib.org/reference/oob.html).

- size_palette:

  Palette for the size scale.

- size_transform:

  Transform for the size scale.

- linewidth_type:

  Scale type for linewidth. One of `"continuous"`, `"discrete"`, or
  `"binned"`. Auto-detected if `NULL`.

- linewidth_subtype:

  Scale subtype for linewidth. Auto-detected if `NULL`.

- linewidth_breaks:

  Breaks for the linewidth scale.

- linewidth_drop:

  Whether to drop unused levels for a discrete linewidth scale. Defaults
  to `TRUE`.

- linewidth_guide:

  Guide for the linewidth scale. Defaults to `NULL`.

- linewidth_labels:

  Labels for the linewidth scale.

- linewidth_limits:

  Limits for the linewidth scale.

- linewidth_name:

  Name/title for the linewidth scale.

- linewidth_oob:

  Out-of-bounds handler for the linewidth scale. Defaults to
  [scales::oob_censor](https://scales.r-lib.org/reference/oob.html).

- linewidth_palette:

  Palette for the linewidth scale.

- linewidth_transform:

  Transform for the linewidth scale.

- linetype_type:

  Scale type for linetype. Only `"discrete"` is supported.

- linetype_breaks:

  Breaks for the linetype scale.

- linetype_drop:

  Whether to drop unused levels for the linetype scale. Defaults to
  `TRUE`.

- linetype_guide:

  Guide for the linetype scale. Defaults to `NULL`.

- linetype_labels:

  Labels for the linetype scale.

- linetype_limits:

  Limits for the linetype scale.

- linetype_name:

  Name/title for the linetype scale.

- linetype_palette:

  Palette for the linetype scale.

- shape_type:

  Scale type for shape. Only `"discrete"` is supported.

- shape_breaks:

  Breaks for the shape scale.

- shape_drop:

  Whether to drop unused levels for the shape scale. Defaults to `TRUE`.

- shape_guide:

  Guide for the shape scale. Defaults to `NULL`.

- shape_labels:

  Labels for the shape scale.

- shape_limits:

  Limits for the shape scale.

- shape_name:

  Name/title for the shape scale.

- shape_palette:

  Palette for the shape scale.

- facet_wrap:

  Variables to facet by, passed to
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html).
  Accepts a bare variable name or
  [`ggplot2::vars()`](https://ggplot2.tidyverse.org/reference/vars.html)
  for multiple variables.

- facet_rows:

  Row variables for
  [`ggplot2::facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html).
  Accepts a bare variable name or
  [`ggplot2::vars()`](https://ggplot2.tidyverse.org/reference/vars.html)
  for multiple variables.

- facet_cols:

  Column variables for
  [`ggplot2::facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html).
  Accepts a bare variable name or
  [`ggplot2::vars()`](https://ggplot2.tidyverse.org/reference/vars.html)
  for multiple variables.

- facet_axes:

  Which axes to draw on facet panels. Defaults to `"margins"`.

- facet_axis_labels:

  Which axis labels to draw on facet panels. Defaults to `"all"`.

- facet_drop:

  Whether to drop unused factor levels in facets. Defaults to `TRUE`.

- facet_labeller:

  Labeller for facet strip labels. Defaults to `"label_value"`.

- facet_ncol:

  Number of columns for
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html).

- facet_nrow:

  Number of rows for
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html).

- facet_scales:

  Whether facet scales are fixed or free. Defaults to `"fixed"`.

- facet_space:

  Whether facet space is fixed or free. Defaults to `"fixed"`.

- coord_xlim, coord_ylim:

  Zoom limits within the coordinate system.

- coord_clip:

  Whether drawing is clipped to the panel. Either `"on"` or `"on"`.

- coord_reverse:

  Which axes to reverse. One of `"none"` (default), `"x"`, `"y"`, or
  `"xy"`.

- coord_ratio:

  Aspect ratio expressed as y / x, for
  [`ggplot2::coord_cartesian()`](https://ggplot2.tidyverse.org/reference/coord_cartesian.html).

- title:

  Plot title passed to
  [`ggplot2::labs()`](https://ggplot2.tidyverse.org/reference/labs.html).

- subtitle:

  Plot subtitle passed to
  [`ggplot2::labs()`](https://ggplot2.tidyverse.org/reference/labs.html).

- caption:

  Plot caption passed to
  [`ggplot2::labs()`](https://ggplot2.tidyverse.org/reference/labs.html).

- ggplot:

  A base ggplot object to use. Defaults to `NULL`, which uses
  `\(x) ggplot2::ggplot(x)`.

## Value

A ggplot object.

## Examples

``` r
iris |>
  gg_point(
    x = Sepal.Width,
    y = Sepal.Length,
  )
#> Warning: Ignoring unknown parameters: `linewidth`


iris |>
  gg_point(
    x = Sepal.Width,
    y = Sepal.Length,
    fill = Species,
  )
#> Warning: Ignoring unknown parameters: `linewidth`


iris |>
  gg_point(
    x = Sepal.Width,
    y = Sepal.Length,
    facet_wrap = Species,
  )
#> Warning: Ignoring unknown parameters: `linewidth`


tibble::tibble(
  x = c(1, 3),
  xend = c(2, 4),
  y = c(1, 3),
  yend = c(2, 4),
) |>
  gg_segment(
    x = x,
    xend = xend,
    y = y,
    yend = yend,
  )
#> Warning: Ignoring unknown parameters: `fill`


tibble::tibble(
  x = c(1, 3),
  xend = c(2, 4),
  y = c(1, 3),
  yend = c(2, 4),
  group = c("a", "b"),
) |>
  gg_segment(
    x = x,
    xend = xend,
    y = y,
    yend = yend,
    fill = group,
  )
#> Warning: Ignoring unknown aesthetics: fill


tibble::tibble(
  x = c(1, 3),
  xend = c(2, 4),
  y = c(1, 3),
  yend = c(2, 4),
  group = c("a", "b"),
) |>
  gg_segment(
    x = x,
    xend = xend,
    y = y,
    yend = yend,
    facet_wrap = group,
  )
#> Warning: Ignoring unknown parameters: `fill`

```
