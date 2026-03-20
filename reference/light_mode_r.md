# Light mode theme family

A dark mode family of functions:

- `light_mode_r()` with legend on right

- `light_mode_t()` with legend on top

- `light_mode_b()` with legend on bottom

## Usage

``` r
light_mode_r(
  ...,
  base_size = 11,
  base_family = "",
  base_colour = "#121B24FF",
  axis_line_colour = "#121B24FF",
  axis_line_linewidth = 0.25,
  axis_ticks_colour = axis_line_colour,
  axis_ticks_linewidth = axis_line_linewidth,
  axis_ticks_length = grid::unit(11/3, "pt"),
  panel_grid_colour = "#F6F8FAFF",
  panel_grid_linewidth = 1.33,
  panel_background_fill = "#FFFFFFFF",
  plot_background_fill = "#FFFFFFFF",
  legend_axis_line_colour = plot_background_fill,
  legend_axis_line_linewidth = axis_line_linewidth,
  legend_background_fill = plot_background_fill,
  legend_key_fill = plot_background_fill,
  legend_ticks_colour = legend_axis_line_colour,
  legend_ticks_linewidth = legend_axis_line_linewidth,
  legend_ticks_length = ggplot2::rel(c(0.175, 0))
)

light_mode_t(
  ...,
  base_size = 11,
  base_family = "",
  base_colour = "#121B24FF",
  axis_line_colour = "#121B24FF",
  axis_line_linewidth = 0.25,
  axis_ticks_colour = axis_line_colour,
  axis_ticks_linewidth = axis_line_linewidth,
  axis_ticks_length = grid::unit(11/3, "pt"),
  panel_grid_colour = "#F6F8FAFF",
  panel_grid_linewidth = 1.33,
  panel_background_fill = "#FFFFFFFF",
  plot_background_fill = "#FFFFFFFF",
  legend_axis_line_colour = plot_background_fill,
  legend_axis_line_linewidth = axis_line_linewidth,
  legend_background_fill = plot_background_fill,
  legend_key_fill = plot_background_fill,
  legend_ticks_colour = legend_axis_line_colour,
  legend_ticks_linewidth = legend_axis_line_linewidth,
  legend_ticks_length = ggplot2::rel(c(0.175, 0))
)

light_mode_b(
  ...,
  base_size = 11,
  base_family = "",
  base_colour = "#121B24FF",
  axis_line_colour = "#121B24FF",
  axis_line_linewidth = 0.25,
  axis_ticks_colour = axis_line_colour,
  axis_ticks_linewidth = axis_line_linewidth,
  axis_ticks_length = grid::unit(11/3, "pt"),
  panel_grid_colour = "#F6F8FAFF",
  panel_grid_linewidth = 1.33,
  panel_background_fill = "#FFFFFFFF",
  plot_background_fill = "#FFFFFFFF",
  legend_axis_line_colour = plot_background_fill,
  legend_axis_line_linewidth = axis_line_linewidth,
  legend_background_fill = plot_background_fill,
  legend_key_fill = plot_background_fill,
  legend_ticks_colour = legend_axis_line_colour,
  legend_ticks_linewidth = legend_axis_line_linewidth,
  legend_ticks_length = ggplot2::rel(c(0.175, 0))
)
```

## Arguments

- ...:

  Provided to require argument naming, support trailing commas etc.

- base_size:

  The base size of the text theme element. Defaults to 11.

- base_family:

  The base family of the text theme element. Defaults to "".

- base_colour:

  The base colour of the text theme element.

- axis_line_colour:

  The colour of the axis.line theme element.

- axis_line_linewidth:

  The linewidth of the axis.line theme element.

- axis_ticks_colour:

  The colour of the axis.ticks theme element.

- axis_ticks_linewidth:

  The linewidth of the axis.ticks theme element.

- axis_ticks_length:

  The length of the axis.ticks.length theme element.

- panel_grid_colour:

  The colour of the panel.grid theme element.

- panel_grid_linewidth:

  The linewidth of the panel.grid theme element.

- panel_background_fill:

  The fill (and colour) of the panel.background theme element.

- plot_background_fill:

  The fill (and colour) of the plot.background theme element.

- legend_axis_line_colour:

  The colour of the legend.axis.line theme element.

- legend_axis_line_linewidth:

  The linewidth of the legend.axis.line theme element.

- legend_background_fill:

  The fill (and colour) of the legend.background theme element.

- legend_key_fill:

  The fill (and colour) of the legend.key theme element.

- legend_ticks_colour:

  The colour of the legend.ticks theme element.

- legend_ticks_linewidth:

  The linewidth of the legend.ticks theme element.

- legend_ticks_length:

  The legend.ticks.length theme element.

## Value

A ggplot theme.

## Examples

``` r
library(palmerpenguins)
library(ggplot2)

set_blanket()
#> Warning: Duplicated aesthetics after name standardisation: fill
#> Warning: Duplicated aesthetics after name standardisation: fill

penguins |>
  gg_point(
    x = flipper_length_mm,
    y = body_mass_g,
    col = species,
    mode = light_mode_r()
  )
#> Warning: Ignoring unknown parameters: `mode`
#> Warning: Ignoring unknown parameters: `mode`
#> Scale for colour is already present.
#> Adding another scale for colour, which will replace the existing scale.
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_point()`).


penguins |>
  gg_point(
    x = flipper_length_mm,
    y = body_mass_g,
    col = species,
    mode = light_mode_t()
  )
#> Warning: Ignoring unknown parameters: `mode`
#> Warning: Ignoring unknown parameters: `mode`
#> Scale for colour is already present.
#> Adding another scale for colour, which will replace the existing scale.
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_point()`).


penguins |>
  gg_point(
    x = flipper_length_mm,
    y = body_mass_g,
    col = species,
    mode = light_mode_b()
  )
#> Warning: Ignoring unknown parameters: `mode`
#> Warning: Ignoring unknown parameters: `mode`
#> Scale for colour is already present.
#> Adding another scale for colour, which will replace the existing scale.
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_point()`).

```
