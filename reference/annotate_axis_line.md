# Annotated axis line segment

Replace axis line with an annotated segment.

## Usage

``` r
annotate_axis_line(position, ..., colour = NULL, linewidth = NULL)
```

## Arguments

- position:

  The position of the axis. One of "bottom", "top", "left", or "right".

- ...:

  Extra parameters passed to `ggplot2::annotate("segment", ...)`.

- colour:

  The colour of the annotated segment. Inherits from the current theme
  axis.line etc.

- linewidth:

  The linewidth of the annotated segment. Inherits from the current
  theme axis.line etc.

## Value

A list of a annotate layer and theme elements.
