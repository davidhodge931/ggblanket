# Demo: dark mode

## Overview

This article will demonstrate each wrapper function.

``` r
library(dplyr)
library(ggplot2)
library(scales)
library(ggblanket)
library(palmerpenguins)

set_blanket(
  col_palette_d = c(teal, orange, purple, red, pink, navy),
  col_palette_c = viridis::mako(n = 9),
  col_palette_o = scales::pal_viridis(option = "G"),
  theme = dark_mode_r(),  
)

penguins2 <- penguins |> 
  labelled::set_variable_labels(
    bill_length_mm = "Bill length (mm)",
    bill_depth_mm = "Bill depth (mm)",
    flipper_length_mm = "Flipper length (mm)",
    body_mass_g = "Body mass (g)",
  ) |> 
  mutate(sex = stringr::str_to_sentence(sex)) |> 
  tidyr::drop_na(sex)

experiment <- data.frame(
  trt = factor(c(1, 1, 2, 2)),
  resp = c(1, 5, 3, 4),
  group = factor(c(1, 2, 1, 2)),
  upper = c(1.1, 5.3, 3.3, 4.2),
  lower = c(0.8, 4.6, 2.4, 3.6)) |>
  labelled::set_variable_labels(
    trt = "Treatment", 
    resp = "Response"
  )
```

### gg_area

``` r
economics |>
  gg_area(
    x = date,
    y = unemploy,
    y_label = "Unemployment",
  )
```

![](2_demo_dark_files/figure-html/unnamed-chunk-2-1.png)

### gg_bar

``` r
penguins2 |>
  gg_bar(
    position = position_dodge(preserve = "single"),
    y = species,
    col = sex,
    width = 0.75,
  )
```

![](2_demo_dark_files/figure-html/unnamed-chunk-3-1.png)

### gg_bin_2d

``` r
diamonds |>
  gg_bin_2d(
    x = carat,
    y = price,
    coord = coord_cartesian(),
  )
```

![](2_demo_dark_files/figure-html/unnamed-chunk-4-1.png)

### gg_boxplot

``` r
penguins2 |>
  gg_boxplot(
    x = flipper_length_mm,
    y = sex,
    col = species,
    blend = "multiply",
  )
```

![](2_demo_dark_files/figure-html/unnamed-chunk-5-1.png)

### gg_col

``` r
penguins2 |>
  group_by(sex, species) |>
  summarise(across(flipper_length_mm, \(x) mean(x, na.rm = TRUE))) |>
  labelled::copy_labels_from(penguins2) |>
  gg_col(
    position = position_dodge(preserve = "single"),
    x = flipper_length_mm,
    y = species,
    col = sex,
    width = 0.75,
  )
```

![](2_demo_dark_files/figure-html/unnamed-chunk-6-1.png)

### gg_contour

``` r
faithfuld |>
 gg_contour(
   x = waiting,
   y = eruptions,
   z = density,
  )
```

![](2_demo_dark_files/figure-html/unnamed-chunk-7-1.png)

### gg_contour_filled

``` r
faithfuld |>
 gg_contour_filled(
   x = waiting,
   y = eruptions,
   z = density,
   bins = 8,
  )
```

![](2_demo_dark_files/figure-html/unnamed-chunk-8-1.png)

### gg_crossbar

``` r
experiment |>
  gg_crossbar(
    x = trt,
    y = resp,
    ymin = lower,
    ymax = upper,
    col = group,
    width = 0.5,
    y_limits_include = 0,
    blend = "multiply",
  )
```

![](2_demo_dark_files/figure-html/unnamed-chunk-9-1.png)

### gg_density

``` r
penguins2 |>
  gg_density(
    x = flipper_length_mm,
    col = species,
    blend = "multiply",
  )
```

![](2_demo_dark_files/figure-html/unnamed-chunk-10-1.png)

### gg_density_2d

``` r
faithful |>
  gg_density_2d(
    x = waiting,
    y = eruptions,
    bins = 8,
    contour = TRUE,
  )
```

![](2_demo_dark_files/figure-html/unnamed-chunk-11-1.png)

### gg_density_2d_filled

``` r
faithful |>
  gg_density_2d_filled(
    x = waiting,
    y = eruptions,
    bins = 8,
    contour = TRUE,
  )
```

![](2_demo_dark_files/figure-html/unnamed-chunk-12-1.png)

### gg_errorbar

``` r
experiment |>
  gg_errorbar(
    x = trt,
    ymin = lower,
    ymax = upper,
    col = group,
    y_label = "Response",
    width = 0.1,
    y_limits_include = 0,
 )
```

![](2_demo_dark_files/figure-html/unnamed-chunk-13-1.png)

### gg_freqpoly

``` r
penguins2 |>
  gg_freqpoly(
    x = flipper_length_mm,
    col = sex,
    theme = dark_mode_t(),
  ) +
  theme(legend.title = element_blank())
```

![](2_demo_dark_files/figure-html/unnamed-chunk-14-1.png)

### gg_function

``` r
gg_function(
  fun = \(x) dnorm(x, mean = 0, sd = 5),
  x_limits_include = qnorm(p = c(0.005, 0.995), mean = 0, sd = 5),
  x_label = "X",
  y_limits_include = 0,
)
```

![](2_demo_dark_files/figure-html/unnamed-chunk-15-1.png)

### gg_hex

``` r
diamonds |>
  gg_hex(
    x = carat,
    y = price,
  )
```

![](2_demo_dark_files/figure-html/unnamed-chunk-16-1.png)

### gg_histogram

``` r
penguins2 |>
  gg_histogram(
    x = flipper_length_mm,
    col = sex,
  ) 
```

![](2_demo_dark_files/figure-html/unnamed-chunk-17-1.png)

### gg_jitter

``` r
set.seed(123)

penguins2 |>
  gg_jitter(
    position = position_jitter(), 
    x = species,
    y = body_mass_g,
    col = flipper_length_mm,
    y_limits_include = 0,
    col_steps = TRUE,
  )
```

![](2_demo_dark_files/figure-html/unnamed-chunk-18-1.png)

### gg_label

``` r
bind_rows(
  mtcars |> slice_min(order_by = mpg),
  mtcars |> slice_max(order_by = mpg)) |>
  tibble::rownames_to_column("model") |>
  gg_label(
    x = model,
    y = mpg,
    label = model,
    y_limits_include = 0,
    y_label = "Miles per gallon", 
  )
```

![](2_demo_dark_files/figure-html/unnamed-chunk-19-1.png)

### gg_line

``` r
economics |>
  gg_line(
    x = date,
    y = unemploy,
    y_limits_include = 0, 
    y_label = "Unemployment",
  )
```

![](2_demo_dark_files/figure-html/unnamed-chunk-20-1.png)

### gg_linerange

``` r
experiment |>
  gg_linerange(
    position = position_dodge(width = 0.2),
    x = trt,
    ymin = lower,
    ymax = upper,
    col = group,
    y_label = "Response",
    y_limits_include = 0,
  )
```

![](2_demo_dark_files/figure-html/unnamed-chunk-21-1.png)

### gg_path

``` r
economics |>
  mutate(unemploy_rate = unemploy / pop) |>
  gg_path(
    x = unemploy_rate,
    y = psavert,
    x_label = "Unemployment rate",
    y_limits_include = 0,
    y_label = "Personal savings rate",
  )
```

![](2_demo_dark_files/figure-html/unnamed-chunk-22-1.png)

### gg_point

``` r
penguins2 |>
  gg_point(
    x = flipper_length_mm,
    y = body_mass_g, 
    col = species,
  )
```

![](2_demo_dark_files/figure-html/unnamed-chunk-23-1.png)

### gg_pointrange

``` r
experiment |>
  gg_pointrange(
    position = position_dodge(width = 0.2),
    x = trt,
    y = resp,
    col = group,
    ymin = lower,
    ymax = upper,
    y_limits_include = 0,
  )
```

![](2_demo_dark_files/figure-html/unnamed-chunk-24-1.png)

### gg_polygon

``` r
ids <- factor(c("1.1", "2.1", "1.2", "2.2", "1.3", "2.3"))

values <- data.frame(
  id = ids,
  value = c(3, 3.1, 3.1, 3.2, 3.15, 3.5)
)

positions <- data.frame(
  id = rep(ids, each = 4),
  x = c(2, 1, 1.1, 2.2, 1, 0, 0.3, 1.1, 2.2, 1.1, 1.2, 2.5, 1.1, 0.3,
        0.5, 1.2, 2.5, 1.2, 1.3, 2.7, 1.2, 0.5, 0.6, 1.3),
  y = c(-0.5, 0, 1, 0.5, 0, 0.5, 1.5, 1, 0.5, 1, 2.1, 1.7, 1, 1.5,
        2.2, 2.1, 1.7, 2.1, 3.2, 2.8, 2.1, 2.2, 3.3, 3.2)
)

datapoly <- merge(values, positions, by = c("id"))

datapoly |>
  gg_polygon(
    x = x,
    y = y,
    col = value,
    group = id, 
  )
```

![](2_demo_dark_files/figure-html/unnamed-chunk-25-1.png)

### gg_qq

``` r
penguins2 |>
  gg_qq(
    sample = body_mass_g,
    facet = species,
    coord = coord_cartesian(clip = "on"), 
  ) +
  geom_qq_line()
```

![](2_demo_dark_files/figure-html/unnamed-chunk-26-1.png)

### gg_quantile

``` r
penguins2 |>
  gg_quantile(
    x = flipper_length_mm,
    y = body_mass_g,
  )
```

![](2_demo_dark_files/figure-html/unnamed-chunk-27-1.png)

### gg_raster

``` r
faithfuld |>
  gg_raster(
    x = waiting,
    y = eruptions,
    col = density,
  )
```

![](2_demo_dark_files/figure-html/unnamed-chunk-28-1.png)

### gg_rect

``` r
data.frame(
  x = rep(c(2, 5, 7, 9, 12), 2),
  y = rep(c(1, 2), each = 5),
  z = factor(c(rep(1:4, each = 2), 5, NA)),
  w = rep(diff(c(0, 4, 6, 8, 10, 14)), 2)) |>
  mutate(
    xmin = x - w / 2,
    xmax = x + w / 2,
    ymin = y,
    ymax = y + 1
  ) |>
  gg_rect(
    xmin = xmin,
    xmax = xmax,
    ymin = ymin,
    ymax = ymax,
    col = z,
  )
```

![](2_demo_dark_files/figure-html/unnamed-chunk-29-1.png)

### gg_ribbon

``` r
data.frame(year = 1875:1972, level = as.vector(LakeHuron)) |>
  mutate(level_min = level - 1, level_max = level + 1) |>
  gg_ribbon(
    x = year,
    ymin = level_min,
    ymax = level_max,
    x_labels = \(x) x,
    y_label = "Level",
  ) 
```

![](2_demo_dark_files/figure-html/unnamed-chunk-30-1.png)

### gg_rug

``` r
penguins2 |>
  gg_rug(
    x = flipper_length_mm,
    y = body_mass_g,
    col = sex,
  )
```

![](2_demo_dark_files/figure-html/unnamed-chunk-31-1.png)

### gg_segment

``` r
data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0) |>
  gg_segment(
    x = x1,
    xend = x2,
    y = y1,
    yend = y2,
  )
```

![](2_demo_dark_files/figure-html/unnamed-chunk-32-1.png)

### gg_sf

``` r
sf::st_read(system.file("shape/nc.shp", package = "sf")) |> 
  gg_sf(
    col = AREA, 
  )
#> Reading layer `nc' from data source 
#>   `/home/runner/work/_temp/Library/sf/shape/nc.shp' using driver `ESRI Shapefile'
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27
```

![](2_demo_dark_files/figure-html/unnamed-chunk-33-1.png)

### gg_smooth

``` r
penguins2 |>
  gg_smooth(
    x = flipper_length_mm,
    y = body_mass_g,
    col = sex,
    blend = "multiply",
    se = TRUE,
  )
```

![](2_demo_dark_files/figure-html/unnamed-chunk-34-1.png)

### gg_step

``` r
economics |>
  filter(date > lubridate::ymd("2010-01-01")) |>  
  gg_step(
    x = date,
    y = unemploy,
    y_limits_include = 0,
    y_label = "Unemployment",
  )
```

![](2_demo_dark_files/figure-html/unnamed-chunk-35-1.png)

### gg_text

``` r
bind_rows(
  mtcars |> slice_min(order_by = mpg),
  mtcars |> slice_max(order_by = mpg)) |>
  tibble::rownames_to_column("model") |>
  gg_text(
    x = model,
    y = mpg,
    label = model,
    y_limits_include = 0,
    y_label = "Miles per gallon",
  )
```

![](2_demo_dark_files/figure-html/unnamed-chunk-36-1.png)

### gg_tile

``` r
penguins2 |>
  group_by(species, sex) |>
  summarise(flipper_length_mm = mean(flipper_length_mm, na.rm = TRUE)) |>
  labelled::copy_labels_from(penguins2) |>
  gg_tile(
    x = sex,
    y = species,
    col = flipper_length_mm,
  )
```

![](2_demo_dark_files/figure-html/unnamed-chunk-37-1.png)

### gg_violin

``` r
penguins2 |>
  gg_violin(
    x = sex,
    y = body_mass_g,
    col = species,
  )
```

![](2_demo_dark_files/figure-html/unnamed-chunk-38-1.png)

### gg_blanket

``` r
penguins2 |>
  gg_blanket(
    geom = "violin",
    stat = "ydensity",
    position = "dodge",
    x = sex,
    y = body_mass_g,
    col = species,
  )
```

![](2_demo_dark_files/figure-html/unnamed-chunk-39-1.png)
