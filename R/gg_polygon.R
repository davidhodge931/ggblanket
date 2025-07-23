#' Polygon ggplot
#'
#' @description Create a polygon ggplot with a wrapper around [ggplot()][ggplot2::ggplot()] + [layer()][ggplot2::layer()] with [geom_polygon()][ggplot2::geom_polygon()] defaults for the geom, stat and position.
#'
#' @inheritParams gg_blanket
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' set_blanket()
#'
#' ids <- factor(c("1.1", "2.1", "1.2", "2.2", "1.3", "2.3"))
#'
#' values <- data.frame(
#'   id = ids,
#'   value = c(3, 3.1, 3.1, 3.2, 3.15, 3.5)
#' )
#'
#' positions <- data.frame(
#'   id = rep(ids, each = 4),
#'   x = c(
#'     2, 1, 1.1, 2.2, 1, 0, 0.3, 1.1, 2.2, 1.1, 1.2, 2.5, 1.1, 0.3,
#'     0.5, 1.2, 2.5, 1.2, 1.3, 2.7, 1.2, 0.5, 0.6, 1.3
#'   ),
#'   y = c(
#'     -0.5, 0, 1, 0.5, 0, 0.5, 1.5, 1, 0.5, 1, 2.1, 1.7, 1, 1.5,
#'     2.2, 2.1, 1.7, 2.1, 3.2, 2.8, 2.1, 2.2, 3.3, 3.2
#'   )
#' )
#'
#' datapoly <- merge(values, positions, by = c("id"))
#'
#' datapoly |>
#'   gg_polygon(
#'     x = x,
#'     y = y,
#'     col = value,
#'     group = id,
#'   )
#'
gg_polygon <- function(
  data = NULL,
  ...,
  stat = "identity",
  position = "identity",
  coord = ggplot2::coord_cartesian(clip = "off"),
  blend = NULL, 
  perspective = NULL,
  axis_line_transparent = NULL,
  axis_ticks_transparent = NULL,
  panel_grid_transparent = NULL,

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
  colour = NULL, fill = NULL, shape = NULL, linetype = NULL, linewidth = NULL, size = NULL,facet = NULL,
  facet2 = NULL,
  group = NULL,
  subgroup = NULL,
  label = NULL,
  text = NULL,
  sample = NULL,
  mapping = NULL, border = NULL, border_colour = NULL, border_fill = NULL, border_linewidth = NULL,
  x_breaks = NULL,
  x_breaks_n = NULL,
  x_expand = NULL,
  x_limits_include = NULL,
  x_title = NULL,
  x_labels = NULL,
  x_position = "bottom",
  x_sec_axis = ggplot2::waiver(),
  x_symmetric = NULL,
  x_transform = NULL,
  y_breaks = NULL,
  y_breaks_n = NULL,
  y_expand = NULL,
  y_limits_include = NULL,
  y_title = NULL,
  y_labels = NULL,
  y_position = "left",
  y_sec_axis = ggplot2::waiver(),
  y_symmetric = NULL,
  y_transform = NULL,
   col_breaks = ggplot2::waiver(), 
  col_breaks_n = NULL,
  col_drop = FALSE,
  col_limits_include = NULL,
  col_title = NULL,
  col_labels = NULL,
  col_legend_ncol = NULL,
  col_legend_nrow = NULL,
  col_legend_rev = FALSE, col_palette = NULL, col_palette_na = NULL,  
  
  
  col_rescale = scales::rescale(),
  col_scale_type = "gradient",
  col_transform = NULL, colour_palette = NULL, colour_palette_na = NULL, fill_palette = NULL, fill_palette_na = NULL, 
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
  titles_case = NULL
) {
  gg_blanket(
    data = data,
    geom = "polygon",
    stat = stat,
    position = position,
    coord = coord,
    
    perspective = perspective,
    axis_line_transparent = axis_line_transparent,
    axis_ticks_transparent = axis_ticks_transparent,
    panel_grid_transparent = panel_grid_transparent,
    blend = blend,
    x = {{ x }},
    y = {{ y }},
    xmin = {{ xmin }},
    xmax = {{ xmax }},
    xend = {{ xend }},
    ymin = {{ ymin }},
    ymax = {{ ymax }},
    yend = {{ yend }},
    z = {{ z }},
    col = {{ col }},
    colour = {{ colour }}, fill = {{ fill }}, shape = {{ shape }}, linetype = {{ linetype }}, linewidth = {{ linewidth }}, size = {{ size }}, alpha = {{ alpha }},facet = {{ facet }},
    facet2 = {{ facet2 }},
    group = {{ group }},
    subgroup = {{ subgroup }},
    label = {{ label }},
    text = {{ text }},
    sample = {{ sample }},
    mapping = mapping,
    x_breaks = x_breaks,
    x_expand = x_expand,
    x_limits_include = x_limits_include,
    x_labels = x_labels,
    x_breaks_n = x_breaks_n,
    x_sec_axis = x_sec_axis,
    x_symmetric = x_symmetric,
    x_position = x_position,
    x_title = x_title,
    x_transform = x_transform,
    y_breaks = y_breaks,
    y_expand = y_expand,
    y_limits_include = y_limits_include,
    y_labels = y_labels,
    y_breaks_n = y_breaks_n,
    y_sec_axis = y_sec_axis,
    y_symmetric = y_symmetric,
    y_position = y_position,
    y_title = y_title,
    y_transform = y_transform,
    border = border, col_breaks = col_breaks, col_breaks_n = col_breaks_n,
    col_drop = col_drop,
    col_limits_include = col_limits_include,
    col_labels = col_labels,
    col_legend_ncol = col_legend_ncol,
    col_legend_nrow = col_legend_nrow,
    col_legend_rev = col_legend_rev, col_palette = col_palette, col_palette_na = col_palette_na,   
    
    
    
    col_rescale = col_rescale,
    col_scale_type = col_scale_type,
    col_title = col_title,
    col_transform = col_transform, colour_palette = colour_palette, colour_palette_na = colour_palette_na, fill_palette = fill_palette, fill_palette_na = fill_palette_na, 
    facet_axes = facet_axes,
    facet_axis_labels = facet_axis_labels,
    facet_drop = facet_drop,
    facet_labels = facet_labels,
    facet_layout = facet_layout,
    facet_ncol = facet_ncol,
    facet_nrow = facet_nrow,
    facet_scales = facet_scales,
    facet_space = facet_space,
    title = title,
    subtitle = subtitle,
    caption = caption,
    titles_case = titles_case, 
    ...
  )
}
