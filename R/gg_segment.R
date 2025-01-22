#' Segment ggplot
#'
#' @description Create a segment ggplot with a wrapper around [ggplot2::ggplot()] + [geom_segment()][ggplot2::geom_segment()].
#'
#' @inheritParams gg_blanket
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(palmerpenguins)
#'
#' set_blanket()
#'
#' data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0) |>
#'   gg_segment(
#'     x = x1,
#'     xend = x2,
#'     y = y1,
#'     yend = y2,
#'   )
#'
gg_segment <- function(data = NULL,
                       ...,
                       stat = "identity",
                       position = "identity",
                       coord = ggplot2::coord_cartesian(clip = "off"),
                       theme = NULL, theme_orientation = NULL, theme_axis_line_rm = NULL,  theme_axis_ticks_rm = NULL,  theme_panel_grid_rm = NULL, blend = NULL,
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
                       x_breaks = NULL, x_breaks_n = NULL, 
                       x_expand = NULL,
                       x_expand_limits = NULL,
                       x_label = NULL, x_labels = NULL, 
                       
                       
                       x_position = "bottom", 
                       
                       x_sec_axis = ggplot2::waiver(), x_symmetric = NULL, x_transform = NULL,
                       y_breaks = NULL, y_breaks_n = NULL,
                       y_expand = NULL,
                       y_expand_limits = NULL,
                       y_label = NULL, y_labels = NULL,
                        y_position = "left",
                       y_sec_axis = ggplot2::waiver(),
                       y_symmetric = NULL, 
                       
                       y_transform = NULL,
                       col_breaks = NULL, col_breaks_n = 5,
                       col_drop = FALSE,
                       col_expand_limits = NULL,
                       col_label = NULL, col_labels = NULL,
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
                       label_case = NULL) {
  gg_blanket(
    data = data,
    geom = "segment",
    stat = stat,
    position = position,
    coord = coord,
    theme = theme, theme_orientation = theme_orientation, theme_axis_line_rm = theme_axis_line_rm ,  theme_axis_ticks_rm = theme_axis_ticks_rm,  theme_panel_grid_rm = theme_panel_grid_rm, blend = blend,
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
    facet = {{ facet }},
    facet2 = {{ facet2 }},
    group = {{ group }},
    subgroup = {{ subgroup }},
    label = {{ label }},
    text = {{ text }},
    sample = {{ sample }},
    mapping = mapping,
    x_breaks = x_breaks,
    x_expand = x_expand,
    x_expand_limits = x_expand_limits,
    x_labels = x_labels,
    x_breaks_n = x_breaks_n,
    x_sec_axis = x_sec_axis,
    x_symmetric = x_symmetric, x_position = x_position,
    x_label = x_label,
    x_transform = x_transform,
    y_breaks = y_breaks,
    y_expand = y_expand,
    y_expand_limits = y_expand_limits,
    y_labels = y_labels,
    y_breaks_n = y_breaks_n,
    y_sec_axis = y_sec_axis,
    y_symmetric = y_symmetric, y_position = y_position,
    y_label = y_label,
    y_transform = y_transform,
    col_breaks = col_breaks,
    col_drop = col_drop,
    col_expand_limits = col_expand_limits,
    col_labels = col_labels,
    col_legend_ncol = col_legend_ncol,
    col_legend_nrow = col_legend_nrow,
    col_legend_rev = col_legend_rev,
    col_breaks_n = col_breaks_n,
    
    col_palette = col_palette,
    col_palette_na = col_palette_na,
    col_rescale = col_rescale,
    col_steps = col_steps,
    col_label = col_label,
    col_transform = col_transform,
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
    label_case = label_case,
    ...
  )
}
