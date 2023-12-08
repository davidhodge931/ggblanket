#' @title Blank ggplot
#'
#' @description Create a blank ggplot with a wrapper around ggplot2::geom_blank().
#' @param data A data frame or tibble.
#' @param x Unquoted x aesthetic variable.
#' @param y Unquoted y aesthetic variable.
#' @param col Unquoted col and fill aesthetic variable.
#' @param facet Unquoted facet aesthetic variable.
#' @param facet2 Unquoted second facet variable.
#' @param group Unquoted group aesthetic variable.
#' @param xmin Unquoted xmin aesthetic variable.
#' @param xmax Unquoted xmax aesthetic variable.
#' @param xend Unquoted xend aesthetic variable.
#' @param ymin Unquoted ymin aesthetic variable.
#' @param ymax Unquoted ymax aesthetic variable.
#' @param yend Unquoted yend aesthetic variable.
#' @param z Unquoted z aesthetic variable.
#' @param sample Unquoted sample aesthetic variable.
#' @param label Unquoted label aesthetic variable.
#' @param subgroup Unquoted subgroup aesthetic variable.
#' @param mapping Map additional aesthetics using the ggplot2::aes function (e.g. shape). Excludes colour, fill or alpha.
#' @param pal Colours to use. A character vector of hex codes (or names).
#' @param pal_na Colour to use for NA values. A character vector of a hex code (or name).
#' @param alpha Opacity. A number between 0 and 1.
#' @param ... Other arguments passed to within a params list in the layer function.
#' @param title Title string.
#' @param subtitle Subtitle string.
#' @param x_breaks A scales::breaks_* function (e.g. scales::breaks_pretty()), or a vector of breaks.
#' @param x_expand Padding to the limits with the ggplot2::expansion function, or a vector of length 2 (e.g. c(0, 0)).
#' @param x_gridlines TRUE or FALSE for vertical x gridlines. NULL guesses based on the classes of the x and y.
#' @param x_include For a continuous x variable, any values that the limits should encompass (e.g. 0).
#' @param x_labels A function that takes the breaks as inputs (e.g. scales::label_comma()), or a vector of labels.
#' @param x_limits A vector of length 2 to determine the limits of the axis.
#' @param x_oob For a continuous x variable, a scales::oob_* function of how to handle values outside of limits (e.g. scales::oob_keep). Defaults to scales::oob_keep.
#' @param x_sec_axis A secondary axis using the ggplot2::sec_axis or ggplot2::dup_axis function.
#' @param x_title Axis title string. Use "" for no title.
#' @param x_trans For a numeric x variable, a transformation object (e.g. "log10", "sqrt" or "reverse").
#' @param y_breaks A scales::breaks_* function (e.g. scales::breaks_pretty()), or a vector of breaks.
#' @param y_expand Padding to the limits with the ggplot2::expansion function, or a vector of length 2 (e.g. c(0, 0)).
#' @param y_gridlines TRUE or FALSE of horizontal y gridlines. NULL guesses based on the classes of the x and y.
#' @param y_include For a continuous y variable, any values that the limits should encompass (e.g. 0).
#' @param y_labels A function that takes the breaks as inputs (e.g. scales::label_comma()), or a vector of labels.
#' @param y_limits A vector of length 2 to determine the limits of the axis.
#' @param y_oob For a continuous y variable, a scales::oob_* function of how to handle values outside of limits (e.g. scales::oob_keep). Defaults to scales::oob_keep.
#' @param y_sec_axis A secondary axis using the ggplot2::sec_axis or ggplot2::dup_axis function.
#' @param y_title Axis title string. Use "" for no title.
#' @param y_trans For a numeric y variable, a transformation object (e.g. "log10", "sqrt" or "reverse").
#' @param col_breaks A scales::breaks_* function (e.g. scales::breaks_pretty()), or a vector of breaks.
#' @param col_continuous For a continuous col variable, the type of colouring. Either "gradient" or "steps". Defaults to "gradient".
#' @param col_expand Padding to the limits with the ggplot2::expansion function, or a vector of length 2 (e.g. c(0, 0)).
#' @param col_include For a continuous col variable, any values that the limits should encompass (e.g. 0).
#' @param col_labels A function that takes the breaks as inputs (e.g. scales::label_comma()), or a vector of labels.
#' @param col_legend_ncol The number of columns for the legend elements.
#' @param col_legend_nrow The number of rows for the legend elements.
#' @param col_legend_place The place for the legend. Either "bottom", "right", "top" or "left". Or just the first letter of each.
#' @param col_legend_rev Reverse the elements of the legend. Defaults to FALSE.
#' @param col_limits A vector to determine the limits of the colour scale.
#' @param col_oob For a continuous col variable, a scales::oob_* function of how to handle values outside of limits (e.g. scales::oob_keep). Defaults to scales::oob_keep.
#' @param col_rescale For a continuous col variable, a scales::rescale function.
#' @param col_title Legend title string. Use "" for no title.
#' @param col_trans For a numeric col variable, a transformation object (e.g. "log10", "sqrt" or "reverse").
#' @param facet_labels A function that takes the breaks as inputs (e.g. scales::label_comma()), or a named vector of labels (e.g. c("value" = "label", ...)).
#' @param facet_ncol The number of columns of facets. Only applies to a facet layout of "wrap".
#' @param facet_nrow The number of rows of facets. Only applies to a facet layout of "wrap".
#' @param facet_scales Whether facet scales should be "fixed" across facets, "free" in both directions, or free in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed".
#' @param facet_space Whether facet space should be "fixed" across facets, "free" to be proportional in both directions, or free to be proportional in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed". Only applies where the facet layout is "grid" and facet scales are not "fixed".
#' @param facet_layout Whether the layout is to be "wrap" or "grid". If NULL and a single facet (or facet2) argument is provided, then defaults to "wrap". If NULL and both facet and facet2 arguments are provided, defaults to "grid".
#' @param facet_switch Whether the facet layout is "grid", whether to switch the facet labels to the opposite side of the plot. Either "x", "y" or "both".
#' @param linetype_title Legend title string. Use "" for no title.
#' @param shape_title Legend title string. Use "" for no title.
#' @param size_title Legend title string. Use "" for no title.
#' @param caption Caption title string.
#' @param titles A function to format unspecified titles. Defaults to snakecase::to_sentence_case.
#' @param stat A ggplot2 character string stat.
#' @param position Position adjustment function (e.g. ggplot2::position_identity()).
#' @param coord A coordinate function from ggplot2 (e.g. ggplot2::coord_cartesian(clip = "off")).
#' @param theme A ggplot2 theme.
#'
#' @return A ggplot object.
#' @export
#' @examples
#' library(dplyr)
#' library(palmerpenguins)
#'
#'
gg_point_DEV <- function(
    data = NULL,
    x = NULL,
    y = NULL,
    col = NULL,
    facet = NULL,
    facet2 = NULL,
    group = NULL,
    xmin = NULL,
    xmax = NULL,
    xend = NULL,
    ymin = NULL,
    ymax = NULL,
    yend = NULL,
    z = NULL,
    sample = NULL,
    label = NULL,
    subgroup = NULL,
    mapping = NULL,
    pal = NULL,
    pal_na = "#bebebe",
    alpha = 1,
    ...,
    title = NULL,
    subtitle = NULL,
    x_breaks = NULL,
    x_expand = NULL,
    x_gridlines = NULL,
    x_include = NULL,
    x_labels = NULL,
    x_limits = NULL,
    x_oob = scales::oob_keep,
    x_sec_axis = ggplot2::waiver(),
    x_title = NULL,
    x_trans = "identity",
    y_breaks = NULL,
    y_expand = NULL,
    y_gridlines = NULL,
    y_include = NULL,
    y_labels = NULL,
    y_limits = NULL,
    y_oob = scales::oob_keep,
    y_sec_axis = ggplot2::waiver(),
    y_title = NULL,
    y_trans = "identity",
    col_breaks = NULL,
    col_continuous = "gradient",
    col_include = NULL,
    col_expand = NULL,
    col_labels = NULL,
    col_legend_place = "right",
    col_legend_ncol = NULL,
    col_legend_nrow = NULL,
    col_legend_rev = FALSE,
    col_limits = NULL,
    col_oob = scales::oob_keep,
    col_rescale = scales::rescale(),
    col_title = NULL,
    col_trans = NULL,
    facet_labels = NULL,
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_scales = "fixed",
    facet_space = "fixed",
    facet_layout = NULL,
    facet_switch = NULL,
    linetype_title = NULL,
    shape_title = NULL,
    size_title = NULL,
    caption = NULL,
    titles = snakecase::to_sentence_case,
    stat = "identity",
    position = ggplot2::position_identity(),
    coord = ggplot2::coord_cartesian(clip = "off"),
    theme = NULL
    ) {

  gg_blanket(
    data = data,
    x = {{ x }},
    y = {{ y }},
    col = {{ col }},
    facet = {{ facet }},
    facet2 = {{ facet2 }},
    xmin = {{ xmin }},
    xmax = {{ xmax }},
    xend = {{ xend }},
    ymin = {{ ymin }},
    ymax = {{ ymax }},
    yend = {{ yend }},
    group = {{ group }},
    subgroup = {{ subgroup }},
    label = {{ label }},
    sample = {{ sample }},
    mapping = mapping,
    pal = pal,
    pal_na = pal_na,
    alpha = alpha,
    ...,
    title = title,
    subtitle = subtitle,
    x_breaks = x_breaks,
    x_expand = x_expand,
    x_gridlines = x_gridlines,
    x_include = x_include,
    x_labels = x_labels,
    x_limits = x_limits,
    x_oob = x_oob,
    x_sec_axis = x_sec_axis,
    x_title = x_title,
    x_trans = x_trans,
    y_breaks = y_breaks,
    y_expand = y_expand,
    y_gridlines = y_gridlines,
    y_include = y_include,
    y_labels = y_labels,
    y_limits = y_limits,
    y_oob = y_oob,
    y_sec_axis = y_sec_axis,
    y_title = y_title,
    y_trans = y_trans,
    col_breaks = col_breaks,
    col_continuous = col_continuous,
    col_include = col_include,
    col_labels = col_labels,
    col_legend_place = col_legend_place,
    col_legend_ncol = col_legend_ncol,
    col_legend_nrow = col_legend_nrow,
    col_legend_rev = col_legend_rev,
    col_limits = col_limits,
    col_oob = col_oob,
    col_rescale = col_rescale,
    col_title = col_title,
    col_trans = col_trans,
    facet_labels = facet_labels,
    facet_ncol = facet_ncol,
    facet_nrow = facet_nrow,
    facet_scales = facet_scales,
    facet_space = facet_space,
    facet_layout = facet_layout,
    facet_switch = facet_switch,
    linetype_title = linetype_title,
    shape_title = shape_title,
    size_title = size_title,
    caption = caption,
    titles = titles,
    geom = "point",
    stat = stat,
    position = position,
    coord = coord,
    theme = theme
  )
}


