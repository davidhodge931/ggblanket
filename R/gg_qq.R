#' @title Qq ggplot
#'
#' @description Create a qq ggplot with a wrapper around ggplot2::geom_qq.
#'
#' @param data A data frame or tibble.
#' @param stat A statistical transformation to use on the data. A ggproto Stat subclass object or character string.
#' @param position A position adjustment. A ggproto Position subclass object, or character string.
#' @param coord A coordinate function from ggplot2 (e.g. ggplot2::coord_cartesian(clip = "off")).
#' @param theme A ggplot2 theme.
#' @param x Unquoted x aesthetic variable.
#' @param y Unquoted y aesthetic variable.
#' @param col Unquoted col and fill aesthetic variable.
#' @param facet Unquoted facet aesthetic variable.
#' @param facet2 Unquoted second facet variable.
#' @param alpha Unquoted alpha aesthetic variable.
#' @param xmin Unquoted xmin aesthetic variable.
#' @param xmax Unquoted xmax aesthetic variable.
#' @param xend Unquoted xend aesthetic variable.
#' @param ymin Unquoted ymin aesthetic variable.
#' @param ymax Unquoted ymax aesthetic variable.
#' @param yend Unquoted yend aesthetic variable.
#' @param z Unquoted z aesthetic variable.
#' @param group Unquoted group aesthetic variable.
#' @param subgroup Unquoted subgroup aesthetic variable.
#' @param label Unquoted label aesthetic variable.
#' @param text Unquoted text aesthetic variable.
#' @param sample Unquoted sample aesthetic variable.
#' @param mapping Map additional aesthetics using the ggplot2::aes function (e.g. shape). Excludes colour, fill or alpha_pal.
#' @param title Title string.
#' @param subtitle Subtitle string.
#' @param x_breaks A scales::breaks_* function (e.g. scales::breaks_pretty()), or a vector of breaks.
#' @param x_expand Padding to the limits with the ggplot2::expansion function, or a vector of length 2 (e.g. c(0, 0)).
#' @param x_gridlines TRUE or FALSE for vertical x gridlines. NULL guesses based on the classes of the x and y.
#' @param x_expand_limits For a continuous x variable, any values that the limits should encompass (e.g. 0).
#' @param x_labels A function that takes the breaks as inputs (e.g. scales::label_comma()), or a vector of labels.
#' @param x_limits A vector of length 2 to determine the limits of the axis.
#' @param x_oob For a continuous x variable, a scales::oob_* function of how to handle values outside of limits (e.g. scales::oob_keep). Defaults to scales::oob_keep.
#' @param x_sec_axis A secondary axis using the ggplot2::sec_axis or ggplot2::dup_axis function.
#' @param x_title Axis title string. Use "" for no title.
#' @param x_transform For a numeric x variable, a transformation object (e.g. "log10", "sqrt" or "reverse").
#' @param y_breaks A scales::breaks_* function (e.g. scales::breaks_pretty()), or a vector of breaks.
#' @param y_expand Padding to the limits with the ggplot2::expansion function, or a vector of length 2 (e.g. c(0, 0)).
#' @param y_gridlines TRUE or FALSE of horizontal y gridlines. NULL guesses based on the classes of the x and y.
#' @param y_expand_limits For a continuous y variable, any values that the limits should encompass (e.g. 0).
#' @param y_labels A function that takes the breaks as inputs (e.g. scales::label_comma()), or a vector of labels.
#' @param y_limits A vector of length 2 to determine the limits of the axis.
#' @param y_oob For a continuous y variable, a scales::oob_* function of how to handle values outside of limits (e.g. scales::oob_keep). Defaults to scales::oob_keep.
#' @param y_sec_axis A secondary axis using the ggplot2::sec_axis or ggplot2::dup_axis function.
#' @param y_title Axis title string. Use "" for no title.
#' @param y_transform For a numeric y variable, a transformation object (e.g. "log10", "sqrt" or "reverse").
#' @param col_breaks A scales::breaks_* function (e.g. scales::breaks_pretty()), or a vector of breaks.
#' @param col_expand Padding to the limits with the ggplot2::expansion function, or a vector of length 2 (e.g. c(0, 0)).
#' @param col_expand_limits For a continuous variable, any values that the limits should encompass (e.g. 0).
#' @param col_labels A function that takes the breaks as inputs (e.g. scales::label_comma()), or a vector of labels.
#' @param col_legend_ncol The number of columns for the legend elements.
#' @param col_legend_nrow The number of rows for the legend elements.
#' @param col_legend_rev Reverse the elements of the legend. Defaults to FALSE.
#' @param col_limits A vector to determine the limits of the scale.
#' @param col_oob For a continuous variable, a scales::oob_* function of how to handle values outside of limits (e.g. scales::oob_keep). Defaults to scales::oob_keep.
#' @param col_pal colours to use. A character vector of hex codes (or names).
#' @param col_pal_na colour to use for NA values. A character vector of a hex code (or name).
#' @param col_rescale For a continuous variable, a scales::rescale function.
#' @param col_steps For a continuous variable, whether to colour in steps. Defaults to FALSE (i.e. a gradient).
#' @param col_title Legend title string. Use "" for no title.
#' @param col_transform For a numeric variable, a transformation object (e.g. "log10", "sqrt" or "reverse").
#' @param facet_axes Whether to add interior axes and ticks with "margins", "all", "all_x", or "all_y".
#' @param facet_axis_labels Whether to add interior axis labels with "margins", "all", "all_x", or "all_y".
#' @param facet_labels A function that takes the breaks as inputs (e.g. scales::label_comma()), or a named vector of labels (e.g. c("value" = "label", ...)).
#' @param facet_ncol The number of columns of facets. Only applies to a facet layout of "wrap".
#' @param facet_nrow The number of rows of facets. Only applies to a facet layout of "wrap".
#' @param facet_scales Whether facet scales should be "fixed" across facets, "free" in both directions, or free in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed".
#' @param facet_space Whether facet space should be "fixed" across facets, "free" to be proportional in both directions, or free to be proportional in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed". Only applies where the facet layout is "grid" and facet scales are not "fixed".
#' @param facet_layout Whether the layout is to be "wrap" or "grid". If NULL and a single facet (or facet2) argument is provided, then defaults to "wrap". If NULL and both facet and facet2 arguments are provided, defaults to "grid".
#' @param facet_switch Whether the facet layout is "grid", whether to switch the facet labels to the opposite side of the plot. Either "x", "y" or "both".
#' @param alpha_breaks A scales::breaks_* function (e.g. scales::breaks_pretty()), or a vector of breaks.
#' @param alpha_expand Padding to the limits with the ggplot2::expansion function, or a vector of length 2 (e.g. c(0, 0)).
#' @param alpha_expand_limits For a continuous variable, any values that the limits should encompass (e.g. 0).
#' @param alpha_labels A function that takes the breaks as inputs (e.g. scales::label_comma()), or a vector of labels.
#' @param alpha_legend_ncol The number of columns for the legend elements.
#' @param alpha_legend_nrow The number of rows for the legend elements.
#' @param alpha_legend_rev Reverse the elements of the legend. Defaults to FALSE.
#' @param alpha_limits A vector to determine the limits of the scale.
#' @param alpha_oob For a continuous variable, a scales::oob_* function of how to handle values outside of limits (e.g. scales::oob_keep). Defaults to scales::oob_keep.
#' @param alpha_pal Alpha values to use as a numeric vector. For a continuous variable, a range is only needed. 
#' @param lpha_pal_na Alpha value to use for the NA value. 
#' @param alpha_title Legend title string. Use "" for no title.
#' @param alpha_transform For a numeric variable, a transformation object (e.g. "log10", "sqrt" or "reverse").
#' @param caption Caption title string.
#' @param titles A function to format unspecified titles. Defaults to snakecase::to_sentence_case.
#' @param ... Other arguments passed to within a params list in the layer function.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#'
gg_qq <- function(
    data = NULL,
    stat = "qq",
    position = "identity",
    coord = ggplot2::coord_cartesian(clip = "off"),
    theme = NULL,
    x = NULL,
    y = NULL,
    col = NULL,
    facet = NULL,
    facet2 = NULL,
    alpha = NULL,
    xmin = NULL,
    xmax = NULL,
    xend = NULL,
    ymin = NULL,
    ymax = NULL,
    yend = NULL,
    z = NULL,
    group = NULL,
    subgroup = NULL,
    label = NULL,
    text = NULL,
    sample = NULL,
    mapping = NULL,
    title = NULL,
    subtitle = NULL,
    x_breaks = NULL,
    x_expand = NULL,
    x_gridlines = NULL,
    x_expand_limits = NULL,
    x_labels = NULL,
    x_limits = NULL,
    x_oob = scales::oob_keep,
    x_sec_axis = ggplot2::waiver(),
    x_title = NULL,
    x_transform = NULL,
    y_breaks = NULL,
    y_expand = NULL,
    y_gridlines = NULL,
    y_expand_limits = NULL,
    y_labels = NULL,
    y_limits = NULL,
    y_oob = scales::oob_keep,
    y_sec_axis = ggplot2::waiver(),
    y_title = NULL,
    y_transform = NULL,
    col_breaks = NULL,
    col_expand_limits = NULL,
    col_expand = ggplot2::waiver(),
    col_labels = NULL,
    col_legend_ncol = NULL,
    col_legend_nrow = NULL,
    col_legend_rev = FALSE,
    col_limits = NULL,
    col_oob = scales::oob_keep,
    col_pal = NULL,
    col_pal_na = "#bebebe",
    col_rescale = scales::rescale(),
    col_steps = FALSE,
    col_title = NULL,
    col_transform = NULL,
    facet_axes = "margins",
    facet_axis_labels = "all",
    facet_labels = NULL,
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_scales = "fixed",
    facet_space = "fixed",
    facet_layout = NULL,
    facet_switch = NULL,
    alpha_breaks = NULL,
    alpha_expand_limits = NULL,
    alpha_expand = ggplot2::waiver(),
    alpha_labels = NULL,
    alpha_legend_ncol = NULL,
    alpha_legend_nrow = NULL,
    alpha_legend_rev = FALSE,
    alpha_limits = NULL,
    alpha_oob = scales::oob_keep,
    alpha_pal = NULL,
    alpha_pal_na = NA,
    alpha_title = NULL,
    alpha_transform = NULL,
    caption = NULL,
    titles = snakecase::to_sentence_case,
    ...
) {

  gg_blanket(
    data = data,
    geom = "point",
    stat = stat,
    position = position,
    coord = coord,
    theme = theme,
    x = {{ x }},
    y = {{ y }},
    col = {{ col }},
    facet = {{ facet }},
    facet2 = {{ facet2 }},
    alpha = {{ alpha }},
    xmin = {{ xmin }},
    xmax = {{ xmax }},
    xend = {{ xend }},
    ymin = {{ ymin }},
    ymax = {{ ymax }},
    yend = {{ yend }},
    z = {{ z }},
    group = {{ group }},
    subgroup = {{ subgroup }},
    label = {{ label }},
    text = {{ text }},
    sample = {{ sample }},
    mapping = mapping,
    title = title,
    subtitle = subtitle,
    x_breaks = x_breaks,
    x_expand = x_expand,
    x_gridlines = x_gridlines,
    x_expand_limits = x_expand_limits,
    x_labels = x_labels,
    x_limits = x_limits,
    x_oob = x_oob,
    x_sec_axis = x_sec_axis,
    x_title = x_title,
    x_transform = x_transform,
    y_breaks = y_breaks,
    y_expand = y_expand,
    y_gridlines = y_gridlines,
    y_expand_limits = y_expand_limits,
    y_labels = y_labels,
    y_limits = y_limits,
    y_oob = y_oob,
    y_sec_axis = y_sec_axis,
    y_title = y_title,
    y_transform = y_transform,
    col_breaks = col_breaks,
    col_expand_limits = col_expand_limits,
    col_labels = col_labels,
    col_legend_ncol = col_legend_ncol,
    col_legend_nrow = col_legend_nrow,
    col_legend_rev = col_legend_rev,
    col_limits = col_limits,
    col_oob = col_oob,
    col_pal = col_pal,
    col_pal_na = col_pal_na,
    col_rescale = col_rescale,
    col_steps = col_steps,
    col_title = col_title,
    col_transform = col_transform,
    facet_axes = facet_axes,
    facet_axis_labels = facet_axis_labels,
    facet_labels = facet_labels,
    facet_ncol = facet_ncol,
    facet_nrow = facet_nrow,
    facet_scales = facet_scales,
    facet_space = facet_space,
    facet_layout = facet_layout,
    facet_switch = facet_switch,
    alpha_breaks = alpha_breaks,
    alpha_expand_limits = alpha_expand_limits,
    alpha_expand = alpha_expand,
    alpha_labels = alpha_labels,
    alpha_legend_ncol = alpha_legend_ncol,
    alpha_legend_nrow = alpha_legend_nrow,
    alpha_legend_rev = alpha_legend_rev,
    alpha_limits = alpha_limits,
    alpha_oob = alpha_oob,
    alpha_pal = alpha_pal,
    alpha_pal_na = alpha_pal_na,
    alpha_title = alpha_title,
    alpha_transform = alpha_transform,
    caption = caption,
    titles = titles,
    ...
  )
}

