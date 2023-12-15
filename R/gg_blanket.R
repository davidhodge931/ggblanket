#' @title Blanket ggplot
#'
#' @description Create a blanket ggplot with a wrapper around ggplot2::layer().
#'
#' @param data A data frame or tibble.
#' @param geom A geometric object to display the data. A ggproto Geom subclass object or character string.
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
#' @param alpha_pal alphaours to use. A character vector of hex codes (or names).
#' @param alpha_pal_na alphaour to use for NA values. A character vector of a hex code (or name).
#' @param alpha_title Legend title string. Use "" for no title.
#' @param alpha_transform For a numeric variable, a transformation object (e.g. "log10", "sqrt" or "reverse").
#' @param caption Caption title string.
#' @param titles A function to format unspecified titles. Defaults to snakecase::to_sentence_case.
#' @param ... Other arguments passed to within a params list in the layer function.
#'
#' @return A ggplot object.
#' @export
#' @examples
#'
#'
gg_blanket <- function(
    data = NULL,
    geom = "blank",
    stat = "identity",
    position = "identity",
    coord = NULL,
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

  ##############################################################################
  #quote
  ##############################################################################

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  col <- rlang::enquo(col)
  facet <- rlang::enquo(facet)
  facet2 <- rlang::enquo(facet2)
  xmin <- rlang::enquo(xmin)
  xmax <- rlang::enquo(xmax)
  xend <- rlang::enquo(xend)
  ymin <- rlang::enquo(ymin)
  ymax <- rlang::enquo(ymax)
  yend <- rlang::enquo(yend)
  z <- rlang::enquo(z)
  group <- rlang::enquo(group)
  subgroup <- rlang::enquo(subgroup)
  label <- rlang::enquo(label)
  text <- rlang::enquo(text)
  sample <- rlang::enquo(sample)
  alpha <- rlang::enquo(alpha)

  ##############################################################################
  #stop, warn and inform
  ##############################################################################

  if (!rlang::is_null(mapping)) {
    if (any(names(unlist(mapping)) %in% c("facet", "facet2"))) {
      rlang::abort("mapping argument does not support facet or facet2")
    }
  }

  ##############################################################################
  #get default theme, if global theme not set
  ##############################################################################

  if (rlang::is_null(theme)) {
    if (identical(ggplot2::theme_get(), ggplot2::theme_grey())) {
      theme <- lightmode_right2()
    }
  }

  ##############################################################################
  #determine classes
  ##############################################################################

  x_null <- inherits(
    rlang::eval_tidy(x, data),
    what = c("NULL")
  ) &
    inherits(
      rlang::eval_tidy(xmin, data),
      what = c("NULL")
    ) &
    inherits(
      rlang::eval_tidy(xmax, data),
      what = c("NULL")
    ) &
    inherits(
      rlang::eval_tidy(xend, data),
      what = c("NULL")
    )

  x_numeric <- inherits(
    rlang::eval_tidy(x, data),
    what = c("numeric", "double", "integer")
  ) |
    inherits(
      rlang::eval_tidy(xmin, data),
      what = c("numeric", "double", "integer")
    ) |
    inherits(
      rlang::eval_tidy(xmax, data),
      what = c("numeric", "double", "integer")
    ) |
    inherits(
      rlang::eval_tidy(xend, data),
      what = c("numeric", "double", "integer")
    )

  x_date <- inherits(
    rlang::eval_tidy(x, data),
    what = c("Date")
  ) |
    inherits(
      rlang::eval_tidy(xmin, data),
      what = c("Date")
    ) |
    inherits(
      rlang::eval_tidy(xmax, data),
      what = c("Date")
    ) |
    inherits(
      rlang::eval_tidy(xend, data),
      what = c("Date")
    )

  x_posixct <- inherits(
    rlang::eval_tidy(x, data),
    what = c("POSIXct")
  ) |
    inherits(
      rlang::eval_tidy(xmin, data),
      what = c("POSIXct")
    ) |
    inherits(
      rlang::eval_tidy(xmax, data),
      what = c("POSIXct")
    ) |
    inherits(
      rlang::eval_tidy(xend, data),
      what = c("POSIXct")
    )

  x_hms <- inherits(
    rlang::eval_tidy(x, data),
    what = c("hms")
  ) |
    inherits(
      rlang::eval_tidy(xmin, data),
      what = c("hms")
    ) |
    inherits(
      rlang::eval_tidy(xmax, data),
      what = c("hms")
    ) |
    inherits(
      rlang::eval_tidy(xend, data),
      what = c("hms")
    )

  x_continuous <- x_null | x_numeric | x_date | x_posixct | x_hms

  y_null <- inherits(
    rlang::eval_tidy(y, data),
    what = c("NULL")
  ) &
    inherits(
      rlang::eval_tidy(ymin, data),
      what = c("NULL")
    ) &
    inherits(
      rlang::eval_tidy(ymax, data),
      what = c("NULL")
    ) &
    inherits(
      rlang::eval_tidy(yend, data),
      what = c("NULL")
    )

  y_numeric <- inherits(
    rlang::eval_tidy(y, data),
    what = c("numeric", "double", "integer")
  ) |
    inherits(
      rlang::eval_tidy(ymin, data),
      what = c("numeric", "double", "integer")
    ) |
    inherits(
      rlang::eval_tidy(ymax, data),
      what = c("numeric", "double", "integer")
    ) |
    inherits(
      rlang::eval_tidy(yend, data),
      what = c("numeric", "double", "integer")
    )

  y_date <- inherits(
    rlang::eval_tidy(y, data),
    what = c("Date")
  ) |
    inherits(
      rlang::eval_tidy(ymin, data),
      what = c("Date")
    ) |
    inherits(
      rlang::eval_tidy(ymax, data),
      what = c("Date")
    ) |
    inherits(
      rlang::eval_tidy(yend, data),
      what = c("Date")
    )

  y_posixct <- inherits(
    rlang::eval_tidy(y, data),
    what = c("POSIXct")
  ) |
    inherits(
      rlang::eval_tidy(ymin, data),
      what = c("POSIXct")
    ) |
    inherits(
      rlang::eval_tidy(ymax, data),
      what = c("POSIXct")
    ) |
    inherits(
      rlang::eval_tidy(yend, data),
      what = c("POSIXct")
    )

  y_hms <- inherits(
    rlang::eval_tidy(y, data),
    what = c("hms")
  ) |
    inherits(
      rlang::eval_tidy(ymin, data),
      what = c("hms")
    ) |
    inherits(
      rlang::eval_tidy(ymax, data),
      what = c("hms")
    ) |
    inherits(
      rlang::eval_tidy(yend, data),
      what = c("hms")
    )

  y_continuous <- y_null | y_numeric | y_date | y_posixct | y_hms

  ##############################################################################
  #determine if flipped
  ##############################################################################

  if (x_null & !y_null) flipped <- TRUE
  else if ((x_numeric | x_date | x_posixct | x_hms) &
           !(y_null | y_numeric | y_date | y_posixct | y_hms)) {
    flipped <- TRUE
  }
  else if (x_numeric & (y_date | y_posixct | y_hms)) flipped <- TRUE
  else flipped <- FALSE

  ##############################################################################
  #get geom, stat, transform & position strings
  ##############################################################################

  if (ggplot2::is.ggproto(geom)) {
    geom_name <- stringr::str_to_lower(stringr::str_remove(class(geom)[1], "Geom"))
  }
  else if (is.character(geom)) geom_name <- geom

  if (ggplot2::is.ggproto(stat)) {
    stat_name <- stringr::str_to_lower(stringr::str_remove(class(stat)[1], "Stat"))
  }
  else if (is.character(stat)) stat_name <- stat

  if (ggplot2::is.ggproto(position)) {
    position_name <- stringr::str_to_lower(stringr::str_remove(class(position)[1], "Position"))
  }
  else if (is.character(position)) position_name <- position

  ##############################################################################
  #get positional transform defaults - and strings
  ##############################################################################

  #get x_transform if NULL
  if (rlang::is_null(x_transform)) {
    if (x_hms) x_transform <- scales::transform_hms()
    else if (x_posixct) x_transform <- scales::transform_time()
    else if (x_date) x_transform <- scales::transform_date()
    else x_transform <- scales::transform_identity()
  }

  #make a tidy name to deal with composed transforms
  if (is.character(x_transform)) x_transform_name <- x_transform
  else if (inherits(x_transform, what = "transform")) {
    x_transform_name <- x_transform$name %>%
      stringr::str_remove("composition") %>%
      stringr::str_remove("\\(") %>%
      stringr::str_remove("\\)") %>%
      stringr::str_split(",") %>%
      unlist()
  }

  #get y_transform if NULL
  if (rlang::is_null(y_transform)) {
    if (y_hms) y_transform <- scales::transform_hms()
    else if (y_posixct) y_transform <- scales::transform_time()
    else if (y_date) y_transform <- scales::transform_date()
    else y_transform <- scales::transform_identity()
  }

  #make a tidy name to deal with composed transforms
  if (is.character(y_transform)) y_transform_name <- y_transform
  else if (inherits(y_transform, what = "transform")) {
    y_transform_name <- y_transform$name %>%
      stringr::str_remove("composition") %>%
      stringr::str_remove("\\(") %>%
      stringr::str_remove("\\)") %>%
      stringr::str_split(",") %>%
      unlist()
  }

  ##############################################################################
  #process the data
  ##############################################################################

  data <- data %>%
    #ungroup the data
    dplyr::ungroup() %>%
    #make infinite values NA
    dplyr::mutate(dplyr::across(
      c(!!x, !!xmin, !!xmax, !!xend,
        !!y, !!ymin, !!ymax, !!yend,
        !!col, !!alpha, !!facet, !!facet2,
        !!group, !!subgroup, !!label, !!sample),
      na_if_inf)) %>%
    #convert logicals to factors
    dplyr::mutate(dplyr::across(c(!!x, !!xmin, !!xmax, !!xend,
                                  !!y, !!ymin, !!ymax, !!yend,
                                  !!col, !!alpha, !!facet, !!facet2) &
                                  tidyselect::where(is.logical), function(x)
                                    factor(x, levels = c(TRUE, FALSE)))) %>%
    #convert characters to factors
    dplyr::mutate(dplyr::across(c(!!x, !!xmin, !!xmax, !!xend,
                                  !!y, !!ymin, !!ymax, !!yend,
                                  !!col, !!alpha, !!facet, !!facet2) &
                                  tidyselect::where(is.character), function(x)
                                    factor(x))) %>%
    #reverse y*, so that reads top low-levels to bottom high-levels
    dplyr::mutate(dplyr::across(c(!!y, !!ymin, !!ymax, !!yend) &
                                  tidyselect::where(is.factor),
                                function(x) forcats::fct_rev(x)))

  #if flipped, order col correctly
  if ((!identical(rlang::eval_tidy(y, data), rlang::eval_tidy(col, data))) &
      flipped) {
    data <- data %>%
      dplyr::mutate(dplyr::across(!!col & tidyselect::where(is.factor),
                                  function(x) forcats::fct_rev(x)))
  }

  ##############################################################################
  #yet more defaults
  ##############################################################################

  if (!rlang::quo_is_null(facet)) {
    facet_n <- data %>%
      dplyr::pull(!!facet) %>%
      levels() %>%
      length()

    if (any(is.na(data %>% dplyr::pull(!!facet)))) facet_n <- facet_n + 1
  }

  if (!rlang::quo_is_null(facet2)) {
    facet2_n <- data %>%
      dplyr::pull(!!facet2) %>%
      levels() %>%
      length()

    if (any(is.na(data %>% dplyr::pull(!!facet2)))) facet2_n <- facet2_n + 1
  }

  x_drop <- ifelse(facet_scales %in% c("free_x", "free"), TRUE, FALSE)
  y_drop <- ifelse(facet_scales %in% c("free_y", "free"), TRUE, FALSE)

  ##############################################################################
  # add ggplot() with aesthetics
  ##############################################################################

  if (rlang::quo_is_null(alpha)) {
    if (rlang::quo_is_null(col)) {
      if (!x_null & !y_null) {
        plot <- data %>%
          ggplot2::ggplot(mapping = ggplot2::aes(
            x = !!x,
            y = !!y,
            xmin = !!xmin,
            xmax = !!xmax,
            xend = !!xend,
            ymin = !!ymin,
            ymax = !!ymax,
            yend = !!yend,
            z = !!z,
            group = !!group,
            subgroup = !!subgroup,
            sample = !!sample,
            label = !!label,
            # geometry = geometry,
            !!!mapping
          ))
      }
      else if (!x_null & y_null) {
        plot <- data %>%
          ggplot2::ggplot(mapping = ggplot2::aes(
            x = !!x,
            xmin = !!xmin,
            xmax = !!xmax,
            xend = !!xend,
            ymin = !!ymin,
            ymax = !!ymax,
            yend = !!yend,
            z = !!z,
            group = !!group,
            subgroup = !!subgroup,
            sample = !!sample,
            label = !!label,
            # geometry = geometry,
            !!!mapping
          ))
      }
      else if (x_null & !y_null) {
        plot <- data %>%
          ggplot2::ggplot(mapping = ggplot2::aes(
            y = !!y,
            xmin = !!xmin,
            xmax = !!xmax,
            xend = !!xend,
            ymin = !!ymin,
            ymax = !!ymax,
            yend = !!yend,
            z = !!z,
            group = !!group,
            subgroup = !!subgroup,
            sample = !!sample,
            label = !!label,
            # geometry = geometry,
            !!!mapping
          ))
      }
      else if (x_null & y_null) {
        plot <- data %>%
          ggplot2::ggplot(mapping = ggplot2::aes(
            xmin = !!xmin,
            xmax = !!xmax,
            xend = !!xend,
            ymin = !!ymin,
            ymax = !!ymax,
            yend = !!yend,
            z = !!z,
            group = !!group,
            subgroup = !!subgroup,
            sample = !!sample,
            label = !!label,
            # geometry = geometry,
            !!!mapping
          ))
      }
    }
    else {
      if (!x_null & !y_null) {
        plot <- data %>%
          ggplot2::ggplot(mapping = ggplot2::aes(
            x = !!x,
            y = !!y,
            col = !!col,
            fill = !!col,
            xmin = !!xmin,
            xmax = !!xmax,
            xend = !!xend,
            ymin = !!ymin,
            ymax = !!ymax,
            yend = !!yend,
            z = !!z,
            group = !!group,
            subgroup = !!subgroup,
            sample = !!sample,
            label = !!label,
            # geometry = geometry,
            !!!mapping
          ))
      }
      else if (!x_null & y_null) {
        plot <- data %>%
          ggplot2::ggplot(mapping = ggplot2::aes(
            x = !!x,
            col = !!col,
            fill = !!col,
            xmin = !!xmin,
            xmax = !!xmax,
            xend = !!xend,
            ymin = !!ymin,
            ymax = !!ymax,
            yend = !!yend,
            z = !!z,
            group = !!group,
            subgroup = !!subgroup,
            sample = !!sample,
            label = !!label,
            # geometry = geometry,
            !!!mapping
          ))
      }
      else if (x_null & !y_null) {
        plot <- data %>%
          ggplot2::ggplot(mapping = ggplot2::aes(
            y = !!y,
            col = !!col,
            fill = !!col,
            xmin = !!xmin,
            xmax = !!xmax,
            xend = !!xend,
            ymin = !!ymin,
            ymax = !!ymax,
            yend = !!yend,
            z = !!z,
            group = !!group,
            subgroup = !!subgroup,
            sample = !!sample,
            label = !!label,
            # geometry = geometry,
            !!!mapping
          ))
      }
      else if (x_null & y_null) {
        plot <- data %>%
          ggplot2::ggplot(mapping = ggplot2::aes(
            col = !!col,
            fill = !!col,
            xmin = !!xmin,
            xmax = !!xmax,
            xend = !!xend,
            ymin = !!ymin,
            ymax = !!ymax,
            yend = !!yend,
            z = !!z,
            group = !!group,
            subgroup = !!subgroup,
            sample = !!sample,
            label = !!label,
            # geometry = geometry,
            !!!mapping
          ))
      }
    }
  }
  else {
    if (rlang::quo_is_null(col)) {
      if (!x_null & !y_null) {
        plot <- data %>%
          ggplot2::ggplot(mapping = ggplot2::aes(
            x = !!x,
            y = !!y,
            xmin = !!xmin,
            xmax = !!xmax,
            xend = !!xend,
            ymin = !!ymin,
            ymax = !!ymax,
            yend = !!yend,
            z = !!z,
            group = !!group,
            subgroup = !!subgroup,
            sample = !!sample,
            label = !!label,
            # geometry = geometry,
            alpha = !!alpha, !!!mapping
          ))
      }
      else if (!x_null & y_null) {
        plot <- data %>%
          ggplot2::ggplot(mapping = ggplot2::aes(
            x = !!x,
            xmin = !!xmin,
            xmax = !!xmax,
            xend = !!xend,
            ymin = !!ymin,
            ymax = !!ymax,
            yend = !!yend,
            z = !!z,
            group = !!group,
            subgroup = !!subgroup,
            sample = !!sample,
            label = !!label,
            # geometry = geometry,
            alpha = !!alpha, !!!mapping
          ))
      }
      else if (x_null & !y_null) {
        plot <- data %>%
          ggplot2::ggplot(mapping = ggplot2::aes(
            y = !!y,
            xmin = !!xmin,
            xmax = !!xmax,
            xend = !!xend,
            ymin = !!ymin,
            ymax = !!ymax,
            yend = !!yend,
            z = !!z,
            group = !!group,
            subgroup = !!subgroup,
            sample = !!sample,
            label = !!label,
            # geometry = geometry,
            alpha = !!alpha, !!!mapping
          ))
      }
      else if (x_null & y_null) {
        plot <- data %>%
          ggplot2::ggplot(mapping = ggplot2::aes(
            xmin = !!xmin,
            xmax = !!xmax,
            xend = !!xend,
            ymin = !!ymin,
            ymax = !!ymax,
            yend = !!yend,
            z = !!z,
            group = !!group,
            subgroup = !!subgroup,
            sample = !!sample,
            label = !!label,
            # geometry = geometry,
            alpha = !!alpha, !!!mapping
          ))
      }
    }
    else {
      if (!x_null & !y_null) {
        plot <- data %>%
          ggplot2::ggplot(mapping = ggplot2::aes(
            x = !!x,
            y = !!y,
            col = !!col,
            fill = !!col,
            xmin = !!xmin,
            xmax = !!xmax,
            xend = !!xend,
            ymin = !!ymin,
            ymax = !!ymax,
            yend = !!yend,
            z = !!z,
            group = !!group,
            subgroup = !!subgroup,
            sample = !!sample,
            label = !!label,
            # geometry = geometry,
            alpha = !!alpha, !!!mapping
          ))
      }
      else if (!x_null & y_null) {
        plot <- data %>%
          ggplot2::ggplot(mapping = ggplot2::aes(
            x = !!x,
            col = !!col,
            fill = !!col,
            xmin = !!xmin,
            xmax = !!xmax,
            xend = !!xend,
            ymin = !!ymin,
            ymax = !!ymax,
            yend = !!yend,
            z = !!z,
            group = !!group,
            subgroup = !!subgroup,
            sample = !!sample,
            label = !!label,
            # geometry = geometry,
            alpha = !!alpha, !!!mapping
          ))
      }
      else if (x_null & !y_null) {
        plot <- data %>%
          ggplot2::ggplot(mapping = ggplot2::aes(
            y = !!y,
            col = !!col,
            fill = !!col,
            xmin = !!xmin,
            xmax = !!xmax,
            xend = !!xend,
            ymin = !!ymin,
            ymax = !!ymax,
            yend = !!yend,
            z = !!z,
            group = !!group,
            subgroup = !!subgroup,
            sample = !!sample,
            label = !!label,
            # geometry = geometry,
            alpha = !!alpha, !!!mapping
          ))
      }
      else if (x_null & y_null) {
        plot <- data %>%
          ggplot2::ggplot(mapping = ggplot2::aes(
            col = !!col,
            fill = !!col,
            xmin = !!xmin,
            xmax = !!xmax,
            xend = !!xend,
            ymin = !!ymin,
            ymax = !!ymax,
            yend = !!yend,
            z = !!z,
            group = !!group,
            subgroup = !!subgroup,
            sample = !!sample,
            label = !!label,
            # geometry = geometry,
            alpha = !!alpha, !!!mapping
          ))
      }
    }
  }

  ##############################################################################
  # add faceting
  ##############################################################################

  if (rlang::is_null(facet_layout)) {
    if (!rlang::quo_is_null(facet) & rlang::quo_is_null(facet2)) facet_layout <- "wrap"
    else if (!rlang::quo_is_null(facet2) & rlang::quo_is_null(facet)) facet_layout <- "wrap"
    else if (!rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) facet_layout <- "grid"
    else if (rlang::quo_is_null(facet) & rlang::quo_is_null(facet2)) facet_layout <- "grid"
    else facet_layout <- "null"
  }

  if (facet_layout == "wrap") {
    if (!rlang::quo_is_null(facet) & rlang::quo_is_null(facet2)) {
      plot <- plot +
        ggplot2::facet_wrap(
          facets = ggplot2::vars(!!facet),
          scales = facet_scales,
          drop = FALSE,
          axes = facet_axes,
          axis.labels = facet_axis_labels,
          nrow = facet_nrow,
          ncol = facet_ncol,
          labeller = ggplot2::as_labeller(facet_labels)
        )
    }
    else if (rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) {
      plot <- plot +
        ggplot2::facet_wrap(
          facets = ggplot2::vars(!!facet2),
          scales = facet_scales,
          drop = FALSE,
          axes = facet_axes,
          axis.labels = facet_axis_labels,
          nrow = facet_nrow,
          ncol = facet_ncol,
          labeller = ggplot2::as_labeller(facet_labels)
        )
    }
    else if (!rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) {
      plot <- plot +
        ggplot2::facet_wrap(
          facets = ggplot2::vars(!!facet, !!facet2),
          scales = facet_scales,
          drop = FALSE,
          axes = facet_axes,
          axis.labels = facet_axis_labels,
          nrow = facet_nrow,
          ncol = facet_ncol,
          labeller = ggplot2::as_labeller(facet_labels)
        )
    }
  }
  else if (facet_layout == "grid") {
    if (!rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) {
      plot <- plot +
        ggplot2::facet_grid(switch = facet_switch,
                            rows = ggplot2::vars(!!facet2),
                            cols = ggplot2::vars(!!facet),
                            scales = facet_scales,
                            space = facet_space,
                            drop = FALSE,
                            axes = facet_axes,
                            axis.labels = facet_axis_labels,
                            labeller = ggplot2::as_labeller(facet_labels)
        )
    }
    else if (!rlang::quo_is_null(facet) & rlang::quo_is_null(facet2)) {
      plot <- plot +
        ggplot2::facet_grid(switch = facet_switch,
                            cols = ggplot2::vars(!!facet),
                            scales = facet_scales,
                            space = facet_space,
                            drop = FALSE,
                            axes = facet_axes,
                            axis.labels = facet_axis_labels,
                            labeller = ggplot2::as_labeller(facet_labels)
        )
    }
    else if (rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) {
      plot <- plot +
        ggplot2::facet_grid(switch = facet_switch,
                            rows = ggplot2::vars(!!facet2),
                            scales = facet_scales,
                            space = facet_space,
                            drop = FALSE,
                            axes = facet_axes,
                            axis.labels = facet_axis_labels,
                            labeller = ggplot2::as_labeller(facet_labels)
        )
    }
  }

  ##############################################################################
  # Add positional scales pre getting plot data
  ##############################################################################

  if (!stringr::str_detect(stat_name, "sf")) {

    #limits reversed to deal with when trans is reverse
    if (x_numeric) {
      if (any(x_transform_name %in% "reverse") & !rlang::is_null(x_limits)) {
        plot <- plot +
          ggplot2::scale_x_continuous(limits = x_limits[c(2, 1)], oob = x_oob)
      }
      else {
        plot <- plot +
          ggplot2::scale_x_continuous(limits = x_limits, oob = x_oob)
      }
    }
    else if (x_date) {
      if (any(x_transform_name %in% "reverse") & !rlang::is_null(x_limits)) {
        plot <- plot +
          ggplot2::scale_x_date(limits = x_limits[c(2, 1)], oob = x_oob)
      }
      else {
        plot <- plot +
          ggplot2::scale_x_date(limits = x_limits, oob = x_oob)
      }
    }
    else if (x_posixct) {
      if (any(x_transform_name %in% "reverse") & !rlang::is_null(x_limits)) {
        plot <- plot +
          ggplot2::scale_x_datetime(limits = x_limits[c(2, 1)], oob = x_oob)
      }
      else {
        plot <- plot +
          ggplot2::scale_x_datetime(limits = x_limits, oob = x_oob)
      }
    }
    else if (x_hms) {
      if (any(x_transform_name %in% "reverse") & !rlang::is_null(x_limits)) {
        plot <- plot +
          ggplot2::scale_x_continuous(limits = x_limits[c(2, 1)], oob = x_oob, trans ="hms")
      }
      else {
        plot <- plot +
          ggplot2::scale_x_continuous(limits = x_limits, oob = x_oob, trans ="hms")
      }
    }
    else if (!x_continuous) {
      if (!rlang::is_null(x_limits)) {
        plot <- plot +
          ggplot2::scale_x_discrete(limits = x_limits, drop = x_drop)
      }
      else {
        plot <- plot +
          ggplot2::scale_x_discrete(drop = x_drop)
      }
    }

    if (y_numeric) {
      if (any(y_transform_name %in% "reverse") & !rlang::is_null(y_limits)) {
        plot <- plot +
          ggplot2::scale_y_continuous(limits = y_limits[c(2, 1)], oob = y_oob)
      }
      else {
        plot <- plot +
          ggplot2::scale_y_continuous(limits = y_limits, oob = y_oob)
      }
    }
    else if (y_date) {
      if (any(y_transform_name %in% "reverse") & !rlang::is_null(y_limits)) {
        plot <- plot +
          ggplot2::scale_y_date(limits = y_limits[c(2, 1)], oob = y_oob)
      }
      else {
        plot <- plot +
          ggplot2::scale_y_date(limits = y_limits, oob = y_oob)
      }
    }
    else if (y_posixct) {
      if (any(y_transform_name %in% "reverse") & !rlang::is_null(y_limits)) {
        plot <- plot +
          ggplot2::scale_y_datetime(limits = y_limits[c(2, 1)], oob = y_oob)
      }
      else {
        plot <- plot +
          ggplot2::scale_y_datetime(limits = y_limits, oob = y_oob)
      }
    }
    else if (y_hms) {
      if (any(y_transform_name %in% "reverse") & !rlang::is_null(y_limits)) {
        plot <- plot +
          ggplot2::scale_y_continuous(limits = y_limits[c(2, 1)], oob = y_oob, trans ="hms")
      }
      else {
        plot <- plot +
          ggplot2::scale_y_continuous(limits = y_limits, oob = y_oob, trans ="hms")
      }
    }
    else if (!y_continuous) {
      if (!rlang::is_null(y_limits)) {
        plot <- plot +
          ggplot2::scale_y_discrete(limits = y_limits, drop = y_drop)
      }
      else {
        plot <- plot +
          ggplot2::scale_y_discrete(drop = y_drop)
      }
    }
  }

  ##############################################################################
  # Add layer
  ##############################################################################

  if (geom_name == "blank") show_legend <- FALSE
  else show_legend <- TRUE

  if (stat_name %in% c("density_2d", "density_2d_filled")) contour <- TRUE
  else contour <- TRUE

  if (stringr::str_detect(stat_name, "sf")) {
    if (rlang::is_null(coord)) coord <- ggplot2::coord_sf(clip = "off")

    plot1 <- plot +
      ggplot2::layer_sf(
        geom = geom,
        stat = stat,
        position = position,
        params = list(contour = contour, ...),
        show.legend = show_legend,
      ) +
      coord +
      theme
  }
  else {
    if (rlang::is_null(coord)) coord <- ggplot2::coord_cartesian(clip = "off")

    plot1 <- plot +
      ggplot2::layer(
        geom = geom,
        stat = stat,
        position = position,
        params = list(contour = contour, ...),
        show.legend = show_legend,
      ) +
      coord +
      theme
  }


  ##############################################################################
  # Get plot build and data
  ##############################################################################

  plot_build <- ggplot2::ggplot_build(plot1)
  plot_data <- plot_build$data[[1]]

  ##############################################################################
  # Detect whether the plot has a col or alpha scale
  ##############################################################################

  #sf seems to document scales differently, and this fixes
  if (stringr::str_detect(stat_name, "sf")) {
    if (class(rlang::eval_tidy(col, data)) %in%
        c("numeric", "double", "integer","Date", "POSIXct","hms")) {
      col_continuous <- TRUE
    }
    else if (class(rlang::eval_tidy(col, data)) %in%
             c("character", "logical", "factor")) {
      col_continuous <- FALSE
    }
    else col_continuous <- NA

    if (class(rlang::eval_tidy(alpha, data)) %in%
        c("numeric", "double", "integer","Date", "POSIXct","hms")) {
      alpha_continuous <- TRUE
    }
    else if (class(rlang::eval_tidy(alpha, data)) %in%
             c("character", "logical", "factor")) {
      alpha_continuous <- FALSE
    }
    else alpha_continuous <- NA
  }
  #support where col is null, but there is a colour scale
  else {
    scales <- purrr::map_chr(plot_build$plot$scales$scales, \(x) {
      ifelse(rlang::is_null(rlang::call_name(x[["call"]])), NA,
             rlang::call_name(x[["call"]]))
    })

    if (any(scales %in% continuous_scales_col)) col_continuous <- TRUE
    else if (any(scales %in% discrete_scales_col)) col_continuous <- FALSE
    else col_continuous <- NA

    if (any(scales %in% continuous_scales_alpha)) alpha_continuous <- TRUE
    else if (any(scales %in% discrete_scales_alpha)) alpha_continuous <- FALSE
    else alpha_continuous <- NA
  }

  ##############################################################################
  # Remake the plot where there is either no col or no alpha scale identified
  ##############################################################################

  #get params for when no col or alpha aesthetic
  if ((is.na(col_continuous)) & (is.na(alpha_continuous))) {
    if (rlang::is_null(col_pal)) col_pal1 <- pal_none()
    else col_pal1 <- col_pal[1]

    if (rlang::is_null(alpha_pal)) {
      if (geom_name == "label") alpha_pal1 <- 0.1
      else alpha_pal1 <- 0.9
    }
    else alpha_pal1 <- alpha_pal[1]

    params_list <- list(contour = contour, colour = col_pal1, fill = col_pal1, alpha = alpha_pal1, ...)
  }
  else if (is.na(col_continuous)) {
    if (rlang::is_null(col_pal)) col_pal1 <- pal_none()
    else col_pal1 <- col_pal[1]

    params_list <- list(contour = contour, colour = col_pal1, fill = col_pal1, ...)
  }
  else if (is.na(alpha_continuous)) {
    if (rlang::is_null(alpha_pal)) {
      if (geom_name == "label") alpha_pal1 <- 0.1
      else alpha_pal1 <- 0.9
    }
    else alpha_pal1 <- alpha_pal[1]

    params_list <- list(contour = contour, alpha = alpha_pal1, ...)
  }

  #remake plot where either no col or alpha aesthetic
  if (is.na(col_continuous) | is.na(alpha_continuous)) {
    if (stringr::str_detect(stat_name, "sf")) {
      plot <- plot +
        ggplot2::layer_sf(
          geom = geom,
          stat = stat,
          position = position,
          params = params_list,
          show.legend = show_legend,
        ) +
        coord +
        theme
    }
    else {
      plot <- plot +
        ggplot2::layer(
          geom = geom,
          stat = stat,
          position = position,
          params = params_list,
          show.legend = show_legend,
        ) +
        coord +
        theme
    }
  }
  #revert back to the original plot where there is a col or alpha aesthetic
  else {
    plot <- plot1
  }

  ##############################################################################
  # Make colour scale where there is a colour scale identified
  ##############################################################################

  if (!is.na(col_continuous)) {
    if (col_continuous) {
      if (rlang::is_null(col_pal)) {
        col_pal <- pal_continuous()
      }

      #get col_transform if NULL
      if (rlang::is_null(col_transform)) {
        if (inherits(rlang::eval_tidy(col, data), what = "hms")) col_transform <- scales::transform_hms()
        else if (inherits(rlang::eval_tidy(col, data), what = "POSIXct")) col_transform <- scales::transform_time()
        else if (inherits(rlang::eval_tidy(col, data), what = "Date")) col_transform <- scales::transform_date()
        else col_transform <- scales::transform_identity()
      }

      #make a tidy name to deal with composed transforms
      if (is.character(col_transform)) col_transform_name <- col_transform
      else if (inherits(col_transform, what = "transform")) {
        col_transform_name <- col_transform$name %>%
          stringr::str_remove("composition") %>%
          stringr::str_remove("\\(") %>%
          stringr::str_remove("\\)") %>%
          stringr::str_split(",") %>%
          unlist()
      }

      if (rlang::is_null(col_breaks)) {
        if (!any(col_transform_name %in% c("identity", "reverse"))) col_breaks <- ggplot2::waiver()
        else col_breaks <- scales::breaks_pretty(n = 5)
      }

      if (rlang::is_null(col_labels)) {
        if (any(col_transform_name %in% c("hms"))) col_labels <- scales::label_time()
        else if (any(col_transform_name %in% c("date", "time"))) col_labels <- scales::label_date_short()
        else if (!any(col_transform_name %in% c("identity", "reverse"))) col_labels <- ggplot2::waiver()
        else col_labels <- scales::label_comma(drop0trailing = TRUE)
      }

      if (!col_steps) {
        plot <- plot +
          ggplot2::scale_fill_gradientn(
            colours = col_pal,
            values = col_rescale,
            limits = col_limits,
            expand = col_expand,
            breaks = col_breaks,
            labels = col_labels,
            transform = col_transform,
            oob = col_oob,
            na.value = col_pal_na,
          ) +
          ggplot2::scale_colour_gradientn(
            colours = col_pal,
            values = col_rescale,
            limits = col_limits,
            expand = col_expand,
            breaks = col_breaks,
            labels = col_labels,
            transform = col_transform,
            oob = col_oob,
            na.value = col_pal_na,
          ) +
          ggplot2::guides(
            colour = ggplot2::guide_colourbar(
              reverse = col_legend_rev
              # alpha = alpha_pal1
            ),
            fill = ggplot2::guide_colourbar(
              reverse = col_legend_rev
              # alpha = alpha_pal1
            )
          )

      }
      else if (col_steps) {
        plot <- plot +
          ggplot2::scale_fill_stepsn(
            colours = col_pal,
            values = col_rescale,
            limits = col_limits,
            expand = col_expand,
            breaks = col_breaks,
            labels = col_labels,
            transform = col_transform,
            oob = col_oob,
            na.value = col_pal_na,
          ) +
          ggplot2::scale_colour_stepsn(
            colours = col_pal,
            values = col_rescale,
            limits = col_limits,
            expand = col_expand,
            breaks = col_breaks,
            labels = col_labels,
            transform = col_transform,
            oob = col_oob,
            na.value = col_pal_na,
          ) +
          ggplot2::guides(
            colour = ggplot2::guide_coloursteps(
              reverse = col_legend_rev
              # alpha = alpha_pal1
            ),
            fill = ggplot2::guide_coloursteps(
              reverse = col_legend_rev
              # alpha = alpha_pal1
            )
          )
      }
    }
    else if (!col_continuous) {
      if (rlang::is_null(col_pal)) {

      }
      if (!rlang::quo_is_null(col)) {
        col_n <- data %>%
          dplyr::pull(!!col) %>%
          levels() %>%
          length()

        col_pal <- pal_discrete(col_n)
      }
      else { #guess anything that's ordered represents col,
        #as there is a discrete col scale and no col variable supplied
        plot_data_ordered <- plot_data %>%
          dplyr::summarise(dplyr::across(tidyselect::where(is.ordered), \(x) length(levels(x))))

        if (ncol(plot_data_ordered) == 0) {
          col_pal <- pal_discrete(n = 4)
        }
        else {
          col_n <- plot_data_ordered %>%
            tidyr::pivot_longer(tidyselect::everything()) %>%
            dplyr::summarise(max(.data$value)) %>%
            dplyr::pull()

          col_pal <- pal_continuous(n = col_n)
        }
      }

      if (flipped) {
        col_legend_rev <- !col_legend_rev
        col_pal <- rev(col_pal)
      }

      if (rlang::is_null(col_labels)) col_labels <- ggplot2::waiver()

      if (rlang::is_null(col_breaks)) col_breaks <- ggplot2::waiver()

      plot <- plot +
        ggplot2::scale_fill_manual(
          values = col_pal,
          limits = col_limits,
          expand = col_expand,
          breaks = col_breaks,
          labels = col_labels,
          na.value = col_pal_na,
          drop = FALSE, #consider add argument
        ) +
        ggplot2::scale_colour_manual(
          values = col_pal,
          limits = col_limits,
          expand = col_expand,
          breaks = col_breaks,
          labels = col_labels,
          na.value = col_pal_na,
          drop = FALSE, #consider add argument
        ) +
        ggplot2::guides(
          colour = ggplot2::guide_legend(
            reverse = col_legend_rev,
            ncol = col_legend_ncol,
            nrow = col_legend_nrow
          ),
          fill = ggplot2::guide_legend(
            reverse = col_legend_rev,
            ncol = col_legend_ncol,
            nrow = col_legend_nrow
          )
        )
    }

    #expand limits if necessary
    plot <- plot +
      ggplot2::expand_limits(
        colour = col_expand_limits,
        fill = col_expand_limits
      )
  }

  ##############################################################################
  # Make alpha scale where there is a alpha scale identified
  ##############################################################################

  if (!is.na(alpha_continuous)) {
    if (alpha_continuous) {
      if (rlang::is_null(alpha_pal)) {
        alpha_pal <- c(0.1, 1)
      }

      #get alpha_transform if NULL
      if (rlang::is_null(alpha_transform)) {
        if (inherits(rlang::eval_tidy(alpha, data), what = "hms")) alpha_transform <- scales::transform_hms()
        else if (inherits(rlang::eval_tidy(alpha, data), what = "POSIXct")) alpha_transform <- scales::transform_time()
        else if (inherits(rlang::eval_tidy(alpha, data), what = "Date")) alpha_transform <- scales::transform_date()
        else alpha_transform <- scales::transform_identity()
      }

      #make a tidy name to deal with composed transforms
      if (is.character(alpha_transform)) alpha_transform_name <- alpha_transform
      else if (inherits(alpha_transform, what = "transform")) {
        alpha_transform_name <- alpha_transform$name %>%
          stringr::str_remove("composition") %>%
          stringr::str_remove("\\(") %>%
          stringr::str_remove("\\)") %>%
          stringr::str_split(",") %>%
          unlist()
      }

      if (rlang::is_null(alpha_breaks)) {
        if (!any(alpha_transform_name %in% c("identity", "reverse"))) alpha_breaks <- ggplot2::waiver()
        else alpha_breaks <- scales::breaks_pretty(n = 5)
      }

      if (rlang::is_null(alpha_labels)) {
        if (any(alpha_transform_name %in% c("hms"))) alpha_labels <- scales::label_time()
        else if (any(alpha_transform_name %in% c("date", "time"))) alpha_labels <- scales::label_date_short()
        else if (!any(alpha_transform_name %in% c("identity", "reverse"))) alpha_labels <- ggplot2::waiver()
        else alpha_labels <- scales::label_comma(drop0trailing = TRUE)
      }

      plot <- plot +
        ggplot2::scale_alpha_continuous(
          range = alpha_pal,
          limits = alpha_limits,
          expand = alpha_expand,
          breaks = alpha_breaks,
          labels = alpha_labels,
          transform = alpha_transform,
          oob = alpha_oob,
          na.value = alpha_pal_na,
        ) +
        ggplot2::guides(
          alpha = ggplot2::guide_legend(
            reverse = TRUE
          )
        )
    }
    else if (!alpha_continuous) {
      if (rlang::is_null(alpha_pal)) {
        if (!rlang::quo_is_null(alpha)) {
          alpha_n <- data %>%
            dplyr::pull(!!alpha) %>%
            levels() %>%
            length()

          alpha_pal <- seq(from = 0.1, to = 1, by = (1 - 0.1) / (alpha_n - 1))
        }
        else { #guess anything that's ordered represents alpha,
          #as there is a discrete alpha scale and no alpha variable supplied
          plot_data_ordered <- plot_data %>%
            dplyr::summarise(dplyr::across(tidyselect::where(is.ordered), \(x) length(levels(x))))

          if (ncol(plot_data_ordered) == 0) {
            alpha_pal <- rep(1, times = 10)
          }
          else {
            alpha_n <- plot_data_ordered %>%
              tidyr::pivot_longer(tidyselect::everything()) %>%
              dplyr::summarise(max(.data$value)) %>%
              dplyr::pull()

            alpha_pal <- seq(from = 0.1, to = 1, by = (1 - 0.1) / (alpha_n - 1))
          }
        }
      }

      if ((identical(rlang::eval_tidy(col, data), rlang::eval_tidy(alpha, data))) &
          flipped) {
        alpha_legend_rev <- !(alpha_legend_rev)
        alpha_pal <- rev(alpha_pal)
      }

      if (rlang::is_null(alpha_labels)) alpha_labels <- ggplot2::waiver()

      if (rlang::is_null(alpha_breaks)) alpha_breaks <- ggplot2::waiver()

      plot <- plot +
        ggplot2::scale_alpha_manual(
          values = alpha_pal,
          limits = alpha_limits,
          expand = alpha_expand,
          breaks = alpha_breaks,
          labels = alpha_labels,
          na.value = alpha_pal_na,
          drop = FALSE, #consider add argument
        ) +
        ggplot2::guides(
          alpha = ggplot2::guide_legend(
            reverse = alpha_legend_rev,
            ncol = alpha_legend_ncol,
            nrow = alpha_legend_nrow
          )
        )
    }

    #expand limits if necessary
    plot <- plot +
      ggplot2::expand_limits(
        alpha = alpha_expand_limits
      )
  }

  ##############################################################################
  # Positional scales
  ##############################################################################

  #Make x scale based on plot_data
  if (stringr::str_detect(stat_name, "sf")) {
    if (rlang::is_null(x_expand)) x_expand <- ggplot2::waiver()
    if (rlang::is_null(x_labels)) x_labels <- ggplot2::waiver()
    if (rlang::is_null(x_breaks)) x_breaks <- ggplot2::waiver()
    if (rlang::is_null(x_transform)) x_transform <- scales::transform_identity()

    suppressMessages({
      plot <- plot +
        ggplot2::scale_x_continuous(
          limits = x_limits,
          expand = x_expand,
          breaks = x_breaks,
          labels = x_labels,
          oob = x_oob,
          sec.axis = x_sec_axis,
          transform = x_transform
        )
    })
  }
  else if (!x_continuous) {
    if (rlang::is_null(x_expand)) x_expand <- ggplot2::waiver()
    if (rlang::is_null(x_labels)) x_labels <- ggplot2::waiver()
    if (rlang::is_null(x_breaks)) x_breaks <- ggplot2::waiver()

    suppressMessages({
      plot <- plot +
        ggplot2::scale_x_discrete(
          limits = x_limits,
          expand = x_expand,
          breaks = x_breaks,
          labels = x_labels,
          drop = x_drop
        )
    })
  }
  else {
    #get x_labels if NULL
    if (rlang::is_null(x_labels)) {
      if (any(x_transform_name %in% c("hms"))) x_labels <- scales::label_time()
      else if (any(x_transform_name %in% c("date", "time"))) x_labels <- scales::label_date_short()
      else if (!any(x_transform_name %in% c("identity", "reverse"))) x_labels <- ggplot2::waiver()
      else x_labels <- scales::label_comma(drop0trailing = TRUE)
    }
    #get x_breaks_n if x_breaks is NULL
    if (rlang::is_null(x_breaks)) {
      if (rlang::quo_is_null(facet) & rlang::quo_is_null(facet2)) x_breaks_n <- 7
      else if (facet_layout == "wrap") {
        if (!rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) {
          if (facet_n == 1) x_breaks_n <- 7
          else if (facet_n == 2) x_breaks_n <- 4
          else if (facet_n == 3) x_breaks_n <- 3
          else x_breaks_n <- 2
        }
        else if (!rlang::quo_is_null(facet)) {
          if (facet_n <= 1) x_breaks_n <- 7
          else if (facet_n == 2) x_breaks_n <- 4
          else if (facet_n == 3) x_breaks_n <- 3
          else x_breaks_n <- 2
        }
        else if (!rlang::quo_is_null(facet2)) {
          if (facet2_n <= 1) x_breaks_n <- 7
          else if (facet2_n == 2) x_breaks_n <- 4
          else if (facet_n == 3) x_breaks_n <- 3
          else x_breaks_n <- 2
        }
      }
      else if (facet_layout == "grid") {
        if (rlang::quo_is_null(facet)) x_breaks_n <- 7
        else if (!rlang::quo_is_null(facet)) {
          if (facet_n == 1) x_breaks_n <- 7
          else if (facet_n == 2) x_breaks_n <- 4
          else if (facet_n == 3) x_breaks_n <- 3
          else x_breaks_n <- 2
        }
      }

      if (flipped) x_breaks_n <- x_breaks_n - 1
    }
    #get x_expand and x_breaks for simple scales situation
    if (!flipped |
        facet_scales %in% c("free", "free_x") |
        !any(x_transform_name %in% c("identity", "reverse", "date", "time", "hms")) |
        stringr::str_detect(stat_name, "sf")) {

      if (rlang::is_null(x_expand)) x_expand <- ggplot2::waiver()

      if (rlang::is_null(x_breaks)) {
        x_breaks <- scales::breaks_pretty(n = x_breaks_n)
      }
    }
    #get x_limits, x_expand and x_breaks for complex situation
    else {
      if (!rlang::is_null(x_limits)) {
        if (rlang::is_null(x_breaks)) x_breaks <- scales::breaks_pretty(n = x_breaks_n)
      }
      #get a vector to use
      else if (rlang::is_null(x_limits)) {
        x_vars_str <- "^x$|^xmin$|^xmax$|^xend$|^xmin_final$|^xmax_final$"

        x_vctr_temp <- plot_data %>%
          dplyr::filter(dplyr::if_any(tidyselect::matches(stringr::regex(x_vars_str)), \(x) !is.na(x))) %>%
          dplyr::select(tidyselect::matches(stringr::regex(x_vars_str)))

        if (ncol(x_vctr_temp) != 0) {
          if (!flipped & stringr::str_detect(stat_name, "bin")) {
            x_vctr <- x_vctr_temp %>%
              dplyr::select(tidyselect::matches(stringr::regex("^x$"))) %>%
              tidyr::pivot_longer(cols = tidyselect::everything()) %>%
              dplyr::pull(.data$value)
          }
          else {
            x_vctr <- x_vctr_temp %>%
              tidyr::pivot_longer(cols = tidyselect::everything()) %>%
              dplyr::pull(.data$value)
          }

          if (!rlang::is_null(x_expand_limits)) {
            if (any(x_transform_name %in% "reverse")) {
              x_vctr <- c(x_vctr, x_expand_limits * -1)
            }
            else x_vctr <- c(x_vctr, x_expand_limits)
          }
        }

        if (x_hms) x_vctr <- hms::as_hms(x_vctr)
        else if (x_posixct) x_vctr <- lubridate::as_datetime(x_vctr, origin = "1970-01-01")
        else if (x_date) x_vctr <- lubridate::as_date(x_vctr, origin = "1970-01-01")

        #get the range from the vctr
        x_range <- range(x_vctr, na.rm = TRUE)
        if (x_hms) x_range <- hms::as_hms(x_range)

        if (any(x_transform_name %in% "reverse")) {
          x_range <- sort(x_range, decreasing = TRUE)
        }

        if (position_name == "fill") {
          x_limits <- c(NA, NA)
          x_breaks <- seq(0, 1, 0.25)
        }
        else if (rlang::is_null(x_breaks)) {
          if (rlang::is_null(x_expand)) {
            x_breaks <- scales::breaks_pretty(n = x_breaks_n)(x_range)
            x_limits <- sort(range(x_breaks))
          }
          else x_breaks <- scales::breaks_pretty(n = x_breaks_n)
        }
        else {
          if (rlang::is_null(x_expand)) {
            if (is.function(x_breaks)) x_breaks <- x_breaks(x_range)
            else if (is.vector(x_breaks)) x_limits <- sort(range(x_breaks))
          }
          else x_breaks <- x_breaks
        }

        if (any(x_transform_name %in% "reverse")) {
          if (!rlang::is_null(x_limits)) {
            x_limits <- sort(x_limits, decreasing = TRUE)
          }
        }
      }
      if (rlang::is_null(x_expand)) x_expand <- c(0, 0)
    }

    #make the x_scale
    suppressMessages({
      plot <- plot +
        ggplot2::scale_x_continuous(
          limits = x_limits,
          expand = x_expand,
          breaks = x_breaks,
          labels = x_labels,
          oob = x_oob,
          sec.axis = x_sec_axis,
          transform = x_transform
        )
    })
  }

  #Make y scale based on plot_data
  if (stringr::str_detect(stat_name, "sf")) {
    if (rlang::is_null(y_expand)) y_expand <- ggplot2::waiver()
    if (rlang::is_null(y_labels)) y_labels <- ggplot2::waiver()
    if (rlang::is_null(y_breaks)) y_breaks <- ggplot2::waiver()
    if (rlang::is_null(y_transform)) y_transform <- scales::transform_identity()

    suppressMessages({
      plot <- plot +
        ggplot2::scale_y_continuous(
          limits = y_limits,
          expand = y_expand,
          labels = y_labels,
          breaks = y_breaks,
          oob = y_oob,
          sec.axis = y_sec_axis,
          transform = y_transform
        )
    })
  }
  else if (!y_continuous) {
    if (rlang::is_null(y_expand)) y_expand <- ggplot2::waiver()
    if (rlang::is_null(y_labels)) y_labels <- ggplot2::waiver()
    if (rlang::is_null(y_breaks)) y_breaks <- ggplot2::waiver()

    suppressMessages({
      plot <- plot +
        ggplot2::scale_y_discrete(
          limits = y_limits,
          expand = y_expand,
          breaks = y_breaks,
          labels = y_labels,
          drop = y_drop
        )
    })
  }
  else {
    #get y_labels if NULL
    if (rlang::is_null(y_labels)) {
      if (any(y_transform_name %in% c("hms"))) y_labels <- scales::label_time()
      else if (any(y_transform_name %in% c("date", "time"))) y_labels <- scales::label_date_short()
      else if (!any(y_transform_name %in% c("identity", "reverse"))) y_labels <- ggplot2::waiver()
      else y_labels <- scales::label_comma(drop0trailing = TRUE)
    }
    #get y_breaks_n if y_breaks is NULL
    if (rlang::is_null(y_breaks)) {
      if (rlang::quo_is_null(facet) & rlang::quo_is_null(facet2)) {
        y_breaks_n <- 7
      }
      else if (facet_layout == "wrap") {
        if (!rlang::quo_is_null(facet) & !rlang::quo_is_null(facet2)) {
          if (facet_n * facet2_n <= 3) y_breaks_n <- 6
          else if (facet_n * facet2_n <= 6) y_breaks_n <- 5
          else y_breaks_n <- 4
        }
        else if (!rlang::quo_is_null(facet)) {
          if (facet_n <= 3) y_breaks_n <- 6
          else if (facet_n == 4) y_breaks_n <- 5
          else y_breaks_n <- 4
        }
        else if (!rlang::quo_is_null(facet2)) {
          if (facet2_n <= 3) y_breaks_n <- 6
          else if (facet2_n == 4) y_breaks_n <- 5
          else y_breaks_n <- 4
        }
      }
      else if (facet_layout == "grid") {
        if (rlang::quo_is_null(facet2)) {
          y_breaks_n <- 6
        }
        else if (!rlang::quo_is_null(facet2)) {
          if (facet2_n <= 1) y_breaks_n <- 6
          else if (facet2_n == 2) y_breaks_n <- 5
          else y_breaks_n <- 4
        }
      }
    }

    #get y_expand and y_breaks for simple scales situation
    if (flipped |
        facet_scales %in% c("free", "free_x") |
        !any(y_transform_name %in% c("identity", "reverse", "date", "time", "hms")) |
        stringr::str_detect(stat_name, "sf")) {

      if (rlang::is_null(y_expand)) y_expand <- ggplot2::waiver()

      if (rlang::is_null(y_breaks)) {
        y_breaks <- scales::breaks_pretty(n = y_breaks_n)
      }
    }
    #get y_limits, y_expand and y_breaks for compley situation
    else {
      if (!rlang::is_null(y_limits)) {
        if (rlang::is_null(y_breaks)) y_breaks <- scales::breaks_pretty(n = y_breaks_n)
      }
      #get a vector to use
      else if (rlang::is_null(y_limits)) {
        y_vars_str <- "^y$|^ymin$|^ymax$|^yend$|^ymin_final$|^ymax_final$"

        y_vctr_temp <- plot_data %>%
          dplyr::filter(dplyr::if_any(tidyselect::matches(stringr::regex(y_vars_str)), \(y) !is.na(y))) %>%
          dplyr::select(tidyselect::matches(stringr::regex(y_vars_str)))

        if (!flipped & stringr::str_detect(stat_name, "bin")) {
          y_vctr <- y_vctr_temp %>%
            dplyr::select(tidyselect::matches(stringr::regex("^y$"))) %>%
            tidyr::pivot_longer(cols = tidyselect::everything()) %>%
            dplyr::pull(.data$value)
        }
        else {
          y_vctr <- y_vctr_temp %>%
            tidyr::pivot_longer(cols = tidyselect::everything()) %>%
            dplyr::pull(.data$value)
        }

        if (!rlang::is_null(y_expand_limits)) {
          if (any(y_transform_name %in% "reverse")) {
            y_vctr <- c(y_vctr, y_expand_limits * -1)
          }
          else y_vctr <- c(y_vctr, y_expand_limits)
        }

        if (y_hms) y_vctr <- hms::as_hms(y_vctr)
        else if (y_posixct) y_vctr <- lubridate::as_datetime(y_vctr, origin = "1970-01-01")
        else if (y_date) y_vctr <- lubridate::as_date(y_vctr, origin = "1970-01-01")

        #get the range from the vctr
        y_range <- range(y_vctr, na.rm = TRUE)
        if (y_hms) y_range <- hms::as_hms(y_range)

        if (any(y_transform_name %in% "reverse")) {
          y_range <- sort(y_range, decreasing = TRUE)
        }

        if (position_name == "fill") {
          y_limits <- c(NA, NA)
          y_breaks <- seq(0, 1, 0.25)
        }
        else if (rlang::is_null(y_breaks)) {
          if (rlang::is_null(y_expand)) {
            y_breaks <- scales::breaks_pretty(n = y_breaks_n)(y_range)
            y_limits <- sort(range(y_breaks))
          }
          else y_breaks <- scales::breaks_pretty(n = y_breaks_n)
        }
        else {
          if (rlang::is_null(y_expand)) {
            if (is.function(y_breaks)) y_breaks <- y_breaks(y_range)
            else if (is.vector(y_breaks)) y_limits <- sort(range(y_breaks))
          }
          else y_breaks <- y_breaks
        }

        if (any(y_transform_name %in% "reverse")) {
          if (!rlang::is_null(y_limits)) {
            y_limits <- sort(y_limits, decreasing = TRUE)
          }
        }
      }
      if (rlang::is_null(y_expand)) y_expand <- c(0, 0)
    }

    #make the y_scale
    suppressMessages({
      plot <- plot +
        ggplot2::scale_y_continuous(
          limits = y_limits,
          expand = y_expand,
          breaks = y_breaks,
          labels = y_labels,
          oob = y_oob,
          sec.axis = y_sec_axis,
          transform = y_transform
        )
    })
  }

  #expand limits if necessary
  if (!rlang::is_null(x_expand_limits)) {
    plot <- plot +
      ggplot2::expand_limits(x = x_expand_limits)
  }
  if (!rlang::is_null(y_expand_limits)) {
    plot <- plot +
      ggplot2::expand_limits(y = y_expand_limits)
  }

  #############################################################################
  # titles
  #############################################################################
  if (rlang::is_null(x_title)) {
    if (!rlang::is_null(plot_build$plot$labels$x)) {
      x_title <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$x[1]), titles)
    }
  }
  if (rlang::is_null(y_title)) {
    if (!rlang::is_null(plot_build$plot$labels$y)) {
      y_title <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$y[1]), titles)
    }
  }
  if (rlang::is_null(col_title)) {
    if (!rlang::is_null(plot_build$plot$labels$fill)) {
      col_title <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$fill[1]), titles)
    }
    else if (!rlang::is_null(plot_build$plot$labels$colour)) {
      col_title <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$colour[1]), titles)
    }
  }
  if (rlang::is_null(alpha_title)) {
    if (!rlang::is_null(plot_build$plot$labels$alpha)) {
      alpha_title <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$alpha[1]), titles)
    }
  }

  plot <- plot +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      x = x_title,
      y = y_title,
      alpha = alpha_title
    )

  if (!rlang::is_null(col_title)) {
    plot <- plot +
      ggplot2::labs(col = col_title, fill = col_title)
  }
  if (!rlang::is_null(alpha_title)) {
    plot <- plot +
      ggplot2::labs(alpha = alpha_title)
  }

  if (!rlang::is_null(x_title)) {
    if (x_title == "") {
      plot <- plot +
        ggplot2::labs(x = NULL)
    }
  }
  if (!rlang::is_null(y_title)) {
    if (y_title == "") {
      plot <- plot +
        ggplot2::labs(y = NULL)
    }
  }
  if (!rlang::is_null(col_title)) {
    if (col_title == "") {
      plot <- plot +
        ggplot2::labs(colour = NULL, fill = NULL)
    }
  }
  if (!rlang::is_null(alpha_title)) {
    if (alpha_title == "") {
      plot <- plot +
        ggplot2::labs(alpha = NULL)
    }
  }

  ##############################################################################
  # gridlines
  ##############################################################################

  if (rlang::is_null(x_gridlines)) {
    if (stringr::str_detect(stat_name, "sf")) x_gridlines <- FALSE
    else if (flipped) x_gridlines <- TRUE
    else x_gridlines <- FALSE
  }

  if (rlang::is_null(y_gridlines)) {
    if (stringr::str_detect(stat_name, "sf")) y_gridlines <- FALSE
    else if (flipped) y_gridlines <- FALSE
    else y_gridlines <- TRUE
  }

  if (!x_gridlines & !y_gridlines) {
    plot <- plot + #resolve ggplot2 issue #4730
      ggplot2::theme(panel.grid.major = ggplot2::element_blank()) +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  }
  else {
    if (!x_gridlines) {
      plot <- plot +
        ggplot2::theme(panel.grid.major.x = ggplot2::element_blank()) +
        ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank())
    }
    if (!y_gridlines) {
      plot <- plot +
        ggplot2::theme(panel.grid.major.y = ggplot2::element_blank()) +
        ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank())
    }
  }

  ##############################################################################
  # plot
  ##############################################################################

  return(plot)
}


