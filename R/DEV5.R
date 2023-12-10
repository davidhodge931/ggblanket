#' @title Blank ggplot
#'
#' @description Create a blank ggplot with a wrapper around ggplot2::geom_blank().
#' @param data A data frame or tibble.
#' @param geom The geometric object to use to display the data as a ggproto Geom subclass.
#' @param x Unquoted x aesthetic variable.
#' @param y Unquoted y aesthetic variable.
#' @param col Unquoted col and fill aesthetic variable.
#' @param alpha Unquoted alpha aesthetic variable.
#' @param facet Unquoted facet aesthetic variable.
#' @param facet2 Unquoted second facet variable.
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
#' @param col_pal Colours to use. A character vector of hex codes (or names).
#' @param col_pal_na Colour to use for NA values. A character vector of a hex code (or name).
#' @param alpha_pal Opacity. A number between 0 and 1.
#' @param alpha_pal_na Opacity. A number between 0 and 1.
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
#' @param col_expand_limits For a continuous col variable, any values that the limits should encompass (e.g. 0).
#' @param col_labels A function that takes the breaks as inputs (e.g. scales::label_comma()), or a vector of labels.
#' @param col_legend_ncol The number of columns for the legend elements.
#' @param col_legend_nrow The number of rows for the legend elements.
#' @param col_legend_place The place for the legend. Either "bottom", "right", "top" or "left". Or just the first letter of each.
#' @param col_legend_rev Reverse the elements of the legend. Defaults to FALSE.
#' @param col_limits A vector to determine the limits of the colour scale.
#' @param col_oob For a continuous col variable, a scales::oob_* function of how to handle values outside of limits (e.g. scales::oob_keep). Defaults to scales::oob_keep.
#' @param col_rescale For a continuous col variable, a scales::rescale function.
#' @param col_steps For a continuous col variable, whether to colour in steps. Defaults to FALSE (i.e. a gradient).
#' @param col_title Legend title string. Use "" for no title.
#' @param col_transform For a numeric col variable, a transformation object (e.g. "log10", "sqrt" or "reverse").
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
#' @param ... Other arguments passed to within a params list in the layer function.
#'
#' @return A ggplot object.
#' @export
#' @examples
#'
#'
gg_blanket <- function(
    data = NULL,
    geom = ggplot2::GeomBlank,
    x = NULL,
    y = NULL,
    col = NULL,
    facet = NULL,
    facet2 = NULL,
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
    alpha = NULL,
    mapping = NULL,
    col_pal = NULL,
    col_pal_na = "#bebebe",
    alpha_pal = NULL,
    alpha_pal_na = NULL,
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
    col_legend_place = "right",
    col_legend_ncol = NULL,
    col_legend_nrow = NULL,
    col_legend_rev = FALSE,
    col_limits = NULL,
    col_oob = scales::oob_keep,
    col_rescale = scales::rescale(),
    col_steps = FALSE,
    col_title = NULL,
    col_transform = NULL,
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
    stat = ggplot2::StatIdentity,
    position = ggplot2::position_identity(),
    coord = NULL,
    theme = NULL,
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

  # if (!rlang::is_null(mapping)) {
  #   if (any(names(unlist(mapping)) %in% c("colour", "fill", "alpha_pal"))) {
  #     rlang::abort("mapping argument does not support colour, fill or alpha_pal aesthetics")
  #   }
  #   if (any(names(unlist(mapping)) %in% c("facet", "facet2"))) {
  #     rlang::abort("mapping argument does not support facet or facet2")
  #   }
  # }

  ##############################################################################
  #get default theme, if global theme not set
  ##############################################################################

  if (rlang::is_null(theme)) {
    if (identical(ggplot2::theme_get(), ggplot2::theme_grey())) {
      theme <- light_mode()
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
  #process the data
  ##############################################################################

  data <- data %>%
    #ungroup the data
    dplyr::ungroup() %>%
    #make infinite values NA
    dplyr::mutate(dplyr::across(
      c(!!x, !!xmin, !!xmax, !!xend,
        !!y, !!ymin, !!ymax, !!yend,
        !!col, !!facet, !!facet2,
        !!group, !!subgroup, !!label, !!sample),
      na_if_inf)) |>
    #convert logicals to factors
    dplyr::mutate(dplyr::across(c(!!x, !!xmin, !!xmax, !!xend,
                                  !!y, !!ymin, !!ymax, !!yend,
                                  !!col, !!facet, !!facet2) &
                                  tidyselect::where(is.logical), function(x)
                                    factor(x, levels = c(TRUE, FALSE)))) |>
    #convert characters to factors
    dplyr::mutate(dplyr::across(c(!!x, !!xmin, !!xmax, !!xend,
                                  !!y, !!ymin, !!ymax, !!yend,
                                  !!col, !!facet, !!facet2) &
                                  tidyselect::where(is.character), function(x)
                                    factor(x))) |>
    #reverse y*, so that reads top low-levels to bottom high-levels
    dplyr::mutate(dplyr::across(c(!!y, !!ymin, !!ymax, !!yend) &
                                  tidyselect::where(is.factor),
                                function(x) forcats::fct_rev(x)))

  #if flipped, order col correctly
  if (!identical(rlang::eval_tidy(y, data), rlang::eval_tidy(col, data))) {
    if (flipped) {
      data <- data |>
        dplyr::mutate(dplyr::across(!!col & tidyselect::where(is.factor),
                                    function(x) forcats::fct_rev(x)))
    }
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

  if (stringr::str_detect(class(stat)[1], "Sf")) {
    geometry <- sf::st_geometry(data)
    if (rlang::is_null(coord)) coord <- ggplot2::coord_sf(clip = "off")
  }
  else {
    geometry <- NULL
    if (rlang::is_null(coord)) ggplot2::coord_cartesian(clip = "off")
  }

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
            geometry = geometry,
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
            geometry = geometry,
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
            geometry = geometry,
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
            geometry = geometry,
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
            geometry = geometry,
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
            geometry = geometry,
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
            geometry = geometry,
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
            geometry = geometry,
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
            geometry = geometry,
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
            geometry = geometry,
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
            geometry = geometry,
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
            geometry = geometry,
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
            geometry = geometry,
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
            geometry = geometry,
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
            geometry = geometry,
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
            geometry = geometry,
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
                            labeller = ggplot2::as_labeller(facet_labels)
        )
    }
  }

  ##############################################################################
  # Add positional scales pre getting plot data
  ##############################################################################

  if (!stringr::str_detect(class(stat)[1], "Sf")) {

    #limits reversed to deal with when trans is reverse
    if (x_numeric) {
      if (any(x_transform$name %in% "reverse") & !rlang::is_null(x_limits)) {
        plot <- plot +
          ggplot2::scale_x_continuous(limits = x_limits[c(2, 1)], oob = x_oob)
      }
      else {
        plot <- plot +
          ggplot2::scale_x_continuous(limits = x_limits, oob = x_oob)
      }
    }
    else if (x_date) {
      if (any(x_transform$name %in% "reverse") & !rlang::is_null(x_limits)) {
        plot <- plot +
          ggplot2::scale_x_date(limits = x_limits[c(2, 1)], oob = x_oob)
      }
      else {
        plot <- plot +
          ggplot2::scale_x_date(limits = x_limits, oob = x_oob)
      }
    }
    else if (x_posixct) {
      if (any(x_transform$name %in% "reverse") & !rlang::is_null(x_limits)) {
        plot <- plot +
          ggplot2::scale_x_datetime(limits = x_limits[c(2, 1)], oob = x_oob)
      }
      else {
        plot <- plot +
          ggplot2::scale_x_datetime(limits = x_limits, oob = x_oob)
      }
    }
    else if (x_hms) {
      if (any(x_transform$name %in% "reverse") & !rlang::is_null(x_limits)) {
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
      if (any(y_transform$name %in% "reverse") & !rlang::is_null(y_limits)) {
        plot <- plot +
          ggplot2::scale_y_continuous(limits = y_limits[c(2, 1)], oob = y_oob)
      }
      else {
        plot <- plot +
          ggplot2::scale_y_continuous(limits = y_limits, oob = y_oob)
      }
    }
    else if (y_date) {
      if (any(y_transform$name %in% "reverse") & !rlang::is_null(y_limits)) {
        plot <- plot +
          ggplot2::scale_y_date(limits = y_limits[c(2, 1)], oob = y_oob)
      }
      else {
        plot <- plot +
          ggplot2::scale_y_date(limits = y_limits, oob = y_oob)
      }
    }
    else if (y_posixct) {
      if (any(y_transform$name %in% "reverse") & !rlang::is_null(y_limits)) {
        plot <- plot +
          ggplot2::scale_y_datetime(limits = y_limits[c(2, 1)], oob = y_oob)
      }
      else {
        plot <- plot +
          ggplot2::scale_y_datetime(limits = y_limits, oob = y_oob)
      }
    }
    else if (y_hms) {
      if (any(y_transform$name %in% "reverse") & !rlang::is_null(y_limits)) {
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

  if (rlang::quo_is_null(col)) {
    if (rlang::is_null(col_pal)) col_pal1 <- "#357BA2"
    else col_pal1 <- col_pal[1]

    plot2 <- plot +
      ggplot2::layer(
        geom = geom,
        stat = stat,
        position = position,
        params = list(colour = col_pal1, fill = col_pal1, ...)
        # params = list(colour = col_pal1, fill = col_pal1, alpha = alpha_pal, ...)
      ) +
      coord +
      theme
  }
  else {
    plot2 <- plot +
      ggplot2::layer(
        geom = geom,
        stat = stat,
        position = position,
        params = list(...)
        # params = list(alpha = alpha_pal, ...)
      ) +
      coord +
      theme
  }

  ##############################################################################
  # Get plot build and data
  ##############################################################################

  #Get plot data and flipped status
  plot_build <- ggplot2::ggplot_build(plot2)
  plot_data <- plot_build$data[[1]]

  ##############################################################################
  # Add col scale
  ##############################################################################

  #sf documents scales differently, and this fixes
  if (stringr::str_detect(class(stat)[1], "Sf")) {
    if (class(rlang::eval_tidy(col, data)) %in%
        c("NULL", "numeric", "double", "integer","Date", "POSIXct","hms")) {
      col_continuous <- TRUE
    }
    if (class(rlang::eval_tidy(col, data)) %in%
        c("character", "logical", "factor")) {
      col_continuous <- FALSE
    }
    else col_continuous <- NA

    plot <- plot2
  }
  #correct for plots where col is null, but there should be a colour scale
  else {
    col_scale <- purrr::map_chr(plot_build$plot$scales$scales, \(x) rlang::call_name((x[["call"]])))

    if (any(
      col_scale %in%
      c("scale_colour_binned",
        "scale_colour_brewer",
        "scale_colour_continuous",
        "scale_colour_date",
        "scale_colour_datetime",
        "scale_colour_distiller",
        "scale_colour_fermenter",
        "scale_colour_gradient",
        "scale_colour_gradient2",
        "scale_colour_gradientn",
        "scale_colour_grey",
        "scale_colour_steps",
        "scale_colour_steps2",
        "scale_colour_stepsn",
        "scale_colour_viridis_b",
        "scale_colour_viridis_c",

        "scale_fill_binned",
        "scale_fill_brewer",
        "scale_fill_continuous",
        "scale_fill_date",
        "scale_fill_datetime",
        "scale_fill_distiller",
        "scale_fill_fermenter",
        "scale_fill_gradient",
        "scale_fill_gradient2",
        "scale_fill_gradientn",
        "scale_fill_grey",
        "scale_fill_steps",
        "scale_fill_steps2",
        "scale_fill_stepsn",
        "scale_fill_viridis_b",
        "scale_fill_viridis_c"
      )

    )) {
      col_continuous <- TRUE
    }
    else if (any(
      col_scale %in%
      c(
        "scale_colour_discrete",
        "scale_colour_manual",
        "scale_colour_ordinal",
        "scale_colour_viridis_d",

        "scale_fill_discrete",
        "scale_fill_manual",
        "scale_fill_ordinal",
        "scale_fill_viridis_d"
      )
    )) {
      col_continuous <- FALSE
    }
    else col_continuous <- NA

    if (rlang::quo_is_null(col) & !is.na(col_continuous)) {
      plot <- plot +
        ggplot2::layer(
          geom = geom,
          stat = stat,
          position = position,
          # params = list(alpha = alpha_pal, ...)
          params = list(...)
        ) +
        coord +
        theme
    }
    else {
      plot <- plot2
    }
  }

  #make col scale
  if (!is.na(col_continuous)) {
    if (col_continuous) {
      if (rlang::is_null(col_pal)) {
        col_pal <- viridisLite::mako(18, direction = -1)
      }

      if (rlang::is_null(col_transform)) {
        if (inherits(rlang::eval_tidy(col, data), what = "hms")) col_transform <- scales::transform_hms()
        else if (inherits(rlang::eval_tidy(col, data), what = "POSIXct")) col_transform <- scales::transform_time()
        else if (inherits(rlang::eval_tidy(col, data), what = "Date")) col_transform <- scales::transform_date()
        else col_transform <- scales::transform_identity()
      }

      #make a tidy name to deal with composed transforms
      col_transform_name <- col_transform$name |>
        stringr::str_remove("composition") |>
        stringr::str_remove("\\(") |>
        stringr::str_remove("\\)") |>
        stringr::str_split(",") |>
        unlist()

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
            trans = col_transform,
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
            trans = col_transform,
            oob = col_oob,
            na.value = col_pal_na,
          ) +
          ggplot2::guides(
            colour = ggplot2::guide_colourbar(
              title.position = "top", #keep for now
              ticks.colour = "#fcfdfe", #keep for now
              reverse = col_legend_rev,
              order = 1
            ),
            fill = ggplot2::guide_colourbar(
              title.position = "top", #keep for now
              ticks.colour = "#fcfdfe", #keep for now
              reverse = col_legend_rev,
              order = 1
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
            trans = col_transform,
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
            trans = col_transform,
            oob = col_oob,
            na.value = col_pal_na,
          ) +
          ggplot2::guides(
            colour = ggplot2::guide_coloursteps(
              title.position = "top",
              reverse = col_legend_rev,
              order = 1
            ),
            fill = ggplot2::guide_coloursteps(
              title.position = "top",
              reverse = col_legend_rev,
              order = 1
            )
          )
      }
    }
    else if (!col_continuous) {
      if (!rlang::quo_is_null(col)) {
        col_n <- data %>%
          dplyr::pull(!!col) %>%
          levels() %>%
          length()
        if (rlang::is_null(col_pal)) col_pal <- guardian()
        col_pal <- col_pal[1:col_n]
      }
      else {
        if (!rlang::is_null(plot_build$plot$labels$fill)) {
          col_n <- length(levels(dplyr::pull(plot_data, rlang::as_name(plot_build$plot$labels$fill[1]))))
        }
        else if (!rlang::is_null(plot_build$plot$labels$colour)) {
          col_n <- length(levels(dplyr::pull(plot_data, rlang::as_name(plot_build$plot$labels$colour[1]))))
        }

        if (rlang::is_null(col_pal)) col_pal <- viridisLite::mako(col_n, direction = -1)
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
            title.position = "top", #keep for now
            ncol = col_legend_ncol,
            nrow = col_legend_nrow,
            byrow = TRUE, #keep for now
            key.spacing = grid::unit(11 * 0.33, "pt"), #keep for now
            order = 1
          ),
          fill = ggplot2::guide_legend(
            reverse = col_legend_rev,
            title.position = "top", #keep for now
            ncol = col_legend_ncol,
            nrow = col_legend_nrow,
            byrow = TRUE, #keep for now
            key.spacing = grid::unit(11 * 0.33, "pt"), #keep for now
            order = 1
          )
        )
    }
  }

  #expand limits if necessary
  plot <- plot +
    ggplot2::expand_limits(
      colour = col_expand_limits,
      fill = col_expand_limits
    )

  ##############################################################################
  # Positional scales
  ##############################################################################

  #Make x scale based on plot_data
  if (!stringr::str_detect(class(stat)[1], "Sf") & !x_continuous) {
      if (rlang::is_null(x_expand)) x_expand <- ggplot2::waiver()
      if (rlang::is_null(x_labels)) x_labels <- ggplot2::waiver()
      if (rlang::is_null(x_breaks)) x_breaks <- ggplot2::waiver()

      suppressMessages({
        plot <- plot +
          ggplot2::scale_x_discrete(
            expand = x_expand,
            labels = x_labels,
            breaks = x_breaks,
            limits = x_limits,
            drop = x_drop
          )
      })
    }
  else {
    #get x_transform if NULL
    if (rlang::is_null(x_transform)) {
      if (x_hms) x_transform <- scales::transform_hms()
      else if (x_posixct) x_transform <- scales::transform_time()
      else if (x_date) x_transform <- scales::transform_date()
      else x_transform <- scales::transform_identity()
    }

    #make a tidy name to deal with composed transforms
    x_transform_name <- x_transform$name |>
      stringr::str_remove("composition") |>
      stringr::str_remove("\\(") |>
      stringr::str_remove("\\)") |>
      stringr::str_split(",") |>
      unlist()

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
        stringr::str_detect(class(stat)[1], "Sf")) {

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
          if (!flipped & stringr::str_detect(class(stat)[1], "Bin")) {
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

        if (class(position)[1] == "PositionFill") x_limits <- c(NA, NA)
        else if (class(position)[1] == "character") {
          if (position == "fill") x_limits <- c(NA, NA)
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
          trans = x_transform
        )
    })
  }

  #Make y scale based on plot_data
  if (!stringr::str_detect(class(stat)[1], "Sf") & !y_continuous) {
    if (rlang::is_null(y_expand)) y_expand <- ggplot2::waiver()
    if (rlang::is_null(y_labels)) y_labels <- ggplot2::waiver()
    if (rlang::is_null(y_breaks)) y_breaks <- ggplot2::waiver()

    suppressMessages({
      plot <- plot +
        ggplot2::scale_y_discrete(
          expand = y_expand,
          labels = y_labels,
          breaks = y_breaks,
          drop = y_drop
        )
    })
  }
  else {
    #get y_transform if NULL
    if (rlang::is_null(y_transform)) {
      if (y_hms) y_transform <- scales::transform_hms()
      else if (y_posixct) y_transform <- scales::transform_time()
      else if (y_date) y_transform <- scales::transform_date()
      else y_transform <- scales::transform_identity()
    }

    #make a tidy name to deal with composed transforms
    y_transform_name <- y_transform$name |>
      stringr::str_remove("composition") |>
      stringr::str_remove("\\(") |>
      stringr::str_remove("\\)") |>
      stringr::str_split(",") |>
      unlist()

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
        stringr::str_detect(class(stat)[1], "Sf")) {

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

        if (!flipped & stringr::str_detect(class(stat)[1], "Bin")) {
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

        if (class(position)[1] == "PositionFill") y_limits <- c(NA, NA)
        else if (class(position)[1] == "character") {
          if (position == "fill") y_limits <- c(NA, NA)
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
          trans = y_transform
        )
    })
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

  if (rlang::is_null(linetype_title)) {
    if (!rlang::is_null(plot_build$plot$labels$linetype)) {
      linetype_title <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$linetype[1]), titles)
    }
  }
  if (rlang::is_null(shape_title)) {
    if (!rlang::is_null(plot_build$plot$labels$shape)) {
      shape_title <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$shape[1]), titles)
    }
  }
  if (rlang::is_null(size_title)) {
    if (!rlang::is_null(plot_build$plot$labels$size)) {
      size_title <- purrr::map_chr(rlang::as_name(plot_build$plot$labels$size[1]), titles)
    }
  }

  plot <- plot +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      x = x_title,
      y = y_title,
      col = col_title,
      fill = col_title,
      linetype = linetype_title,
      shape = shape_title,
      size = size_title)

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

  #expand limits if necessary
  # if (!stringr::str_detect(class(stat)[1], "Sf")) {
    if (!rlang::is_null(x_expand_limits)) {
      plot <- plot +
        ggplot2::expand_limits(x = x_expand_limits)
    }
    if (!rlang::is_null(y_expand_limits)) {
      plot <- plot +
        ggplot2::expand_limits(y = y_expand_limits)
    }
  # }

  if (!rlang::is_null(col_expand_limits)) {
    plot <- plot +
      ggplot2::expand_limits(colour = col_expand_limits, fill = col_expand_limits)
  }

  ##############################################################################
  # gridlines
  ##############################################################################

  if (rlang::is_null(x_gridlines)) {
    if (stringr::str_detect(class(stat)[1], "Sf")) x_gridlines <- FALSE
    else if (flipped) x_gridlines <- TRUE
    else x_gridlines <- FALSE
  }

  if (rlang::is_null(y_gridlines)) {
    if (stringr::str_detect(class(stat)[1], "Sf")) y_gridlines <- FALSE
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


